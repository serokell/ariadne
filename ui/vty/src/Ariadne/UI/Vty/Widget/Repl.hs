module Ariadne.UI.Vty.Widget.Repl
       ( ReplWidgetState
       , initReplWidget
       , drawReplInputWidget
       , drawReplOutputWidget

       , ReplCompleted(..)
       , InputModification(..)
       , ReplInputEvent(..)
       , ReplOutputEvent(..)
       , keyToReplInputEvent
       , handleReplInputEvent
       , handleReplOutputEvent
       ) where

import Prelude (until)
import Universum

import Control.Lens (makeLensesWith, traversed, uses, zoom, (.=))
import Data.Char as Char
import Data.Function (fix, on)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text.Zipper
  (TextZipper, breakLine, clearZipper, currentChar, currentLine,
  cursorPosition, deleteChar, deletePrevChar, getText, gotoBOL, gotoEOL,
  insertChar, insertMany, killToBOL, lineLengths, moveDown, moveLeft,
  moveRight, moveUp, previousChar, textZipper, moveCursor)
import IiExtras
import Named

import qualified Data.Loc as Loc
import qualified Data.Loc.Span as Loc

import qualified Brick as B
import qualified Graphics.Vty as V
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.UI

type AdaptiveImage =
  Named V.Attr "def_attr" -> Named Int "width" -> V.Image

data OutputElement
  = OutputCommand UiCommandId AdaptiveImage [AdaptiveImage] (Maybe AdaptiveImage)
  | OutputInfo AdaptiveImage

-- Note that fields here are lazy, so we can afford to put all results we might
-- need and discard the existential from 'UiLangFace' without causing excessive
-- recomputation in 'mkReplParseResult'.
data ReplParseResult
  = ReplParseFailure { rpfParseErrDoc :: PP.Doc, rpfParseErrSpans :: [Loc.Span] }
  | ReplParseSuccess { rpfExprDoc :: PP.Doc, rpfPutCommand :: IO UiCommandId }

data ReplWidgetState =
  ReplWidgetState
    { replWidgetParseResult :: ReplParseResult
    , replWidgetTextZipper :: !(TextZipper Text)
    , replWidgetOut :: ![OutputElement]
    , replWidgetHistoryFace :: !UiHistoryFace
    }

makeLensesWith postfixLFields ''ReplWidgetState

widgetInputName, widgetOutputName :: BrickName
widgetInputName = BrickReplInput
widgetOutputName = BrickReplOutput

replWidgetPrompt :: String
replWidgetPrompt = "knit> "

replWidgetPromptCont :: String
replWidgetPromptCont = "  ... "

replWidgetText :: ReplWidgetState -> Text
replWidgetText = Text.unlines . getText . replWidgetTextZipper

mkReplParseResult :: UiLangFace -> Text -> ReplParseResult
mkReplParseResult UiLangFace{..} t =
  case langParse t of
    Left err ->
      ReplParseFailure
        { rpfParseErrDoc = langPpParseError err
        , rpfParseErrSpans = langParseErrSpans err
        }
    Right expr ->
      ReplParseSuccess
        { rpfExprDoc = langPpExpr expr
        , rpfPutCommand = langPutCommand expr
        }

replReparse :: Monad m => UiLangFace -> StateT ReplWidgetState m ()
replReparse langFace = do
  t <- gets replWidgetText
  replWidgetParseResultL .= mkReplParseResult langFace t

initReplWidget :: UiLangFace -> UiHistoryFace -> ReplWidgetState
initReplWidget langFace historyFace =
  fix $ \this -> ReplWidgetState
    { replWidgetParseResult = mkReplParseResult langFace (replWidgetText this)
    , replWidgetTextZipper = textZipper [] Nothing
    , replWidgetOut = [OutputInfo ariadneBanner]
    , replWidgetHistoryFace = historyFace
    }

drawReplOutputWidget :: Bool -> ReplWidgetState -> B.Widget BrickName
drawReplOutputWidget _hasFocus replWidgetState =
  B.viewport widgetOutputName B.Vertical $
    B.cached widgetOutputName $
    B.Widget
      { B.hSize = B.Fixed
      , B.vSize = B.Fixed
      , B.render = render
      }
  where
    outElems = reverse (replWidgetOut replWidgetState)
    render = do
      rdrCtx <- B.getContext
      let
        defAttr = rdrCtx ^. B.attrL
        width = rdrCtx ^. B.availWidthL
        img =
          V.vertCat $
          List.intersperse (V.backgroundFill 1 1) $
          fmap drawOutputElement outElems
        drawOutputElement (OutputInfo mkImg) =
          mkImg ! #def_attr defAttr ! #width width
        drawOutputElement (OutputCommand commandId commandSrc commandMsgs mCommandOut) =
          let
            cmdInfo = maybe "" (<> " ") (cmdTaskIdRendered commandId)
            prompt = V.text' defAttr "> "
          in
            V.vertCat
              [ V.horizCat
                [ prompt
                , commandSrc
                    ! #def_attr defAttr
                    ! #width (width - V.imageWidth prompt)
                ]
              , V.vertCat $ reverse commandMsgs <&> \mkImg ->
                  mkImg ! #def_attr defAttr ! #width width
              , case mCommandOut of
                  Nothing -> V.text' defAttr $ cmdInfo <> "Waiting for result..."
                  Just mkImg -> mkImg ! #def_attr defAttr ! #width width
              ]
      return $
        B.emptyResult
          & B.imageL .~ img

drawReplInputWidget :: Bool -> ReplWidgetState -> B.Widget BrickName
drawReplInputWidget hasFocus replWidgetState =
  fixedViewport widgetInputName B.Horizontal B.Widget
    { B.hSize = B.Fixed
    , B.vSize = B.Fixed
    , B.render = render
    }
  where
    render = do
      rdrCtx <- B.getContext
      let
        defAttr = rdrCtx ^. B.attrL
        replInputWidth = max 1 $ (rdrCtx ^. B.availWidthL) - length replWidgetPrompt

        attrFn :: (Int, Int) -> V.Attr -> V.Attr
        attrFn loc =
          case replWidgetParseResult replWidgetState of
            ReplParseFailure{..} | inSpans rpfParseErrSpans loc ->
              (`V.withBackColor` V.red)
            _ -> identity

        wrapLines :: Text -> [Text]
        wrapLines line = List.unfoldr f line
          where
            f line' =
              if Text.null line'
              then Nothing
              else Just $ Text.splitAt replInputWidth line'

        linesToImage :: Int -> [Text] -> V.Image
        linesToImage row textLines = V.vertCat
          [ V.horizCat
            [ V.char (attrFn (row, column) defAttr) char
            | (column, char) <- List.zip [1 + subRow * replInputWidth..] (toString line)
            ]
          | (subRow, line) <- List.zip [0..] textLines
          ]

        zipper = replWidgetTextZipper replWidgetState
        img =
          V.vertCat $
          List.zipWith V.horizJoin
            (V.string defAttr replWidgetPrompt :
              List.repeat (V.string defAttr replWidgetPromptCont))
            [ linesToImage row $ wrapLines line
            | (row, line) <- List.zip [1..] (getText zipper)
            ]
        curLoc =
          let
            (y, x) = cursorPosition zipper
            -- We need to take all lines above current, calculate how many screen lines they occupy
            -- and sum these values to get screen line for the current text line
            prevLines = sum $ map (\n -> 1 + n `div` replInputWidth) $ take y $ lineLengths zipper
            (subY, subX) = x `divMod` replInputWidth
          in B.CursorLocation (B.Location (subX + length replWidgetPrompt, prevLines + subY)) Nothing
      return $
        B.emptyResult
          & B.imageL .~ img
          & B.cursorsL .~ [curLoc | hasFocus]

inSpans :: [Loc.Span] -> (Int, Int) -> Bool
inSpans spans (row, column) = inSpan
  where
    inSpan = List.any inSpan1 spans
    inSpan1 = Loc.overlapping $
      Loc.fromTo
        (Loc.loc (fromIntegral row) (fromIntegral column))
        (Loc.loc (fromIntegral row) (fromIntegral column + 1))

data NavAction
  = NavLeft
  | NavLeftWord
  | NavHome
  | NavRight
  | NavRightWord
  | NavEnd
  | NavUp
  | NavDown

data CommandAction
  = NextCommand
  | PrevCommand

data InputModification
  = InsertChar Char
  | InsertMany Text
  | DeleteBackwards
  | DeleteWordBackwards
  | DeleteAllBackwards
  | DeleteForwards
  | DeleteWordForwards
  | DeleteAll
  | BreakLine
  | ReplaceBreakLine

data ReplInputEvent
  = ReplCommandEvent UiCommandId UiCommandEvent
  | ReplInputModifyEvent InputModification
  | ReplInputNavigationEvent NavAction
  | ReplCommandNavigationEvent CommandAction
  | ReplSendEvent
  | ReplSmartEnterEvent
  | ReplAutocompleteEvent
  | ReplQuitEvent
  | ReplMouseDownEvent B.Location

data ReplOutputEvent
  = ReplOutputScrollingEvent ScrollingAction

data ReplCompleted = ReplCompleted | ReplInProgress

keyToReplInputEvent
  :: ReplWidgetState
  -> KeyboardEditEvent
  -> Maybe ReplInputEvent
keyToReplInputEvent ReplWidgetState{..} = \case
  KeyEditLeft ->
    Just $ ReplInputNavigationEvent NavLeft
  KeyEditLeftWord ->
    Just $ ReplInputNavigationEvent NavLeftWord
  KeyEditHome ->
    Just $ ReplInputNavigationEvent NavHome
  KeyEditRight ->
    Just $ ReplInputNavigationEvent NavRight
  KeyEditRightWord ->
    Just $ ReplInputNavigationEvent NavRightWord
  KeyEditEnd ->
    Just $ ReplInputNavigationEvent NavEnd
  -- TODO: go to prev/next command, when we are on first/last line
  KeyEditUp
    | isMultiline ->
        Just $ ReplInputNavigationEvent NavUp
    | otherwise ->
        Just $ ReplCommandNavigationEvent PrevCommand
  KeyEditDown
    | isMultiline ->
        Just $ ReplInputNavigationEvent NavDown
    | otherwise ->
        Just $ ReplCommandNavigationEvent NextCommand
  KeyEditAutocomplete ->
    Just $ ReplInputAutocomplete
  KeyEditDelLeft ->
    Just $ ReplInputModifyEvent DeleteBackwards
  KeyEditDelLeftWord ->
    Just $ ReplInputModifyEvent DeleteWordBackwards
  KeyEditDelLeftAll ->
    Just $ ReplInputModifyEvent DeleteAllBackwards
  KeyEditDelRight ->
    Just $ ReplInputModifyEvent DeleteForwards
  KeyEditDelRightWord ->
    Just $ ReplInputModifyEvent DeleteWordForwards
  KeyEditSend ->
    Just ReplSmartEnterEvent
  KeyEditCancel
    | not isEmpty ->
        Just $ ReplInputModifyEvent DeleteAll
  KeyEditQuit ->
    Just ReplQuitEvent
  KeyEditNext ->
    Just $ ReplCommandNavigationEvent NextCommand
  KeyEditPrev ->
    Just $ ReplCommandNavigationEvent PrevCommand
  KeyEditChar c ->
    Just $ ReplInputModifyEvent (InsertChar c)
  _ -> Nothing
  where
    isMultiline = length (lineLengths replWidgetTextZipper) > 1
    isEmpty = Text.null $ Text.unwords $ getText replWidgetTextZipper

handleReplInputEvent
  :: UiLangFace
  -> ReplInputEvent
  -> StateT ReplWidgetState (B.EventM BrickName) ReplCompleted
handleReplInputEvent langFace = fix $ \go -> \case
  ReplQuitEvent -> return ReplCompleted
  ReplInputModifyEvent modification -> do
    zoom replWidgetTextZipperL $ modify $
      case modification of
        InsertChar c -> insertChar c
        InsertMany t -> insertMany t
        DeleteBackwards -> deletePrevChar
        DeleteWordBackwards -> byWord deletePrevChar previousChar
        DeleteAllBackwards -> killToBOL
        DeleteForwards -> deleteChar
        DeleteWordForwards -> byWord deleteChar currentChar
        DeleteAll -> clearZipper
        BreakLine -> smartBreakLine
        ReplaceBreakLine -> smartBreakLine . deletePrevChar
    replReparse langFace
    history <- gets replWidgetHistoryFace
    t <- gets replWidgetText
    liftIO $ historySetPrefix history t
    return ReplInProgress
  ReplInputNavigationEvent nav -> do
    zoom replWidgetTextZipperL $ modify $
      case nav of
        NavLeft -> moveLeft
        NavLeftWord -> byWord moveLeft previousChar
        NavRight -> moveRight
        NavRightWord -> byWord moveRight currentChar
        NavUp -> moveUp
        NavDown -> moveDown
        NavHome -> gotoBOL
        NavEnd -> gotoEOL
    return ReplInProgress
  ReplMouseDownEvent (B.Location (col, row)) -> do
    zoom replWidgetTextZipperL $ modify $
      safeMoveCursor (row, col - length replWidgetPrompt - 1)
    return ReplInProgress
  ReplCommandNavigationEvent cmdAction -> do
    -- TODO: handle multi-line commands
    historyFace <- gets replWidgetHistoryFace
    let action = case cmdAction of
                    NextCommand -> historyNextCommand
                    PrevCommand -> historyPrevCommand
    cmd <- liftIO $ action historyFace
    zoom replWidgetTextZipperL $ modify $ insertMany (fromMaybe "" cmd) . clearZipper
    replReparse langFace
    return ReplInProgress
  ReplSmartEnterEvent -> do
    quitCommandDetected <- gets (isQuitCommand . replWidgetText)
    if quitCommandDetected
      then go ReplQuitEvent
      else do
        mPrevChar <- uses replWidgetTextZipperL previousChar
        case mPrevChar of
          Just '\\' -> go (ReplInputModifyEvent ReplaceBreakLine)
          _ -> go ReplSendEvent
  ReplAutocompleteEvent -> do
    t <- gets replWidgetText
    completed = t -- TODO: transform this to complete knit commands
    zoom replWidgetTextZipperL $ modify $ insertMany (fromMaybe "" completed) . clearZipper
    replReparse langFace
    return ReplInProgress
  ReplSendEvent -> do
    history <- gets replWidgetHistoryFace
    t <- gets replWidgetText
    liftIO $ historyAddCommand history t
    replParseResult <- use replWidgetParseResultL
    case replParseResult of
      ReplParseFailure{..} -> do
        let out = OutputInfo $ \(Named defAttr) (Named w) -> pprDoc defAttr w rpfParseErrDoc
        zoom replWidgetOutL $ modify (out:)
      ReplParseSuccess{..} -> do
        commandId <- liftIO rpfPutCommand
        zoom replWidgetTextZipperL $ modify $ clearZipper
        let
          commandSrc (Named defAttr) (Named w) = pprDoc defAttr w rpfExprDoc
          out = OutputCommand commandId commandSrc [] Nothing
        zoom replWidgetOutL $ modify (out:)
        replReparse langFace
    lift $ B.invalidateCacheEntry widgetOutputName
    lift $ scrollToEnd widgetOutputName
    return ReplInProgress
  ReplCommandEvent commandId commandEvent -> do
    zoom (replWidgetOutL . traversed) $
      modify (updateCommandResult commandId commandEvent)
    lift $ B.invalidateCacheEntry widgetOutputName
    lift $ scrollToEnd widgetOutputName
    return ReplInProgress

handleReplOutputEvent
  :: ReplOutputEvent
  -> StateT ReplWidgetState (B.EventM BrickName) ()
handleReplOutputEvent = \case
  ReplOutputScrollingEvent action -> do
    lift $ handleScrollingEvent widgetOutputName action

isQuitCommand :: Text -> Bool
isQuitCommand t =
  Text.strip t `List.elem` ["quit", "q", ":quit", ":q", "exit"]

smartBreakLine :: TextZipper Text -> TextZipper Text
smartBreakLine tz =
  let indentation = Text.takeWhile Char.isSpace (currentLine tz)
  in insertMany indentation (breakLine tz)

byWord
  :: (TextZipper Text -> TextZipper Text)
  -> (TextZipper Text -> Maybe Char)
  -> TextZipper Text
  -> TextZipper Text
byWord move check = go Char.isSpace . go (not . Char.isSpace)
  where
    go p = until (nothingLeft p) move
    nothingLeft p tz = case check tz of
      Nothing -> True
      Just c -> p c

safeMoveCursor :: (Int, Int) -> TextZipper Text -> TextZipper Text
safeMoveCursor (row, col) tz = moveCursor (row', col') tz
  where
    clamp mn mx = max mn . min mx
    lengths = lineLengths tz
    row' = clamp 0 (length lengths - 1) row
    col' = clamp 0 (lengths List.!! row') col

updateCommandResult
  :: UiCommandId
  -> UiCommandEvent
  -> OutputElement
  -> OutputElement
updateCommandResult
  commandId
  commandEvent
  (OutputCommand commandId' commandSrc oldMessages oldResultImage) | eqCommandId commandId commandId'
  = OutputCommand commandId commandSrc messages mCommandResultImage
  where
    eqCommandId = (==) `on` cmdIdEqObject
    mCommandResultImage =
      case commandEvent of
        UiCommandSuccess doc ->
          Just $ \(Named defAttr) (Named w) -> pprDoc defAttr w doc
        UiCommandFailure doc ->
          Just $ \(Named defAttr) (Named w) ->
            V.vertJoin
              (V.text' (defAttr `V.withBackColor` V.red) "Error")
              (pprDoc defAttr w doc)
        UiCommandOutput _ -> oldResultImage
    messages =
      case commandEvent of
        UiCommandSuccess _ -> oldMessages
        UiCommandFailure _ -> oldMessages
        UiCommandOutput doc ->
          let message (Named defAttr) (Named w) = pprDoc defAttr w doc
          in message:oldMessages
updateCommandResult _ _ outCmd = outCmd

ariadneBanner :: AdaptiveImage
ariadneBanner (Named defAttr) _ = V.vertCat $ List.map (V.text' defAttr)
  [ "             ___         _           __         "
  , "            /   |  _____(_)___ _____/ /___  ___ "
  , "           / /| | / ___/ / __ `/ __  / __ \\/ _ \\"
  , "          / ___ |/ /  / / /_/ / /_/ / / / /  __/"
  , "         /_/  |_/_/  /_/\\__,_/\\__,_/_/ /_/\\___/ "
  , ""
  , "              Press <Enter> to send a command,"
  , "        <Backslash> <Enter> to insert a line break,"
  , "      <Ctrl+P>/<Ctrl+N> to go to previous/next command,"
  , "       <Tab> to switch between widgets, <Esc> for menu"
  , "     Hint: you can select text with mouse by holding Shift"
  ]
