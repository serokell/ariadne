module Ariadne.UI.Vty.Widget.Repl where

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
  moveRight, moveUp, previousChar, textZipper)
import IiExtras
import Named

import qualified Data.Loc as Loc
import qualified Data.Loc.Span as Loc

import qualified Brick as B
import qualified Graphics.Vty as V
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.CommandHistory
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling

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

data ReplWidgetState n =
  ReplWidgetState
    { replWidgetParseResult :: ReplParseResult
    , replWidgetTextZipper :: TextZipper Text
    , replWidgetOut :: [OutputElement]
    , replWidgetHistory :: CommandHistory
    , replWidgetBrickName :: n
    }

makeLensesWith postfixLFields ''ReplWidgetState

replWidgetText :: ReplWidgetState n -> Text
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

replReparse :: Monad m => UiLangFace -> StateT (ReplWidgetState n) m ()
replReparse langFace = do
  t <- gets replWidgetText
  replWidgetParseResultL .= mkReplParseResult langFace t

initReplWidget
  :: (Ord n, Show n)
  => UiLangFace
  -> CommandHistory
  -> n
  -> ReplWidgetState n
initReplWidget langFace history name =
  fix $ \this -> ReplWidgetState
    { replWidgetParseResult = mkReplParseResult langFace (replWidgetText this)
    , replWidgetTextZipper = textZipper [] Nothing
    , replWidgetOut = [OutputInfo ariadneBanner]
    , replWidgetHistory = history
    , replWidgetBrickName = name
    }

drawReplOutputWidget
  :: (Ord n, Show n)
  => Bool
  -> ReplWidgetState n
  -> B.Widget n
drawReplOutputWidget _hasFocus replWidgetState =
  B.viewport name B.Vertical $
    B.cached name $
    B.Widget
      { B.hSize = B.Fixed
      , B.vSize = B.Fixed
      , B.render = render
      }
  where
    name = replWidgetState ^. replWidgetBrickNameL
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

drawReplInputWidget
  :: Bool
  -> ReplWidgetState n
  -> B.Widget n
drawReplInputWidget hasFocus replWidgetState =
  B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Fixed
    , B.render = render
    }
  where
    render = do
      rdrCtx <- B.getContext
      let
        defAttr = rdrCtx ^. B.attrL
        attrFn :: (Int, Int) -> V.Attr -> V.Attr
        attrFn loc =
          case replWidgetParseResult replWidgetState of
            ReplParseFailure{..} | inSpans rpfParseErrSpans loc ->
              (`V.withBackColor` V.red)
            _ -> identity
        zipper = replWidgetTextZipper replWidgetState
        img =
          V.vertCat $
          List.zipWith V.horizJoin
            (V.string defAttr "knit> " :
              List.repeat (V.string defAttr "  ... "))
            [ V.horizCat
              [ V.char (attrFn (row, column) defAttr) char
              | (column, char) <- List.zip [1..] (toString line)
              ]
            | (row, line) <- List.zip [1..] (getText zipper)
            ]
        curLoc =
          let (y, x) = cursorPosition zipper
          in B.CursorLocation (B.Location (x + 6, y)) Nothing
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

data ReplOutputEvent
  = ReplOutputScrollingEvent ScrollingAction

data ReplCompleted = ReplCompleted | ReplInProgress

keyToReplInputEvent
  :: ReplWidgetState n
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
  :: (Ord n, Show n)
  => UiLangFace
  -> ReplInputEvent
  -> StateT (ReplWidgetState n) (B.EventM n) ReplCompleted
handleReplInputEvent langFace = fix $ \go -> \case
  ReplQuitEvent -> return ReplCompleted
  ReplInputModifyEvent modification -> do
    zoom replWidgetTextZipperL $ modify $
      case modification of
        InsertChar c -> insertChar c
        DeleteBackwards -> deletePrevChar
        DeleteWordBackwards -> byWord deletePrevChar previousChar
        DeleteAllBackwards -> killToBOL
        DeleteForwards -> deleteChar
        DeleteWordForwards -> byWord deleteChar currentChar
        DeleteAll -> clearZipper
        BreakLine -> smartBreakLine
        ReplaceBreakLine -> smartBreakLine . deletePrevChar
    replReparse langFace
    history <- gets replWidgetHistory
    t <- gets replWidgetText
    liftIO $ setCurrCommand history t
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
  ReplCommandNavigationEvent cmdAction -> do
    -- TODO: handle multi-line commands
    history <- gets replWidgetHistory
    let action = case cmdAction of
                    NextCommand -> toNextCommand
                    PrevCommand -> toPrevCommand
    cmd <- liftIO $ action history
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
    history <- gets replWidgetHistory
    t <- gets replWidgetText
    liftIO $ setCurrCommand history t
    liftIO $ startNewCommand history
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
    name <- use replWidgetBrickNameL
    lift $ B.invalidateCacheEntry name
    lift $ scrollToEnd name
    return ReplInProgress
  ReplCommandEvent commandId commandEvent -> do
    zoom (replWidgetOutL . traversed) $
      modify (updateCommandResult commandId commandEvent)
    name <- use replWidgetBrickNameL
    lift $ B.invalidateCacheEntry name
    lift $ scrollToEnd name
    return ReplInProgress

handleReplOutputEvent
  :: (Ord n, Show n)
  => ReplOutputEvent
  -> StateT (ReplWidgetState n) (B.EventM n) ()
handleReplOutputEvent = \case
  ReplOutputScrollingEvent action -> do
    name <- use replWidgetBrickNameL
    lift $ handleScrollingEvent name action

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
  ]
