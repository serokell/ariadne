module Ariadne.UI.Vty.Widget.Repl where

import Prelude hiding (unlines)
import Data.Text as Text
import Data.Char as Char
import Data.Function (fix)
import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Text.Zipper
import Data.List as List
import Data.Function (on)
import IiExtras

import qualified Data.Loc as Loc
import qualified Data.Loc.Span as Loc

import qualified Brick as B
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.Face

data OutputElement
  = OutputCommand UiCommandId (Int -> V.Image) (Maybe (Int -> V.Image))
  | OutputInfo (Int -> V.Image)

-- Note that fields here are lazy, so we can afford to put all results we might
-- need and discard the existential from 'UiLangFace' without causing excessive
-- recomputation in 'mkReplParseResult'.
data ReplParseResult
  = ReplParseFailure { rpfParseErrDoc :: PP.Doc, rpfParseErrSpans :: [Loc.Span] }
  | ReplParseSuccess { rpfExprDoc :: PP.Doc, rpfPutCommand :: IO UiCommandId }

data ReplWidgetState =
  ReplWidgetState
    { replWidgetParseResult :: ReplParseResult
    , replWidgetTextZipper :: TextZipper Text
    , replWidgetOut :: [OutputElement]
    }

makeLensesWith postfixLFields ''ReplWidgetState

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

initReplWidget :: UiLangFace -> ReplWidgetState
initReplWidget langFace =
  fix $ \this -> ReplWidgetState
    { replWidgetParseResult = mkReplParseResult langFace (replWidgetText this)
    , replWidgetTextZipper = textZipper [] Nothing
    , replWidgetOut = [OutputInfo ariadneBanner]
    }

drawReplOutputWidget
  :: Bool
  -> ReplWidgetState
  -> B.Widget name
drawReplOutputWidget _hasFocus replWidgetState =
  B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    outElems = Prelude.reverse (replWidgetOut replWidgetState)
    render = do
      rdrCtx <- B.getContext
      let
        height = (rdrCtx ^. B.availHeightL) - 1
        width = rdrCtx ^. B.availWidthL
        img =
          V.cropTop height $
          V.vertCat $
          List.intersperse (V.backgroundFill 1 1) $
          fmap drawOutputElement outElems
        drawOutputElement (OutputInfo mkImg) =
          mkImg width
        drawOutputElement (OutputCommand commandId commandSrc mCommandOut) =
          let
            cmdInfo = drawCommandId commandId
          in
            V.vertCat
              [ V.horizCat
                [ cmdInfo
                , V.text' V.defAttr " "
                , commandSrc (width - V.imageWidth cmdInfo - 1)
                ]
              , case mCommandOut of
                  Nothing -> V.text' V.defAttr "<waiting for output>"
                  Just mkImg -> mkImg width
              ]
      return $
        B.emptyResult
          & B.imageL .~ img

drawReplInputWidget
  :: Bool
  -> ReplWidgetState
  -> B.Widget name
drawReplInputWidget hasFocus replWidgetState =
  B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Fixed
    , B.render = render
    }
  where
    render = do
      let
        attrFn :: (Int, Int) -> V.Attr -> V.Attr
        attrFn loc =
          case replWidgetParseResult replWidgetState of
            ReplParseFailure{..} | inSpans rpfParseErrSpans loc ->
              (`V.withBackColor` V.red)
            _ -> id
        zipper = replWidgetTextZipper replWidgetState
        img =
          V.vertCat $
          List.zipWith V.horizJoin
            (V.string V.defAttr "knit> " :
              List.repeat (V.string V.defAttr "  ... "))
            [ V.horizCat
              [ V.char (attrFn (row, column) V.defAttr) char
              | (column, char) <- List.zip [1..] (unpack line)
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

drawCommandId :: UiCommandId -> V.Image
drawCommandId (UiCommandId _ t) = V.text' V.defAttr t

data NavAction
  = NavArrowLeft
  | NavArrowRight
  | NavArrowUp
  | NavArrowDown

data InputModification
  = InsertChar Char
  | DeleteBackwards
  | DeleteForwards
  | BreakLine
  | ReplaceBreakLine

data ReplWidgetEvent
  = ReplCommandEvent UiCommandId UiCommandEvent
  | ReplInputModifyEvent InputModification
  | ReplInputNavigationEvent NavAction
  | ReplSendEvent
  | ReplSmartEnterEvent
  | ReplQuitEvent

data ReplCompleted = ReplCompleted | ReplInProgress

handleReplWidgetEvent
  :: UiLangFace
  -> ReplWidgetEvent
  -> StateT ReplWidgetState IO ReplCompleted
handleReplWidgetEvent langFace = fix $ \go -> \case
  ReplQuitEvent -> return ReplCompleted
  ReplInputModifyEvent modification -> do
    zoom replWidgetTextZipperL $ modify $
      case modification of
        InsertChar c -> insertChar c
        DeleteBackwards -> deletePrevChar
        DeleteForwards -> deleteChar
        BreakLine -> smartBreakLine
        ReplaceBreakLine -> smartBreakLine . deletePrevChar
    replReparse langFace
    return ReplInProgress
  ReplInputNavigationEvent nav -> do
    zoom replWidgetTextZipperL $ modify $
      case nav of
        NavArrowLeft -> moveLeft
        NavArrowRight -> moveRight
        NavArrowUp -> moveUp
        NavArrowDown -> moveDown
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
  ReplSendEvent -> do
    replParseResult <- use replWidgetParseResultL
    case replParseResult of
      ReplParseFailure{..} -> do
        let out = OutputInfo $ \w -> pprDoc w rpfParseErrDoc
        zoom replWidgetOutL $ modify (out:)
      ReplParseSuccess{..} -> do
        commandId <- liftIO rpfPutCommand
        zoom replWidgetTextZipperL $ modify $ clearZipper
        let out = OutputCommand commandId (\w -> pprDoc w rpfExprDoc) Nothing
        zoom replWidgetOutL $ modify (out:)
        replReparse langFace
    return ReplInProgress
  ReplCommandEvent commandId commandEvent -> do
    zoom (replWidgetOutL . traversed) $
      modify (updateCommandResult commandId commandEvent)
    return ReplInProgress

isQuitCommand :: Text -> Bool
isQuitCommand t =
  Text.strip t `List.elem` ["quit", "q", ":quit", ":q", "exit"]

smartBreakLine :: TextZipper Text -> TextZipper Text
smartBreakLine tz =
  let indentation = Text.takeWhile Char.isSpace (currentLine tz)
  in insertMany indentation (breakLine tz)

updateCommandResult
  :: UiCommandId
  -> UiCommandEvent
  -> OutputElement
  -> OutputElement
updateCommandResult
  commandId
  commandEvent
  (OutputCommand commandId' commandSrc oldResultImage) | eqCommandId commandId commandId'
  = OutputCommand commandId commandSrc mCommandResultImage
  where
    eqCommandId = (==) `on` cmdIdEqObject
    mCommandResultImage =
      case commandEvent of
        UiCommandSuccess doc ->
          Just $ \w -> pprDoc w doc
        UiCommandFailure doc ->
          Just $ \w -> pprDoc w doc   -- TODO: highlight as an error
        UiCommandOutput _ ->
          -- TODO: create a new field in 'OutputCommand' and append
          -- the 'doc' there.
          oldResultImage
updateCommandResult _ _ outCmd = outCmd

pprDoc :: Int -> PP.Doc -> V.Image
pprDoc w s = ansiToVty $ PP.renderSmart 0.985 w s

ariadneBanner :: Int -> V.Image
ariadneBanner _ = V.vertCat $ List.map (V.text' V.defAttr)
  [ "             ___         _           __         "
  , "            /   |  _____(_)___ _____/ /___  ___ "
  , "           / /| | / ___/ / __ `/ __  / __ \\/ _ \\"
  , "          / ___ |/ /  / / /_/ / /_/ / / / /  __/"
  , "         /_/  |_/_/  /_/\\__,_/\\__,_/_/ /_/\\___/ "
  , ""
  , "              Press <Enter> to send a command,"
  , "        <Backslash> <Enter> to insert a line break."
  ]
