module Ariadne.UI.Widget.Repl where

import Prelude hiding (unlines)
import Data.Text as Text
import Data.Char as Char
import Data.Function (fix)
import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Text.Zipper
import Data.Unique
import Data.List as List
import Numeric (showIntAtBase)
import Control.Exception (displayException)
import Text.Earley (Report (..))

import qualified Data.Loc as Loc
import qualified Data.Loc.Span as Loc

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Text.PrettyPrint.ANSI.Leijen as Ppr.A
import qualified Graphics.Vty as V

import qualified Lang as Auxx
import qualified Printer as Auxx

import Ariadne.UI.AnsiToVty
import Ariadne.Face
import Ariadne.Util

data OutputElement
  = OutputCommand CommandId Text (Maybe (Int -> V.Image))
  | OutputInfo (Int -> V.Image)

data ReplWidgetState =
  ReplWidgetState
    {
      replWidgetExpr :: Either Auxx.ParseError (Auxx.Expr Auxx.Name),
      replWidgetTextZipper :: TextZipper Text,
      replWidgetOut :: [OutputElement]
    }

makeLensesWith postfixLFields ''ReplWidgetState

replWidgetText :: ReplWidgetState -> Text
replWidgetText = Text.unlines . getText . replWidgetTextZipper

replReparse :: Monad m => StateT ReplWidgetState m ()
replReparse = do
  t <- gets replWidgetText
  replWidgetExprL .= Auxx.parse t

initReplWidget
  :: ReplWidgetState
initReplWidget =
  fix $ \this -> ReplWidgetState
    {
      replWidgetExpr = Auxx.parse (replWidgetText this),
      replWidgetTextZipper = textZipper [] Nothing,
      replWidgetOut = []
    }

drawReplWidget
  :: Bool
  -> ReplWidgetState
  -> B.Widget name
drawReplWidget hasFocus replWidgetState =
    B.vBox [drawOutput, B.hBorder, drawInput]
  where
    outElems = Prelude.reverse (replWidgetOut replWidgetState)
    drawOutput =
      B.Widget
        {
          B.hSize = B.Greedy,
          B.vSize = B.Greedy,
          B.render = do
            rdrCtx <- B.getContext
            let
              img =
                V.cropTop ((rdrCtx ^. B.availHeightL) - 1) $
                case outElems of
                  [] -> V.text' V.defAttr "Press <Enter> to send a command, <Backslash> <Enter> to insert a line break"
                  xs -> V.vertCat (fmap drawOutputElement xs)
              drawOutputElement (OutputInfo mkImg) =
                mkImg (rdrCtx ^. B.availWidthL)
              drawOutputElement (OutputCommand commandId commandSrc mCommandOut) =
                V.vertCat
                  [ V.horizCat
                    [ V.text' V.defAttr (drawCommandId commandId)
                    , V.text' V.defAttr " "
                    , V.text' V.defAttr commandSrc
                    ]
                  , case mCommandOut of
                      Nothing -> V.text' V.defAttr "<waiting for output>"
                      Just mkImg -> mkImg (rdrCtx ^. B.availWidthL)
                  ]
            return $
              B.emptyResult
                & B.imageL .~ img
        }
    drawInput =
      B.Widget
        {
          B.hSize = B.Greedy,
          B.vSize = B.Fixed,
          B.render = do
            let
              attrFn :: (Int, Int) -> V.Attr -> V.Attr
              attrFn loc =
                case replWidgetExpr replWidgetState of
                  Right _ -> id
                  Left parseErr ->
                    if parseErrSpanFn parseErr loc
                    then (`V.withBackColor` V.red)
                    else id
              zipper = replWidgetTextZipper replWidgetState
              img =
                V.vertCat $ List.zipWith V.horizJoin
                  (V.string V.defAttr "auxx> " :
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
        }

parseErrSpanFn :: Auxx.ParseError -> (Int, Int) -> Bool
parseErrSpanFn parseErr (row, column) = inSpan
  where
    Auxx.ParseError _ report = parseErr
    spans = List.map fst (unconsumed report)
    inSpan = List.any inSpan1 spans
    inSpan1 = Loc.overlapping $
      Loc.fromTo
        (Loc.loc (fromIntegral row) (fromIntegral column))
        (Loc.loc (fromIntegral row) (fromIntegral column + 1))

drawCommandId :: CommandId -> Text
drawCommandId (CommandId u) = pack $
    '<' : showIntAtBase 36 base36Char (hashUnique u) ">"
  where
    base36Char = (alphabet!!)
    alphabet = "0123456789" ++ ['a'..'z']

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
  = ReplCommandResultEvent CommandId CommandResult
  | ReplInputModifyEvent InputModification
  | ReplInputNavigationEvent NavAction
  | ReplSendEvent
  | ReplSmartEnterEvent
  | ReplQuitEvent

data ReplCompleted = ReplCompleted | ReplInProgress

handleReplWidgetEvent
  :: AuxxFace
  -> ReplWidgetEvent
  -> StateT ReplWidgetState IO ReplCompleted
handleReplWidgetEvent AuxxFace{..} = fix $ \go -> \case
  ReplQuitEvent -> return ReplCompleted
  ReplInputModifyEvent modification -> do
    zoom replWidgetTextZipperL $ modify $
      case modification of
        InsertChar c -> insertChar c
        DeleteBackwards -> deletePrevChar
        DeleteForwards -> deleteChar
        BreakLine -> smartBreakLine
        ReplaceBreakLine -> smartBreakLine . deletePrevChar
    replReparse
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
    exprOrErr <- use replWidgetExprL
    case exprOrErr of
      Left parseErr -> do
        let out = OutputInfo $ \w -> pprDoc w (Auxx.ppParseError parseErr)
        zoom replWidgetOutL $ modify (out:)
      Right expr -> do
        commandId <- liftIO $ putAuxxCommand expr
        zoom replWidgetTextZipperL $ modify $ clearZipper
        let out = OutputCommand commandId (Auxx.pprExpr expr) Nothing
        zoom replWidgetOutL $ modify (out:)
        replReparse
    return ReplInProgress
  ReplCommandResultEvent commandId commandResult -> do
    zoom (replWidgetOutL . traversed) $
      modify (updateCommandResult commandId commandResult)
    return ReplInProgress

isQuitCommand :: Text -> Bool
isQuitCommand t = Text.strip t `List.elem` ["quit", "q", ":quit", ":q"]

smartBreakLine :: TextZipper Text -> TextZipper Text
smartBreakLine tz =
  let indentation = Text.takeWhile Char.isSpace (currentLine tz)
  in insertMany indentation (breakLine tz)

updateCommandResult
  :: CommandId
  -> CommandResult
  -> OutputElement
  -> OutputElement
updateCommandResult
  commandId
  commandResult
  (OutputCommand commandId' commandSrc _) | commandId == commandId'
  = OutputCommand commandId commandSrc (Just commandResultImage)
  where
    commandResultImage w =
      case commandResult of
        CommandSuccess v -> V.string V.defAttr (show v)
        CommandEvalError e -> pprDoc w (Auxx.ppEvalError e)
        CommandProcError e -> pprDoc w (Auxx.ppResolveErrors e)
        CommandException e -> V.string V.defAttr (displayException e)
updateCommandResult _ _ outCmd = outCmd

pprDoc :: Int -> Ppr.A.Doc -> V.Image
pprDoc w s = ansiToVty $ Ppr.A.renderSmart 0.4 w s
