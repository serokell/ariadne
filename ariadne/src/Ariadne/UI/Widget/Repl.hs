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
import Data.Vinyl.TypeLevel
import Data.List as List
import Numeric (showIntAtBase)
import Control.Exception (displayException)
import Text.Earley (Report (..))

import qualified Data.Loc as Loc
import qualified Data.Loc.Span as Loc

import qualified Brick as B
import qualified Text.PrettyPrint.ANSI.Leijen as Ppr.A
import qualified Graphics.Vty as V

import Ariadne.UI.AnsiToVty
import Ariadne.Face
import Ariadne.Util

import qualified Knit

type Components components =
  ( Knit.KnownSpine components
  , AllConstrained (Knit.ComponentTokenizer components) components
  , AllConstrained (Knit.ComponentLitGrammar components) components
  , Knit.PrettyPrintValue components
  )

data OutputElement
  = OutputCommand CommandId (Int -> V.Image) (Maybe (Int -> V.Image))
  | OutputInfo (Int -> V.Image)

data ReplWidgetState components =
  ReplWidgetState
    { replWidgetExpr :: Either (Knit.ParseError components) (Knit.Expr Knit.CommandName components)
    , replWidgetTextZipper :: TextZipper Text
    , replWidgetOut :: [OutputElement]
    }

makeLensesWith postfixLFields ''ReplWidgetState

replWidgetText :: ReplWidgetState components -> Text
replWidgetText = Text.unlines . getText . replWidgetTextZipper

replReparse
  :: (Monad m, Components components)
  => StateT (ReplWidgetState components) m ()
replReparse = do
  t <- gets replWidgetText
  replWidgetExprL .= Knit.parse t

initReplWidget
  :: Components components
  => ReplWidgetState components
initReplWidget =
  fix $ \this -> ReplWidgetState
    { replWidgetExpr = Knit.parse (replWidgetText this)
    , replWidgetTextZipper = textZipper [] Nothing
    , replWidgetOut = [OutputInfo ariadneBanner]
    }

drawReplOutputWidget
  :: Bool
  -> ReplWidgetState components
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
            cmdInfo = V.text' V.defAttr (drawCommandId commandId)
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
  -> ReplWidgetState components
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
          case replWidgetExpr replWidgetState of
            Right _ -> id
            Left parseErr ->
              if parseErrSpanFn parseErr loc
              then (`V.withBackColor` V.red)
              else id
        zipper = replWidgetTextZipper replWidgetState
        img =
          V.vertCat $
          List.zipWith V.horizJoin
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

parseErrSpanFn :: Knit.ParseError components -> (Int, Int) -> Bool
parseErrSpanFn parseErr (row, column) = inSpan
  where
    Knit.ParseError _ report = parseErr
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

data ReplWidgetEvent components
  = ReplCommandResultEvent CommandId (CommandResult components)
  | ReplInputModifyEvent InputModification
  | ReplInputNavigationEvent NavAction
  | ReplSendEvent
  | ReplSmartEnterEvent
  | ReplQuitEvent

data ReplCompleted = ReplCompleted | ReplInProgress

handleReplWidgetEvent
  :: Components components
  => KnitFace components
  -> ReplWidgetEvent components
  -> StateT (ReplWidgetState components) IO ReplCompleted
handleReplWidgetEvent KnitFace{..} = fix $ \go -> \case
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
        let out = OutputInfo $ \w -> pprDoc w (Knit.ppParseError parseErr)
        zoom replWidgetOutL $ modify (out:)
      Right expr -> do
        commandId <- liftIO $ putKnitCommand expr
        zoom replWidgetTextZipperL $ modify $ clearZipper
        let out = OutputCommand commandId (\w -> pprDoc w (Knit.ppExpr expr)) Nothing
        zoom replWidgetOutL $ modify (out:)
        replReparse
    return ReplInProgress
  ReplCommandResultEvent commandId commandResult -> do
    zoom (replWidgetOutL . traversed) $
      modify (updateCommandResult commandId commandResult)
    return ReplInProgress

isQuitCommand :: Text -> Bool
isQuitCommand t =
  Text.strip t `List.elem` ["quit", "q", ":quit", ":q", "exit"]

smartBreakLine :: TextZipper Text -> TextZipper Text
smartBreakLine tz =
  let indentation = Text.takeWhile Char.isSpace (currentLine tz)
  in insertMany indentation (breakLine tz)

updateCommandResult
  :: Components components
  => CommandId
  -> CommandResult components
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
        CommandSuccess v -> pprDoc w (Knit.ppValue v)
        CommandEvalError e -> pprDoc w (Knit.ppEvalError e)
        CommandProcError e -> pprDoc w (Knit.ppResolveErrors e)
        CommandException e -> V.string V.defAttr (displayException e)
updateCommandResult _ _ outCmd = outCmd

pprDoc :: Int -> Ppr.A.Doc -> V.Image
pprDoc w s = ansiToVty $ Ppr.A.renderSmart 0.4 w s

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
