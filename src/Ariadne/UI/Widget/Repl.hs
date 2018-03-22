module Ariadne.UI.Widget.Repl where

import Prelude hiding (unlines)
import Data.Text
import Data.Function (fix)
import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Text.Zipper
import Data.Unique
import Numeric (showHex)
import Control.Exception (displayException)

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Text.PrettyPrint.ANSI.Leijen as Ppr.A
import qualified Graphics.Vty as V

import qualified Lang as Auxx

import Ariadne.UI.AnsiToVty
import Ariadne.Face
import Ariadne.Util

data OutputElement
  = OutputCommand CommandId Text (Maybe (Int -> V.Image))
  | OutputInfo Text

data ReplWidgetState =
  ReplWidgetState
    {
      replWidgetExpr :: Either Auxx.ParseError (Auxx.Expr Auxx.Name),
      replWidgetTextZipper :: TextZipper Text,
      replWidgetOut :: [OutputElement]
    }

makeLensesWith postfixLFields ''ReplWidgetState

replWidgetText :: ReplWidgetState -> Text
replWidgetText = unlines . getText . replWidgetTextZipper

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
                  [] -> V.text' V.defAttr "Press <Enter> to send a command, ^N to insert line break"
                  xs -> V.vertCat (fmap drawOutputElement xs)
              drawOutputElement (OutputInfo t) =
                V.text' V.defAttr t
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
              zipper = replWidgetTextZipper replWidgetState
              img =
                V.string V.defAttr "auxx> " `V.horizJoin`
                V.vertCat
                  [ V.text' V.defAttr line
                  | line <- getText zipper
                  ]
              curLoc =
                let (y, x) = cursorPosition zipper
                in B.CursorLocation (B.Location (x + 6, y)) Nothing
            return $
              B.emptyResult
                & B.imageL .~ img
                & B.cursorsL .~ [curLoc | hasFocus]
        }

drawCommandId :: CommandId -> Text
drawCommandId (CommandId u) = pack $
  '<' : showHex (hashUnique u) ">"

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

data ReplWidgetEvent
  = ReplCommandResultEvent CommandId CommandResult
  | ReplInputModifyEvent InputModification
  | ReplInputNavigationEvent NavAction
  | ReplSendEvent

handleReplWidgetEvent
  :: AuxxFace
  -> ReplWidgetEvent
  -> StateT ReplWidgetState IO ()
handleReplWidgetEvent AuxxFace{..} = \case
  ReplInputModifyEvent modification -> do
    zoom replWidgetTextZipperL $ modify $
      case modification of
        InsertChar c -> insertChar c
        DeleteBackwards -> deletePrevChar
        DeleteForwards -> deleteChar
        BreakLine -> breakLine
    replReparse
  ReplInputNavigationEvent nav -> do
    zoom replWidgetTextZipperL $ modify $
      case nav of
        NavArrowLeft -> moveLeft
        NavArrowRight -> moveRight
        NavArrowUp -> moveUp
        NavArrowDown -> moveDown
  ReplSendEvent -> do
    exprOrErr <- use replWidgetExprL
    case exprOrErr of
      Left _parseErr -> return ()
      Right expr -> do
        commandId <- liftIO $ putAuxxCommand expr
        zoom replWidgetTextZipperL $ modify $ clearZipper
        let out = OutputCommand commandId (pack (show expr)) Nothing
        zoom replWidgetOutL $ modify (out:)
        replReparse
  ReplCommandResultEvent commandId commandResult -> do
    zoom (replWidgetOutL . traversed) $
      modify (updateCommandResult commandId commandResult)

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
    pprDoc w s = ansiToVty $ Ppr.A.renderSmart 0.4 w s
updateCommandResult _ _ outCmd = outCmd
