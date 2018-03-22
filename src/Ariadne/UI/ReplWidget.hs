module Ariadne.UI.ReplWidget where

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
import qualified Brick.Widgets.Edit as B

import qualified Lang as Auxx

import Ariadne.Face
import Ariadne.Util

data OutputElement
  = OutputCommand CommandId Text (Maybe Text)
  | OutputInfo Text

data ReplWidgetSelector
  = AuxxReadName
  | AuxxPrintName
  deriving (Eq, Ord, Show)

data ReplWidgetState name =
  ReplWidgetState
    {
      replWidgetExpr :: Either Auxx.ParseError (Auxx.Expr Auxx.Name),
      replWidgetEditor :: B.Editor Text name,
      replWidgetOut :: [OutputElement]
    }

makeLensesWith postfixLFields ''ReplWidgetState

replWidgetText :: ReplWidgetState name -> Text
replWidgetText = unlines . B.getEditContents . replWidgetEditor

replReparse :: Monad m => StateT (ReplWidgetState name) m ()
replReparse = do
  t <- gets replWidgetText
  replWidgetExprL .= Auxx.parse t

initReplWidget
  :: (ReplWidgetSelector -> name)
  -> ReplWidgetState name
initReplWidget injName =
  fix $ \this -> ReplWidgetState
    {
      replWidgetExpr = Auxx.parse (replWidgetText this),
      replWidgetEditor = B.editorText (injName AuxxReadName) Nothing "",
      replWidgetOut = []
    }

drawReplWidget
  :: (Ord name, Show name)
  => Bool
  -> ReplWidgetState name
  -> B.Widget name
drawReplWidget hasFocus replWidgetState =
    B.vBox [drawOutput, B.hBorder, drawInput]
  where
    outElems = Prelude.reverse (replWidgetOut replWidgetState)
    drawOutput =
      B.padTop B.Max $
      case outElems of
        [] -> B.txt "^S to send a command - try 'help'"
        xs -> B.vBox (fmap drawOutputElement xs)
    drawOutputElement (OutputInfo t) = B.txt t
    drawOutputElement (OutputCommand commandId commandSrc mCommandOut) =
      (B.txt (drawCommandId commandId) B.<+> B.txt " " B.<+> B.txt commandSrc) B.<=>
      (case mCommandOut of
            Nothing -> B.txt "<waiting for output>"
            Just a -> B.txt a)
    drawInput =
        B.str "auxx>" B.<+>
        (B.renderEditor
          (B.txt . unlines)
          hasFocus
          (replWidgetEditor replWidgetState))

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
  -> StateT (ReplWidgetState name) (B.EventM name) ()
handleReplWidgetEvent AuxxFace{..} = \case
  ReplInputModifyEvent modification -> do
    zoom replWidgetEditorL $ modify $ B.applyEdit $
      case modification of
        InsertChar c -> insertChar c
        DeleteBackwards -> deletePrevChar
        DeleteForwards -> deleteChar
        BreakLine -> breakLine
    replReparse
  ReplInputNavigationEvent nav -> do
    zoom replWidgetEditorL $ modify $ B.applyEdit $
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
        zoom replWidgetEditorL $ modify $ B.applyEdit clearZipper
        let out = OutputCommand commandId (pack (show expr)) Nothing
        zoom replWidgetOutL $ modify (out:)
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
  = OutputCommand commandId commandSrc (Just commandResultText)
  where
    commandResultText =
      case commandResult of
        CommandSuccess v -> pack (show v)
        CommandEvalError e -> Auxx.renderAuxxDoc (Auxx.ppEvalError e)
        CommandProcError e -> Auxx.renderAuxxDoc (Auxx.ppResolveErrors e)
        CommandException e -> pack (displayException e)
updateCommandResult _ _ outCmd = outCmd
