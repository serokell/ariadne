module Ariadne.UI.Qt.Widgets.Repl
       ( Repl
       , initRepl
       , displayCommandInfo
       ) where

import Prelude (showChar, showString)
import Universum

import Control.Lens (makeLensesWith)
import Formatting
import IiExtras

import qualified System.Console.ANSI.Types as AT
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI

data Repl =
  Repl
    { layout :: QVBoxLayout.QVBoxLayout
    , cmdHistory :: QTextEdit.QTextEdit
    , cmdLine :: QLineEdit.QLineEdit
    }

makeLensesWith postfixLFields ''Repl

initRepl :: UiLangFace -> IO (QVBoxLayout.QVBoxLayout, Repl)
initRepl langFace = do
  cmdHistory <- QTextEdit.new
  QTextEdit.setReadOnly cmdHistory True

  cmdLine <- QLineEdit.new

  layout <- QVBoxLayout.new
  QLayout.addWidget layout cmdHistory
  QLayout.addWidget layout cmdLine

  connectSignal Repl{..} cmdLine QLineEdit.returnPressedSignal $
    returnPressed langFace

  QWidget.setFocus cmdLine

  return (layout, Repl{..})

returnPressed :: UiLangFace -> UI Repl ()
returnPressed UiLangFace{..} = do
  cmdLine <- view cmdLineL
  cmd <- liftIO $ QLineEdit.text cmdLine
  case langParse $ toText cmd of
    Left err -> displayCommandInfo " " $ langPpParseError err
    Right expr -> do
      UiCommandId{..} <- liftIO $ langPutCommand expr
      displayCommandInfo "&gt; " $ langPpExpr expr
      liftIO $ QLineEdit.clear cmdLine

displayCommandInfo :: Text -> PP.Doc -> UI Repl ()
displayCommandInfo cmdIdRendered doc = displayNewLine $ toString $
  format (stext % stext % "<br>")
    cmdIdRendered
    (simpleDocToHTML $ PP.renderPretty 0.985 120 doc)

displayNewLine :: String -> UI Repl ()
displayNewLine str = do
  cmdHistory <- view $ cmdHistoryL
  liftIO $ QTextEdit.insertHtml cmdHistory str

getColor :: [AT.SGR] -> String
getColor xs = case colors of
  []    -> "black"
  (c:_) -> toColor c
  where
    toColor (AT.SetColor _ _ c) = case c of
      AT.Black   -> "white"
      AT.Red     -> "red"
      AT.Green   -> "green"
      AT.Yellow  -> "yellow"
      AT.Blue    -> "blue"
      AT.Magenta -> "magenta"
      AT.Cyan    -> "cyan"
      AT.White   -> "black"
    toColor _ = "black"

    colors = filter isColor xs

    isColor = \case
      AT.SetColor {} -> True
      _ -> False

simpleDocToHTML :: PP.SimpleDoc -> Text
simpleDocToHTML sdoc = toText $
  format
    ("<span style='\
      \font-family: Hack, \"Fira Code\", \"Source Code Pro\", \"DejaVu Sans Mono\", monospace;\
      \white-space: pre-wrap;'><span>" % string % "</span></span>")
  $ go sdoc ""
  where
    indentation i = if i <= 0 then "" else replicate i ' '
    go = \case
      PP.SFail -> error "simpleDocToHTML: impossible"
      PP.SEmpty -> identity
      PP.SChar c x -> showChar c . go x
      PP.SText _ s x -> showString s . go x
      PP.SLine i x -> showString "<br>" . showString (indentation i) . go x
      PP.SSGR s x -> showString ("</span><span style=\"color: " ++ getColor s ++ ";\">") . go x
