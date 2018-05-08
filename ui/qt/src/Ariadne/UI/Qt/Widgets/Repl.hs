module Ariadne.UI.Qt.Widgets.Repl
       ( Repl
       , initRepl
       , displayCommandInfo
       ) where

import Universum

import Control.Lens (makeLensesWith)
import Formatting
import IiExtras

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.AnsiToHTML
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

  cmdLineLayout <- QHBoxLayout.new
  knitPrompt <- QLabel.newWithText ("knit>" :: String)
  QLayout.addWidget cmdLineLayout knitPrompt
  QLayout.addWidget cmdLineLayout cmdLine

  QBoxLayout.addLayout layout cmdLineLayout

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
