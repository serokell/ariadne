module Ariadne.UI.Qt.Widgets.Logs
    ( Logs
    , initLogs
    , displayLogMessage
    , showLogsWindow
    ) where

import Universum

import Formatting

import Control.Lens (makeLensesWith)
import IiExtras (postfixLFields)

import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.AnsiToHTML
import Ariadne.UI.Qt.UI

data Logs =
  Logs
    { dialog :: QDialog.QDialog
    , logsEdit :: QTextEdit.QTextEdit
    }

makeLensesWith postfixLFields ''Logs

initLogs :: IO (QDialog.QDialog, Logs)
initLogs = do
  dialog <- QDialog.new
  QWidget.setWindowTitle dialog ("Logs" :: String)
  QWidget.resizeRaw dialog 1024 720

  layout <- QVBoxLayout.newWithParent dialog

  logsEdit <- QTextEdit.new
  QTextEdit.setReadOnly logsEdit True

  QLayout.addWidget layout logsEdit

  return (dialog, Logs{..})

showLogsWindow :: UI Logs ()
showLogsWindow = do
  dialog <- view dialogL

  liftIO $ QWidget.show dialog

displayLogMessage :: Text -> UI Logs ()
displayLogMessage message = do
    logsEdit <- view logsEditL
    liftIO $ QTextEdit.insertHtml logsEdit $ toString $
        format (spanFormat % "<br>") $ csiToHTML message
    liftIO $ QTextEdit.ensureCursorVisible logsEdit
