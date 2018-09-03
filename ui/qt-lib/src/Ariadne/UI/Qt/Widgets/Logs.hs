module Ariadne.UI.Qt.Widgets.Logs
    ( Logs
    , initLogs
    , displayLogMessage
    , showLogsWindow
    ) where

import Universum

import Formatting

import Control.Lens (makeLensesWith)

import Graphics.UI.Qtah.Signal (connect_)

import qualified Graphics.UI.Qtah.Widgets.QAbstractScrollArea as QAbstractScrollArea
import qualified Graphics.UI.Qtah.Widgets.QAbstractSlider as QAbstractSlider
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.AnsiToHTML
import Ariadne.UI.Qt.UI
import Ariadne.Util

data Logs =
  Logs
    { dialog :: QDialog.QDialog
    , logsEdit :: QTextEdit.QTextEdit

    , userScrolledRef :: IORef Bool
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

  userScrolledRef <- newIORef False

  let logs = Logs{..}

  scrollBar <- QAbstractScrollArea.verticalScrollBar logsEdit
  connect_ scrollBar QAbstractSlider.rangeChangedSignal $ scrollRangeChanged logs
  connect_ scrollBar QAbstractSlider.valueChangedSignal $ scrollValueChanged logs

  return (dialog, logs)

showLogsWindow :: UI Logs ()
showLogsWindow = do
  dialog <- view dialogL

  liftIO $ QWidget.show dialog

displayLogMessage :: Text -> UI Logs ()
displayLogMessage message = do
  logsEdit <- view logsEditL
  liftIO $ QTextEdit.append logsEdit $ toString $
    format spanFormat $ csiToHTML message

scrollRangeChanged :: Logs -> Int -> Int -> IO ()
scrollRangeChanged Logs{..} _ maximumValue = do
  userScrolled <- readIORef userScrolledRef

  unless userScrolled $ do
    scrollBar <- QAbstractScrollArea.verticalScrollBar logsEdit
    QAbstractSlider.setValue scrollBar maximumValue

scrollValueChanged :: Logs -> Int -> IO ()
scrollValueChanged Logs{..} newValue = do
  maximumValue <- QAbstractScrollArea.verticalScrollBar logsEdit >>= QAbstractSlider.maximum

  writeIORef userScrolledRef $ newValue /= maximumValue
