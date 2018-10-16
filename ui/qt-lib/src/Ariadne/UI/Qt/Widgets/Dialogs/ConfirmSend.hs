module Ariadne.UI.Qt.Widgets.Dialogs.ConfirmSend
  ( runConfirmSend
  , closeConfirmSend
  , ConfirmationResult(..)
  , ConfirmSend
  ) where

import Control.Lens (makeLensesWith)

import qualified Data.Text as T

import Graphics.UI.Qtah.Signal (connect_)

import qualified Graphics.UI.Qtah.Gui.QIcon as QIcon
import qualified Graphics.UI.Qtah.Gui.QMovie as QMovie
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.Dialogs.Util
import Ariadne.UIConfig
import Ariadne.Util

data ConfirmSend =
  ConfirmSend
    { dialog :: QDialog.QDialog
    , isSure :: QCheckBox.QCheckBox
    , confirmButton :: QPushButton.QPushButton
    , cancelButton :: QPushButton.QPushButton
    , confirmButtonGif :: QMovie.QMovie
    , gifButton :: QPushButton.QPushButton
    }

data ConfirmationResult
  = ConfirmationAccepted
  | ConfirmationCanceled

makeLensesWith postfixLFields ''ConfirmSend

initConfirmSend :: [UiConfirmSendInfo] -> MVar Bool -> IO ConfirmSend
initConfirmSend confirmInfo resultVar = do
  dialog <- QDialog.new
  QWidget.resizeRaw dialog 776 266
  QWidget.setWindowTitle dialog (toString sendHeaderMessage)

  layout <- createLayout dialog

  label <- QLabel.newWithText ("SEND" :: String)
  addHeader layout label
  let infos = T.intercalate ", " $ map (\UiConfirmSendInfo{..} ->
        "<b>" <> csiAmount <> " " <> csiCoin <> "</b> to <b>" <> csiAddress <> "</b>") confirmInfo

  infoLabel <- QLabel.newWithText ("You are sending " <> toString infos)
  QLabel.setWordWrap infoLabel True
  QBoxLayout.addWidget layout infoLabel
  addSeparator layout

  isSure <- createCheckBox layout CheckboxOnLeft sendDefinitiveMessage

  confirmButtonGif <- QMovie.newWithFile (":/images/confirm-ic.gif" :: String)

  buttonsLayout <- QHBoxLayout.new
  cancelButton <- QPushButton.newWithText ("CANCEL" :: String)
  confirmButton <- QPushButton.newWithText ("CONFIRM" :: String)
  gifButton <- QPushButton.new
  QBoxLayout.setSpacing buttonsLayout 12
  QBoxLayout.addStretch buttonsLayout
  QBoxLayout.addWidget buttonsLayout cancelButton
  QBoxLayout.addWidget buttonsLayout confirmButton
  QBoxLayout.addWidget buttonsLayout gifButton
  QBoxLayout.addLayout layout buttonsLayout
  QBoxLayout.addStretch buttonsLayout

  setProperty cancelButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  setProperty confirmButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  setProperty gifButton ("dialogButtonRole" :: Text) ("gifButton" :: Text)
  setProperty cancelButton ("styleRole" :: Text) ("secondaryButton" :: Text)

  let cs = ConfirmSend{..}

  QWidget.setEnabled confirmButton False
  QWidget.setVisible gifButton False
  QBoxLayout.addStretch layout
  QWidget.adjustSize dialog

  connect_ confirmButton QAbstractButton.clickedSignal $ \_ -> confirmButtonClicked cs resultVar
  connect_ cancelButton QAbstractButton.clickedSignal $ \_ -> cancelButtonClicked cs resultVar
  connect_ isSure QAbstractButton.toggledSignal $ \_ -> isSureToggled cs
  connect_ confirmButtonGif QMovie.frameChangedSignal $ \_ ->
    QAbstractButton.setIcon gifButton =<< QIcon.newWithPixmap  =<< QMovie.currentPixmap confirmButtonGif

  return cs

confirmButtonClicked :: ConfirmSend -> MVar Bool -> IO ()
confirmButtonClicked ConfirmSend{..} resultVar = do
  QWidget.setEnabled cancelButton False
  QWidget.setEnabled isSure False
  QWidget.setVisible confirmButton False
  QWidget.setVisible gifButton True

  putMVar resultVar True
  QMovie.start confirmButtonGif

cancelButtonClicked :: ConfirmSend -> MVar Bool -> IO ()
cancelButtonClicked ConfirmSend{..} resultVar = do
  void $ QWidget.close dialog
  putMVar resultVar False

isSureToggled :: ConfirmSend -> IO ()
isSureToggled ConfirmSend{..} =
  QAbstractButton.isChecked isSure >>= QWidget.setEnabled confirmButton

runConfirmSend :: [UiConfirmSendInfo] -> MVar Bool -> IO ConfirmSend
runConfirmSend confirmInfo resultVar = do
  cs@ConfirmSend{..} <- initConfirmSend confirmInfo resultVar
  QWidget.show dialog
  return cs

closeConfirmSend :: ConfirmSend -> IO ()
closeConfirmSend ConfirmSend{..} =
  void $ QWidget.close dialog
