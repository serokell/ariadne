module Ariadne.UI.Qt.Widgets.Dialogs.ConfirmSend
  ( runConfirmSend
  , ConfirmationResult(..)
  ) where

import Control.Lens (makeLensesWith)

import qualified Data.Text as T

import Graphics.UI.Qtah.Signal (connect_)

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
    }


data ConfirmationResult
  = ConfirmationAccepted
  | ConfirmationCanceled

makeLensesWith postfixLFields ''ConfirmSend

initConfirmSend :: [UiConfirmSendInfo] -> IO ConfirmSend
initConfirmSend confirmInfo = do
  dialog <- QDialog.new
  QWidget.resizeRaw dialog 776 266
  QWidget.setWindowTitle dialog (toString sendHeaderMessage)

  layout <- createLayout dialog

  label <- QLabel.newWithText ("SEND" :: String)
  addHeader layout label
  let infos = T.intercalate ", " $ map (\UiConfirmSendInfo{..} ->
        "<b>" <> csiAmount <> " " <> csiCoin <> "</b> to <b>" <> csiAddress <> "</b>") confirmInfo

  infoLabel <- QLabel.newWithText ("You are sending " <> toString infos :: String)
  QLabel.setWordWrap infoLabel True
  QBoxLayout.addWidget layout infoLabel
  addSeparator layout

  isSure <- createCheckBox layout CheckboxOnLeft sendDefinitiveMessage

  buttonsLayout <- QHBoxLayout.new
  cancelButton <- QPushButton.newWithText ("CANCEL" :: String)
  confirmButton <- QPushButton.newWithText ("CONFIRM" :: String)
  QBoxLayout.setSpacing buttonsLayout 12
  QBoxLayout.addStretch buttonsLayout
  QBoxLayout.addWidget buttonsLayout cancelButton
  QBoxLayout.addWidget buttonsLayout confirmButton
  QBoxLayout.addLayout layout buttonsLayout
  QBoxLayout.addStretch buttonsLayout

  setProperty cancelButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  setProperty confirmButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  setProperty cancelButton ("styleRole" :: Text) ("secondaryButton" :: Text)

  let cs = ConfirmSend{..}

  QWidget.setEnabled confirmButton False
  QBoxLayout.addStretch layout
  QWidget.adjustSize dialog

  connect_ confirmButton QAbstractButton.clickedSignal $ \_ -> QDialog.accept dialog
  connect_ cancelButton QAbstractButton.clickedSignal $ \_ -> QDialog.reject dialog
  connect_ isSure QAbstractButton.toggledSignal $ \_ -> isSureToggled cs

  return cs

isSureToggled :: ConfirmSend -> IO ()
isSureToggled ConfirmSend{..} =
  QAbstractButton.isChecked isSure >>= QWidget.setEnabled confirmButton

runConfirmSend :: [UiConfirmSendInfo] -> IO ConfirmationResult
runConfirmSend confirmInfo = do
  ConfirmSend{..} <- initConfirmSend confirmInfo
  result <- toEnum <$> QDialog.exec dialog
  return $ case result of
    QDialog.Accepted -> ConfirmationAccepted
    QDialog.Rejected -> ConfirmationCanceled
