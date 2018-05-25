module Ariadne.UI.Qt.Widgets.WalletInfo
       ( WalletInfo
       , initWalletInfo

       , WalletInfoEvent(..)
       , handleWalletInfoEvent
       ) where

import Universum

import Control.Lens (makeLensesWith)
import Graphics.UI.Qtah.Signal (connect_)
import IiExtras

import qualified Graphics.UI.Qtah.Core.QItemSelectionModel as QItemSelectionModel
import qualified Graphics.UI.Qtah.Gui.QStandardItemModel as QStandardItemModel
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QDialogButtonBox as QDialogButtonBox
import qualified Graphics.UI.Qtah.Widgets.QFormLayout as QFormLayout
import qualified Graphics.UI.Qtah.Widgets.QGroupBox as QGroupBox
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI

data WalletInfo =
  WalletInfo
    { balanceLabel :: QLabel.QLabel
    , balanceCommandId :: IORef (Maybe UiCommandId)
    , sendForm :: QGroupBox.QGroupBox
    , sendAddress :: QLineEdit.QLineEdit
    , sendAmount :: QLineEdit.QLineEdit
    , sendButton :: QPushButton.QPushButton
    , itemModel :: QStandardItemModel.QStandardItemModel
    , selectionModel :: QItemSelectionModel.QItemSelectionModel
    }

makeLensesWith postfixLFields ''WalletInfo

initWalletInfo
  :: UiLangFace
  -> QStandardItemModel.QStandardItemModel
  -> QItemSelectionModel.QItemSelectionModel
  -> IO (QWidget.QWidget, WalletInfo)
initWalletInfo langFace itemModel selectionModel = do
  balanceLabel <- QLabel.newWithText ("nothing selected" :: String)
  balanceCommandId <- newIORef Nothing

  infoLayout <- QFormLayout.new
  QFormLayout.addRowStringWidget infoLayout ("Balance:" :: String) balanceLabel

  infoBox <- QGroupBox.newWithTitle ("Selected item" :: String)
  QWidget.setLayout infoBox infoLayout

  sendAddress <- QLineEdit.new
  sendAmount <- QLineEdit.new

  sendButtonBox <- QDialogButtonBox.new
  sendButton <- QDialogButtonBox.addButtonWithText sendButtonBox ("Send" :: String) QDialogButtonBox.AcceptRole

  sendFormLayout <- QFormLayout.new
  QFormLayout.addRowStringWidget sendFormLayout ("Address:" :: String) sendAddress
  QFormLayout.addRowStringWidget sendFormLayout ("Amount:" :: String) sendAmount
  QFormLayout.addRowWidget sendFormLayout sendButtonBox

  sendForm <- QGroupBox.newWithTitle ("Send transaction" :: String)
  QWidget.setLayout sendForm sendFormLayout

  layout <- QVBoxLayout.new
  QLayout.setContentsMarginsRaw layout 0 0 0 0
  QLayout.addWidget layout infoBox
  QLayout.addWidget layout sendForm
  QBoxLayout.addStretch layout

  widget <- QWidget.new
  QWidget.setLayout widget layout

  connect_ sendButton QAbstractButton.clickedSignal $
    sendClicked langFace WalletInfo{..}

  return (widget, WalletInfo{..})

sendClicked :: UiLangFace -> WalletInfo -> Bool -> IO ()
sendClicked UiLangFace{..} WalletInfo{..} _checked = do
  address <- toText <$> QLineEdit.text sendAddress
  amount <- toText <$> QLineEdit.text sendAmount
  langPutUiCommand (UiSend address amount) >>= \case
    Left err ->
      void $ QMessageBox.critical sendForm ("Error" :: String) $ toString err
    Right _ ->
      QWidget.setEnabled sendButton False

data WalletInfoEvent
  = WalletInfoSelectionChange
  | WalletInfoBalanceCommandResult UiCommandId UiBalanceCommandResult
  | WalletInfoSendCommandResult UiCommandId UiSendCommandResult

handleWalletInfoEvent
  :: UiLangFace
  -> WalletInfoEvent
  -> UI WalletInfo ()
handleWalletInfoEvent UiLangFace{..} ev = do
  WalletInfo{..} <- ask
  lift $ case ev of
    WalletInfoSelectionChange -> do
      QLabel.setText balanceLabel ("" :: String)
      mCommandId <- atomicModifyIORef' balanceCommandId $ \cid -> (Nothing, cid)
      whenJust (mCommandId >>= cmdTaskId) $ void . langPutUiCommand . UiKill
      whenRightM (langPutUiCommand UiBalance) $ \cid -> do
        QLabel.setText balanceLabel ("calculating..." :: String)
        writeIORef balanceCommandId $ Just cid

    WalletInfoBalanceCommandResult commandId result -> do
      mCommandId <- readIORef balanceCommandId
      when (mCommandId == Just commandId) $ do
        writeIORef balanceCommandId Nothing
        case result of
          UiBalanceCommandSuccess balance -> do
            QLabel.setText balanceLabel $ toString balance
          UiBalanceCommandFailure err -> do
            QLabel.setText balanceLabel $ toString err

    WalletInfoSendCommandResult _commandId result -> case result of
      UiSendCommandSuccess hash -> do
        QWidget.setEnabled sendButton True
        QLineEdit.clear sendAddress
        QLineEdit.clear sendAmount
        void $ QMessageBox.information sendForm ("Success" :: String) $ toString hash
      UiSendCommandFailure err -> do
        QWidget.setEnabled sendButton True
        void $ QMessageBox.critical sendForm ("Error" :: String) $ toString err
