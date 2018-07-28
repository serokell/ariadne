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

import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Core.QItemSelectionModel as QItemSelectionModel
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Gui.QStandardItemModel as QStandardItemModel
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QDialogButtonBox as QDialogButtonBox
import qualified Graphics.UI.Qtah.Widgets.QFormLayout as QFormLayout
import qualified Graphics.UI.Qtah.Widgets.QGroupBox as QGroupBox
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
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
    { itemNameLabel :: QLabel.QLabel
    , balanceLabel :: QLabel.QLabel
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
  infoLayout <- QVBoxLayout.new
  QLayout.setContentsMarginsRaw infoLayout 36 17 17 36

  itemNameLabel <- QLabel.newWithText ("Select something..." :: String)
  QLayout.addWidget infoLayout itemNameLabel

  balancePane <- QWidget.new
  QObject.setObjectName balancePane ("balancePane" :: String)
  QWidget.setSizePolicyRaw balancePane Preferred Maximum
  balanceLayout <- QHBoxLayout.new
  QLayout.setContentsMarginsRaw balanceLayout 0 0 0 0
  QWidget.setLayout balancePane balanceLayout
  QLayout.addWidget infoLayout balancePane

  balanceLabel <- QLabel.new
  balanceCommandId <- newIORef Nothing
  QLayout.addWidget balanceLayout balanceLabel

  accountControls <- QVBoxLayout.new
  sendButton' <- QPushButton.newWithText ("SEND" :: String)
  requestButton <- QPushButton.newWithText ("REQUEST" :: String)

  -- TODO implement corresponding functionality
  QWidget.hide sendButton'
  QWidget.hide requestButton

  QBoxLayout.addStretch accountControls
  QLayout.addWidget accountControls sendButton'
  QLayout.addWidget accountControls requestButton
  QBoxLayout.addStretch accountControls

  QBoxLayout.addLayout balanceLayout accountControls
  QBoxLayout.setStretch balanceLayout 0 1
  QBoxLayout.setStretch balanceLayout 1 0

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

  QLayout.addWidget infoLayout sendForm

  QBoxLayout.addStretch infoLayout
  widget <- QWidget.new
  QWidget.setLayout widget infoLayout

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
  = WalletInfoSelectionChange UiSelectionInfo
  | WalletInfoSendCommandResult UiCommandId UiSendCommandResult

handleWalletInfoEvent
  :: UiLangFace
  -> WalletInfoEvent
  -> UI WalletInfo ()
handleWalletInfoEvent UiLangFace{..} ev = do
  WalletInfo{..} <- ask
  lift $ case ev of
    WalletInfoSelectionChange selectionInfo -> do
      let (itemName, balance) =
            case selectionInfo of
              UiSelectionWallet UiWalletInfo{..} -> (uwiLabel, uwiBalance)
              UiSelectionAccount UiAccountInfo{..} -> (uaciLabel, uaciBalance)

      whenJust itemName $ QLabel.setText itemNameLabel . toString
      QLabel.setText balanceLabel $ toString balance

    WalletInfoSendCommandResult _commandId result -> case result of
      UiSendCommandSuccess hash -> do
        QWidget.setEnabled sendButton True
        QLineEdit.clear sendAddress
        QLineEdit.clear sendAmount
        void $ QMessageBox.information sendForm ("Success" :: String) $ toString hash
      UiSendCommandFailure err -> do
        QWidget.setEnabled sendButton True
        void $ QMessageBox.critical sendForm ("Error" :: String) $ toString err
