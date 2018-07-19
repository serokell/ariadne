module Ariadne.UI.Qt.Widgets.WalletInfo
       ( WalletInfo
       , initWalletInfo

       , WalletInfoEvent(..)
       , handleWalletInfoEvent
       ) where

import Universum

import Data.Text (toUpper)

import Control.Lens (makeLensesWith)
import Graphics.UI.Qtah.Signal (connect_)
import IiExtras

import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Core.QItemSelectionModel as QItemSelectionModel
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Gui.QIcon as QIcon
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
  QLayout.setContentsMarginsRaw infoLayout 0 0 0 0

  headerLayout <- QHBoxLayout.new
  QLayout.setContentsMarginsRaw headerLayout 36 12 36 12
  itemNameLabel <- QLabel.newWithText ("Select something..." :: String)
  QObject.setObjectName itemNameLabel ("itemNameLabel" :: String)
  QLayout.addWidget headerLayout itemNameLabel

  QBoxLayout.addLayout infoLayout headerLayout

  balancePane <- QWidget.new
  QObject.setObjectName balancePane ("balancePane" :: String)
  QWidget.setSizePolicyRaw balancePane Preferred Maximum
  balanceLayout <- QHBoxLayout.new
  QLayout.setContentsMarginsRaw balanceLayout 36 24 36 24
  QWidget.setLayout balancePane balanceLayout
  QLayout.addWidget infoLayout balancePane

  balanceLabel <- QLabel.new
  balanceCommandId <- newIORef Nothing
  QLayout.addWidget balanceLayout balanceLabel

  accountControls <- QVBoxLayout.new
  sendButton' <- QPushButton.newWithText (" SEND" :: String)
  requestButton <- QPushButton.newWithText (" REQUEST" :: String)

  sendIcon <- QIcon.newWithFile (":/images/send-ic.png" :: String)
  receiveIcon <- QIcon.newWithFile (":/images/receive-ic.png" :: String)

  QAbstractButton.setIcon sendButton' sendIcon
  QAbstractButton.setIcon requestButton receiveIcon

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
      let (itemName, (balance, unit)) =
            case selectionInfo of
              UiSelectionWallet UiWalletInfo{..} -> (uwiLabel, uwiBalance)
              UiSelectionAccount UiAccountInfo{..} -> (uaciLabel, uaciBalance)

      whenJust itemName $ QLabel.setText itemNameLabel . toString . toUpper
      QLabel.setText balanceLabel $ toString $ balance <> " " <> unitToHtml unit

    WalletInfoSendCommandResult _commandId result -> case result of
      UiSendCommandSuccess hash -> do
        QWidget.setEnabled sendButton True
        QLineEdit.clear sendAddress
        QLineEdit.clear sendAmount
        void $ QMessageBox.information sendForm ("Success" :: String) $ toString hash
      UiSendCommandFailure err -> do
        QWidget.setEnabled sendButton True
        void $ QMessageBox.critical sendForm ("Error" :: String) $ toString err

unitToHtml :: UiCurrency -> Text
unitToHtml ADA = "<img src=':/images/ada-symbol-big-dark.png'>"
unitToHtml Lovelace = "Lovelace"
