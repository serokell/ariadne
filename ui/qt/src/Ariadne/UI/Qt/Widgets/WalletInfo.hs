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
import qualified Graphics.UI.Qtah.Widgets.QDialogButtonBox as QDialogButtonBox
import qualified Graphics.UI.Qtah.Widgets.QFormLayout as QFormLayout
import qualified Graphics.UI.Qtah.Widgets.QGroupBox as QGroupBox
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI

data WalletInfo =
  WalletInfo
    { sendForm :: QGroupBox.QGroupBox
    , sendAddress :: QLineEdit.QLineEdit
    , sendAmount :: QLineEdit.QLineEdit
    , sendButton :: QPushButton.QPushButton
    , sendCommandId :: IORef (Maybe UiCommandId)
    , itemModel :: QStandardItemModel.QStandardItemModel
    , selectionModel :: QItemSelectionModel.QItemSelectionModel
    }

makeLensesWith postfixLFields ''WalletInfo

initWalletInfo
  :: UiLangFace
  -> QStandardItemModel.QStandardItemModel
  -> QItemSelectionModel.QItemSelectionModel
  -> IO (QGroupBox.QGroupBox, WalletInfo)
initWalletInfo langFace itemModel selectionModel = do
  sendCommandId <- newIORef Nothing

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

  liftIO $ connect_ sendButton QAbstractButton.clickedSignal $
    \checked -> runUI (sendClicked langFace checked) WalletInfo{..}

  return (sendForm, WalletInfo{..})

sendClicked :: UiLangFace -> Bool -> UI WalletInfo ()
sendClicked UiLangFace{..} _checked = do
  WalletInfo{..} <- ask
  lift $ do
    address <- toText <$> QLineEdit.text sendAddress
    amount <- toText <$> QLineEdit.text sendAmount
    case langMkExpr $ UiSend address amount of
      Left err -> do
        void $ QMessageBox.critical sendForm ("Error" :: String) $ toString err
      Right expr -> do
        QWidget.setEnabled sendButton False
        writeIORef sendCommandId . Just =<< langPutCommand expr

data WalletInfoEvent
  = WalletInfoCommandSuccess UiCommandId
  | WalletInfoCommandFailure UiCommandId

handleWalletInfoEvent
  :: WalletInfoEvent
  -> UI WalletInfo ()
handleWalletInfoEvent ev = do
  WalletInfo{..} <- ask
  lift $ case ev of
    WalletInfoCommandSuccess commandId -> do
      whenM (isCurrentCmd sendCommandId commandId) $ do
        QWidget.setEnabled sendButton True
        QLineEdit.clear sendAddress
        QLineEdit.clear sendAmount
        void $ QMessageBox.information sendForm ("Success" :: String) ("Transaction successfully sent" :: String)
    WalletInfoCommandFailure commandId -> do
      whenM (isCurrentCmd sendCommandId commandId) $ do
        QWidget.setEnabled sendButton True
        void $ QMessageBox.critical sendForm ("Failure" :: String) ("Transaction failed" :: String)
  where
    isCurrentCmd ref actual = atomicModifyIORef ref $
      \contents -> if
        | Just expected <- contents,
          expected == actual -> (Nothing, True)
        | otherwise -> (contents, False)
