module Ariadne.UI.Qt.Widgets.WalletInfo
       ( WalletInfo
       , initWalletInfo

       , WalletInfoEvent(..)
       , handleWalletInfoEvent
       ) where

import Data.Text (toUpper)

import Control.Lens (makeLensesWith)
import Graphics.UI.Qtah.Signal (connect_)

import Graphics.UI.Qtah.Core.Types (QtCursorShape(..), QtMouseButton(..))
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Core.QItemSelectionModel as QItemSelectionModel
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Event as Event
import qualified Graphics.UI.Qtah.Gui.QClipboard as QClipboard
import qualified Graphics.UI.Qtah.Gui.QCursor as QCursor
import qualified Graphics.UI.Qtah.Gui.QIcon as QIcon
import qualified Graphics.UI.Qtah.Gui.QMouseEvent as QMouseEvent
import qualified Graphics.UI.Qtah.Gui.QStandardItemModel as QStandardItemModel
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QInputDialog as QInputDialog
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Util
import Ariadne.UI.Qt.Widgets.Dialogs.AccountSettings
import Ariadne.UI.Qt.Widgets.Dialogs.ConfirmSend
import Ariadne.UI.Qt.Widgets.Dialogs.Delete
import Ariadne.UI.Qt.Widgets.Dialogs.Request
import Ariadne.UI.Qt.Widgets.Dialogs.Send
import Ariadne.Util

data CurrentItem = WIWallet Word [UiAccountInfo] | WIAccount Word UiAccountInfo

data WalletInfo =
  WalletInfo
    { walletInfo :: QWidget.QWidget
    , itemNameLabel :: QLabel.QLabel
    , balanceLabel :: QLabel.QLabel
    , balanceCommandId :: IORef (Maybe UiCommandId)
    , itemModel :: QStandardItemModel.QStandardItemModel
    , selectionModel :: QItemSelectionModel.QItemSelectionModel
    , createAccountButton :: QPushButton.QPushButton
    , changePasswordButton :: QPushButton.QPushButton
    , accountSettingsButton :: QPushButton.QPushButton
    , currentItem :: IORef (Maybe CurrentItem)
    , currentItemName :: IORef Text
    , deleteItemButton :: QPushButton.QPushButton
    , sendButton :: QPushButton.QPushButton
    , requestButton :: QPushButton.QPushButton
    , requestDialog :: IORef (Maybe Request)
    , sendConfirmDialog :: IORef (Maybe ConfirmSend)
    , send :: IORef (Maybe Send)
    }

makeLensesWith postfixLFields ''WalletInfo

initWalletInfo
  :: UiLangFace
  -> UiWalletFace
  -> QStandardItemModel.QStandardItemModel
  -> QItemSelectionModel.QItemSelectionModel
  -> IO (QWidget.QWidget, WalletInfo)
initWalletInfo langFace uiWalletFace itemModel selectionModel = do
  infoLayout <- QVBoxLayout.new
  QLayout.setContentsMarginsRaw infoLayout 0 0 0 0

  header <- QWidget.new
  QObject.setObjectName header ("walletHeader" :: String)
  headerLayout <- QHBoxLayout.new
  QWidget.setLayout header headerLayout

  QLayout.setContentsMarginsRaw headerLayout 36 12 36 12
  QLayout.setSpacing headerLayout 36

  itemNameLabel <- QLabel.newWithText $ toString selectSomethingText
  QObject.setObjectName itemNameLabel ("itemNameLabel" :: String)
  QLayout.addWidget headerLayout itemNameLabel
  QBoxLayout.addStretch headerLayout

  pointingCursor <- QCursor.newWithCursorShape PointingHandCursor
  createAccountButton <- QPushButton.newWithText (" Create account" :: String)
  createAccountIcon <- QIcon.newWithFile (":/images/add-ic.png" :: String)
  QAbstractButton.setIcon createAccountButton createAccountIcon
  QWidget.setCursor createAccountButton pointingCursor
  QWidget.hide createAccountButton -- will be shown only if applicable
  QLayout.addWidget headerLayout createAccountButton

  changePasswordButton <- QPushButton.newWithText ("Change password" :: String)
  QWidget.setCursor changePasswordButton pointingCursor
  QWidget.hide changePasswordButton
  QLayout.addWidget headerLayout changePasswordButton

  accountSettingsButton <- QPushButton.newWithText ("Account settings" :: String)
  settingsIcon <- QIcon.newWithFile (":/images/settings-ic.png" :: String)
  QAbstractButton.setIcon accountSettingsButton settingsIcon
  QLayout.addWidget headerLayout accountSettingsButton
  QWidget.setCursor accountSettingsButton pointingCursor
  QWidget.hide accountSettingsButton -- will be shown only if applicable

  deleteItemButton <- QPushButton.newWithText ("Delete" :: String)
  QLayout.addWidget headerLayout deleteItemButton
  QWidget.setCursor deleteItemButton pointingCursor
  QWidget.hide deleteItemButton -- will be shown once something is selected

  QBoxLayout.addWidget infoLayout header

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
  sendButton <- QPushButton.newWithText (" SEND" :: String)
  requestButton <- QPushButton.newWithText (" REQUEST" :: String)
  setProperty requestButton ("styleRole" :: Text) ("secondaryButton" :: Text)

  sendIcon <- QIcon.newWithFile (":/images/send-ic.png" :: String)
  receiveIcon <- QIcon.newWithFile (":/images/receive-ic.png" :: String)

  QAbstractButton.setIcon sendButton sendIcon
  QAbstractButton.setIcon requestButton receiveIcon

  -- Buttons will be shown once something is selected
  QWidget.hide sendButton
  QWidget.hide requestButton

  QBoxLayout.addStretch accountControls
  QLayout.addWidget accountControls sendButton
  QLayout.addWidget accountControls requestButton
  QBoxLayout.addStretch accountControls

  QBoxLayout.addLayout balanceLayout accountControls
  QBoxLayout.setStretch balanceLayout 0 1
  QBoxLayout.setStretch balanceLayout 1 0

  QBoxLayout.addStretch infoLayout
  walletInfo <- QWidget.new
  QWidget.setLayout walletInfo infoLayout

  currentItem <- newIORef Nothing
  currentItemName <- newIORef ""
  requestDialog <- newIORef Nothing
  sendConfirmDialog <- newIORef Nothing
  send <- newIORef Nothing

  connect_ createAccountButton QAbstractButton.clickedSignal $
    addAccountClicked langFace WalletInfo{..}
  connect_ changePasswordButton QAbstractButton.clickedSignal $
    changePasswordClicked langFace WalletInfo{..}
  connect_ accountSettingsButton QAbstractButton.clickedSignal $
    accountSettingsClicked langFace WalletInfo{..}
  connect_ deleteItemButton QAbstractButton.clickedSignal $
    deleteItemClicked langFace WalletInfo{..}
  connect_ requestButton QAbstractButton.clickedSignal $
    requestButtonClicked langFace WalletInfo{..}
  connect_ sendButton QAbstractButton.clickedSignal $
    sendButtonClicked langFace uiWalletFace WalletInfo{..}

  clipboard <- QApplication.clipboard

  void $ Event.onEvent itemNameLabel $ \(ev :: QMouseEvent.QMouseEvent) -> do
    evType <- QEvent.eventType ev
    button <- QMouseEvent.button ev
    when (evType == QEvent.MouseButtonRelease && button == RightButton) $ do
      toolTip <- QWidget.toolTip itemNameLabel
      unless (null toolTip) $
        QClipboard.setText clipboard toolTip
    return False

  return (walletInfo, WalletInfo{..})

data WalletInfoEvent
  = WalletInfoSelectionChange UiSelectionInfo
  | WalletInfoDeselect
  | WalletInfoSendCommandResult UiCommandId UiSendCommandResult
  | WalletInfoCalcTotalCommandResult UiCalcTotalCommandResult
  | WalletInfoNewAccountCommandResult UiCommandId UiNewAccountCommandResult
  | WalletInfoNewAddressCommandResult UiCommandId UiNewAddressCommandResult
  | WalletInfoChangePasswordCommandResult UiCommandId UiChangePasswordCommandResult
  | WalletInfoConfirmRemove (MVar Bool) UiDeletingItem
  | WalletInfoConfirmSend (MVar Bool) [UiConfirmSendInfo]

handleWalletInfoEvent
  :: UiLangFace
  -> WalletInfoEvent
  -> UI WalletInfo ()
handleWalletInfoEvent UiLangFace{..} ev = do
  WalletInfo{..} <- ask
  lift $ case ev of
    WalletInfoSelectionChange selectionInfo -> do
      let (itemName, (balance, unit), item, tooltip) =
            case selectionInfo of
              UiSelectionWallet UiWalletInfo{..} ->
                (uwiLabel, uwiBalance, WIWallet uwiWalletIdx uwiAccounts, uwiWalletId)
              UiSelectionAccount uaci@UiAccountInfo{..} ->
                (uaciLabel, uaciBalance, WIAccount uaciWalletIdx uaci, "")

      QWidget.setToolTip itemNameLabel $ toString tooltip

      QLabel.setText itemNameLabel . toString . toUpper . fromMaybe "" $ itemName
      QLabel.setText balanceLabel $ toString $ formatBalance balance unit
      case item of
        WIWallet _ accounts -> do
          QWidget.setVisible createAccountButton True
          QWidget.setVisible changePasswordButton True
          QWidget.setVisible accountSettingsButton False
          QWidget.setVisible requestButton $ not $ null accounts
          QWidget.setVisible sendButton $ not $ null accounts
        WIAccount {} -> do
          QWidget.setVisible createAccountButton False
          QWidget.setVisible changePasswordButton False
          QWidget.setVisible accountSettingsButton True
          QWidget.setVisible requestButton True
          QWidget.setVisible sendButton True
      QWidget.setVisible deleteItemButton True

      writeIORef currentItem $ Just item
      -- `itemNameLabel` stores capitalized text, but we need the original for delete dialog
      writeIORef currentItemName $ fromMaybe "" itemName

    WalletInfoDeselect -> do
      QLabel.setText itemNameLabel $ toString selectSomethingText
      QLabel.setText balanceLabel ("" :: String)
      QWidget.hide createAccountButton
      QWidget.hide requestButton
      QWidget.hide deleteItemButton

      writeIORef currentItem Nothing
      writeIORef currentItemName ""

    WalletInfoSendCommandResult _commandId result -> do
      whenJustM (readIORef sendConfirmDialog) $ \dialog -> closeConfirmSend dialog
      void $ case result of
        UiSendCommandSuccess hash -> do
          void $ QMessageBox.information walletInfo ("Success" :: String) $ toString hash
        UiSendCommandFailure err -> do
          void $ QMessageBox.critical walletInfo ("Error" :: String) $ toString err
    WalletInfoCalcTotalCommandResult result -> case result of
        UiCalcTotalCommandSuccess (amount, unit) -> do
          whenJustM (readIORef send) $ \dialog -> changeTotalAmount dialog amount unit
        UiCalcTotalCommandFailure err -> do
          whenJustM (readIORef send) $ \dialog -> showInvalidArgumentsError dialog err
    WalletInfoNewAccountCommandResult _commandId result -> case result of
      UiNewAccountCommandSuccess -> do
        void $ QMessageBox.information walletInfo ("Success" :: String) ("Account created" :: String)
      UiNewAccountCommandFailure err -> do
        void $ QMessageBox.critical walletInfo ("Error" :: String) $ toString err

    WalletInfoNewAddressCommandResult _commandId result -> case result of
      UiNewAddressCommandSuccess wIdx aIdx address -> do
        whenJustM (readIORef requestDialog) $ \req -> addNewAddress req wIdx aIdx address
      UiNewAddressCommandFailure err ->
        void $ QMessageBox.critical walletInfo ("Error" :: String) $ toString err

    WalletInfoChangePasswordCommandResult _commandId result -> case result of
      UiChangePasswordCommandSuсcess -> do
        void $ QMessageBox.information walletInfo ("Success" :: String)
          ("Password was successfully changed" :: String)
      UiChangePasswordCommandFailure err ->
        void $ QMessageBox.critical walletInfo ("Error" :: String) $ toString err

    WalletInfoConfirmRemove resultVar delItemType ->
      liftIO $ runDelete delItemType >>= \case
        DoDelete -> putMVar resultVar True
        Cancel -> putMVar resultVar False

    WalletInfoConfirmSend resultVar sendInfo -> do
      cs <- runConfirmSend sendInfo resultVar
      writeIORef sendConfirmDialog $ Just cs

addAccountClicked :: UiLangFace -> WalletInfo -> Bool -> IO ()
addAccountClicked UiLangFace{..} WalletInfo{..} _checked = do
  name <- toText <$> QInputDialog.getText walletInfo ("New account" :: String) ("Account name" :: String)
  unless (null name) $ void $ langPutUiCommand $ UiNewAccount name

accountSettingsClicked :: UiLangFace -> WalletInfo -> Bool -> IO ()
accountSettingsClicked langFace@UiLangFace{..} wi@WalletInfo{..} _checked =
  whenJustM (readIORef currentItem) $ \_ -> do
    currentName <- readIORef currentItemName

    let
      renameHandler :: RenameHandler
      renameHandler newName = void $ langPutUiCommand $ UiRename newName

      deleteHandler :: DeleteHandler
      deleteHandler = deleteItemClicked langFace wi False

    runAccountSettings currentName renameHandler deleteHandler

deleteItemClicked :: UiLangFace -> WalletInfo -> Bool -> IO ()
deleteItemClicked UiLangFace{..} WalletInfo{..} _checked =
  whenJustM (readIORef currentItem) $ \_ ->
    void $ langPutUiCommand UiRemoveCurrentItem

requestButtonClicked :: UiLangFace -> WalletInfo -> Bool -> IO ()
requestButtonClicked langFace WalletInfo{..} _checked = do
  req <- readIORef requestDialog
  whenNothing_ req $
    whenJustM (readIORef currentItem) $ \item -> do
      let
        accounts = case item of
          WIWallet _ uacis -> RequestAccountsMulti uacis
          WIAccount _ uaci -> RequestAccountsSingle uaci
      req' <- startRequest langFace onClosed accounts
      writeIORef requestDialog $ Just req'
  where onClosed = writeIORef requestDialog Nothing

sendButtonClicked :: UiLangFace -> UiWalletFace -> WalletInfo -> Bool -> IO ()
sendButtonClicked uiLangFace@UiLangFace{..} uiWalletFace WalletInfo{..} _checked = whenJustM (readIORef currentItem) $ \item -> do
  let
    (wIdx, inputs) = case item of
      WIWallet wIdx' uacis -> (wIdx', SendInputsMulti uacis)
      WIAccount wIdx' UiAccountInfo{..} -> (wIdx', SendInputsSingle $ head uaciPath)
  sendDialog <- initSend uiLangFace uiWalletFace inputs
  writeIORef send $ Just sendDialog
  result <- runSend sendDialog
  case result of
    SendCancel -> pass
    SendSuccess SendOptions{..} -> do
      let sendOutputs = zipWith UiSendOutput soAmounts soAddresses
      langPutUiCommand (UiSend $ UiSendArgs wIdx soAccounts sendOutputs) >>= \case
        Left err -> void $ QMessageBox.critical walletInfo ("Error" :: String) $ toString err
        Right _ -> pass

changePasswordClicked :: UiLangFace -> WalletInfo -> Bool -> IO ()
changePasswordClicked UiLangFace{..} WalletInfo{..} _checked = do
  void $ langPutUiCommand (UiChangePassword)

selectSomethingText :: Text
selectSomethingText = "Select something..."
