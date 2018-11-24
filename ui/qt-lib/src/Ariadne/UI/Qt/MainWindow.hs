module Ariadne.UI.Qt.MainWindow
       ( MainWindow
       , initMainWindow
       , handleMainWindowEvent
       ) where

import Control.Lens (magnify, makeLensesWith)

import Graphics.UI.Qtah.Core.Types (QtWindowType(Dialog))

import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.Dialogs.Settings
import Ariadne.UI.Qt.Widgets.Help
import Ariadne.UI.Qt.Widgets.Logs
import Ariadne.UI.Qt.Widgets.Repl
import Ariadne.UI.Qt.Widgets.TopBar
import Ariadne.UI.Qt.Widgets.Wallet
import Ariadne.Util
import Ariadne.UX.PasswordManager

data MainWindow =
  MainWindow
    { mainWindow :: QMainWindow.QMainWindow
    , wallet :: Wallet
    , repl :: Repl
    , topBar :: TopBar
    , logs :: Logs
    , help :: Help
    , settings :: Settings
    }

makeLensesWith postfixLFields ''MainWindow

initMainWindow :: UiLangFace -> UiWalletFace -> UiHistoryFace -> IORef UiSettings -> IO MainWindow
initMainWindow langFace uiWalletFace historyFace uiSettings = do
  mainWindow <- QMainWindow.new
  QWidget.setWindowTitle mainWindow ("Ariadne" :: String)
  QWidget.resizeRaw mainWindow 960 640

  (qWallet, wallet) <- initWallet langFace uiWalletFace
  (qRepl, repl) <- initRepl langFace historyFace
  (qTopBar, topBar) <- initTopBar
  (qLogs, logs) <- initLogs
  (qHelp, help) <- initHelp langFace
  (qSettings, settings) <- initSettings uiSettings

  QWidget.setParentWithFlags qLogs mainWindow Dialog
  QWidget.setParentWithFlags qHelp mainWindow Dialog
  QWidget.setParentWithFlags qSettings mainWindow Dialog

  mainLayout <- QVBoxLayout.new
  QLayout.setContentsMarginsRaw mainLayout 0 0 0 0
  QLayout.setSpacing mainLayout 0

  QBoxLayout.addWidget mainLayout qTopBar
  QBoxLayout.addLayout mainLayout qWallet
  QBoxLayout.addWidget mainLayout qRepl

  QBoxLayout.setStretch mainLayout 0 0
  QBoxLayout.setStretch mainLayout 1 545
  QBoxLayout.setStretch mainLayout 2 266

  centralWidget <- QWidget.new
  QWidget.setLayout centralWidget mainLayout
  QMainWindow.setCentralWidget mainWindow centralWidget

  QWidget.show mainWindow

  runUI connectGlobalSignals MainWindow{..}

  return MainWindow{..}

handleMainWindowEvent
  :: UiLangFace
  -> PutPassword
  -> UiEvent
  -> UI MainWindow ()
handleMainWindowEvent langFace putPass = \case
  UiBackendEvent (UiBackendLogEvent message) ->
    magnify logsL $ displayLogMessage message
  UiBackendEvent (UiBackendStatusUpdateEvent update) ->
    magnify topBarL $ displayBlockchainInfo update
  UiCommandEvent commandId result -> do
    magnify replL $ handleReplEvent commandId result
  UiCommandResult commandId commandResult -> case commandResult of
    UiSendCommandResult result ->
      magnify walletL $ handleWalletEvent langFace putPass $
          WalletSendCommandResult commandId result
    UiCalcTotalCommandResult result ->
      magnify walletL $ handleWalletEvent langFace putPass $
          WalletCalcTotalCommandResult result
    UiNewWalletCommandResult result ->
      magnify walletL $ handleWalletEvent langFace putPass $
          WalletNewWalletCommandResult commandId result
    UiRestoreWalletCommandResult result ->
      magnify walletL $ handleWalletEvent langFace putPass $
          WalletRestoreWalletCommandResult commandId result
    UiNewAccountCommandResult result ->
      magnify walletL $ handleWalletEvent langFace putPass $
          WalletNewAccountCommandResult commandId result
    UiNewAddressCommandResult result ->
      magnify walletL $ handleWalletEvent langFace putPass $
          WalletNewAddressCommandResult commandId result
    UiChangePasswordCommandResult result ->
      magnify walletL $ handleWalletEvent langFace putPass $
          WalletPasswordChangeCommandResult commandId result
  UiWalletEvent UiWalletUpdate{..} ->
    magnify walletL $ handleWalletEvent langFace putPass $
        WalletUpdateEvent wuTrees wuSelection wuSelectionInfo
  UiPasswordEvent (UiPasswordRequest requestMode walletRef cEvent) ->
    magnify walletL $ handleWalletEvent langFace putPass $
        WalletPasswordRequest requestMode walletRef cEvent
  UiConfirmEvent (UiConfirmRequest resultVar confirmationType) -> do
    magnify walletL $ handleWalletEvent langFace putPass $
        WalletConfirmationRequest resultVar confirmationType
  UiBackendExceptionEvent (UiBackendException e) -> liftIO $ do
    msg <- QWidget.new
    void $ QMessageBox.critical msg ("Error" :: String) ("Wallet backend died with exception: " <> show e :: String)
    QCoreApplication.exit 1

connectGlobalSignals :: UI MainWindow ()
connectGlobalSignals = do
  magnify topBarL . doOnLogsAction . runUI showLogsWindow =<< view logsL
  magnify topBarL . doOnHelpAction . runUI showHelpWindow =<< view helpL
  magnify topBarL . doOnSettingsAction . runUI showSettingsWindow =<< view settingsL
  magnify topBarL . doOnReplButtonClick . runUI toggleRepl =<< view replL
