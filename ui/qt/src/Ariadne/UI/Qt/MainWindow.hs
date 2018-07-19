module Ariadne.UI.Qt.MainWindow
    ( MainWindow
    , initMainWindow
    , handleMainWindowEvent
    ) where

import Universum

import Control.Lens (magnify, makeLensesWith)
import IiExtras

import Graphics.UI.Qtah.Core.Types (QtWindowType(Dialog))

import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI

import Ariadne.UI.Qt.Widgets.Help
import Ariadne.UI.Qt.Widgets.Logs
import Ariadne.UI.Qt.Widgets.TopBar
import Ariadne.UI.Qt.Widgets.Repl
import Ariadne.UI.Qt.Widgets.Wallet

data MainWindow =
  MainWindow
    { mainWindow :: QMainWindow.QMainWindow
    , wallet :: Wallet
    , repl :: Repl
    , topBar :: TopBar
    , logs :: Logs
    , help :: Help
    }

makeLensesWith postfixLFields ''MainWindow

initMainWindow :: UiLangFace -> UiHistoryFace -> IO MainWindow
initMainWindow langFace historyFace = do
  mainWindow <- QMainWindow.new
  QWidget.setWindowTitle mainWindow ("Ariadne" :: String)
  QWidget.resizeRaw mainWindow 960 640

  (qWallet, wallet) <- initWallet langFace
  (qRepl, repl) <- initRepl langFace historyFace
  (qTopBar, topBar) <- initTopBar
  (qLogs, logs) <- initLogs
  (qHelp, help) <- initHelp langFace

  QWidget.setParentWithFlags qLogs mainWindow Dialog
  QWidget.setParentWithFlags qHelp mainWindow Dialog

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

handleMainWindowEvent :: UiLangFace -> UiEvent -> UI MainWindow ()
handleMainWindowEvent langFace = \case
  UiCardanoEvent (UiCardanoLogEvent message) ->
    magnify logsL $ displayLogMessage message
  UiCardanoEvent (UiCardanoStatusUpdateEvent update) ->
    magnify topBarL $ displayBlockchainInfo update
  UiCommandEvent commandId result -> do
    magnify replL $ handleReplEvent commandId result
  UiCommandResult commandId commandResult -> case commandResult of
    UiSendCommandResult result ->
      magnify walletL $ handleWalletEvent langFace $ WalletSendCommandResult commandId result
    UiNewWalletCommandResult result ->
      magnify walletL $ handleWalletEvent langFace $ WalletNewWalletCommandResult commandId result
    UiNewAccountCommandResult result ->
      magnify walletL $ handleWalletEvent langFace $ WalletNewAccountCommandResult commandId result
    UiNewAddressCommandResult result ->
      magnify walletL $ handleWalletEvent langFace $ WalletNewAddressCommandResult commandId result
  UiWalletEvent UiWalletUpdate{..} ->
    magnify walletL $ handleWalletEvent langFace $ WalletUpdateEvent wuTrees wuSelection wuSelectionInfo

connectGlobalSignals :: UI MainWindow ()
connectGlobalSignals = do
  magnify topBarL . doOnLogsAction . runUI showLogsWindow =<< view logsL
  magnify topBarL . doOnHelpAction . runUI showHelpWindow =<< view helpL
  magnify topBarL . doOnReplButtonClick . runUI toggleRepl =<< view replL
