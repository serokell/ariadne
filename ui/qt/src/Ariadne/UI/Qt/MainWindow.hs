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
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI

import Ariadne.UI.Qt.Widgets.Help
import Ariadne.UI.Qt.Widgets.Logs
import Ariadne.UI.Qt.Widgets.MenuBar
import Ariadne.UI.Qt.Widgets.Repl
import Ariadne.UI.Qt.Widgets.StatusBar
import Ariadne.UI.Qt.Widgets.Wallet

data MainWindow =
  MainWindow
    { mainWindow :: QMainWindow.QMainWindow
    , wallet :: Wallet
    , repl :: Repl
    , statusBar :: StatusBar
    , menuBar :: MenuBar
    , logs :: Logs
    , help :: Help
    }

makeLensesWith postfixLFields ''MainWindow

initMainWindow :: UiLangFace -> UiHistoryFace -> IO MainWindow
initMainWindow langFace historyFace = do
  mainWindow <- QMainWindow.new
  QWidget.setWindowTitle mainWindow ("Ariadne" :: String)

  (qWallet, wallet) <- initWallet langFace
  (qRepl, repl) <- initRepl langFace historyFace
  (qMenuBar, menuBar) <- initMenuBar
  (qLogs, logs) <- initLogs
  (qHelp, help) <- initHelp langFace

  QMainWindow.setMenuBar mainWindow qMenuBar
  QWidget.setParentWithFlags qLogs mainWindow Dialog
  QWidget.setParentWithFlags qHelp mainWindow Dialog

  mainLayout <- QVBoxLayout.new
  QBoxLayout.addWidget mainLayout qWallet
  QBoxLayout.addLayout mainLayout qRepl
  QBoxLayout.setStretch mainLayout 0 2
  QBoxLayout.setStretch mainLayout 1 1

  centralWidget <- QWidget.new
  QWidget.setLayout centralWidget mainLayout
  QMainWindow.setCentralWidget mainWindow centralWidget

  statusBar <- initStatusBar mainWindow

  QWidget.show mainWindow

  runUI connectGlobalSignals MainWindow{..}

  return MainWindow{..}

handleMainWindowEvent :: UiLangFace -> UiEvent -> UI MainWindow ()
handleMainWindowEvent langFace = \case
  UiCardanoEvent (UiCardanoLogEvent message) ->
    magnify logsL $ displayLogMessage message
  UiCardanoEvent (UiCardanoStatusUpdateEvent update) ->
    magnify statusBarL $ displayBlockchainInfo update
  UiCommandEvent commandId result -> do
    magnify replL $ handleReplEvent commandId result
  UiCommandResult commandId commandResult -> case commandResult of
    UiBalanceCommandResult result ->
      magnify walletL $ handleWalletEvent langFace $ WalletBalanceCommandResult commandId result
    UiSendCommandResult result ->
      magnify walletL $ handleWalletEvent langFace $ WalletSendCommandResult commandId result
    UiNewWalletCommandResult result ->
      magnify walletL $ handleWalletEvent langFace $ WalletNewWalletCommandResult commandId result
    UiNewAccountCommandResult result ->
      magnify walletL $ handleWalletEvent langFace $ WalletNewAccountCommandResult commandId result
    UiNewAddressCommandResult result ->
      magnify walletL $ handleWalletEvent langFace $ WalletNewAddressCommandResult commandId result
  UiWalletEvent UiWalletUpdate{..} ->
    magnify walletL $ handleWalletEvent langFace $ WalletUpdateEvent wuTrees wuSelection

connectGlobalSignals :: UI MainWindow ()
connectGlobalSignals = do
  magnify menuBarL . doOnLogsAction . runUI showLogsWindow =<< view logsL
  magnify menuBarL . doOnHelpAction . runUI showHelpWindow =<< view helpL
