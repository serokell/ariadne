module Ariadne.UI.Qt.MainWindow
    ( MainWindow
    , initMainWindow
    , handleMainWindowEvent
    ) where

import Universum

import Control.Lens (makeLensesWith, magnify)
import IiExtras

import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI

import Ariadne.UI.Qt.Widgets.Repl
import Ariadne.UI.Qt.Widgets.StatusBar
import Ariadne.UI.Qt.Widgets.Wallet

data MainWindow =
  MainWindow
    { mainWindow :: QMainWindow.QMainWindow
    , wallet :: Wallet
    , repl :: Repl
    , statusBar :: StatusBar
    }

makeLensesWith postfixLFields ''MainWindow

initMainWindow :: UiLangFace -> IO MainWindow
initMainWindow langFace = do
  mainWindow <- QMainWindow.new
  QWidget.setWindowTitle mainWindow ("Ariadne" :: String)

  (qWallet, wallet) <- initWallet langFace
  (qRepl, repl) <- initRepl langFace

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

  return MainWindow{..}

handleMainWindowEvent :: UiEvent -> UI MainWindow ()
handleMainWindowEvent = \case
  UiCardanoEvent (UiCardanoStatusUpdateEvent update) ->
    magnify statusBarL $ displayBlockchainInfo update
  UiCommandEvent UiCommandId{..} (UiCommandSuccess doc) ->
    magnify replL $ displayCommandInfo "" doc
  UiCommandEvent UiCommandId{..} (UiCommandFailure doc) ->
    magnify replL $ displayCommandInfo "" doc
  UiWalletEvent UiWalletUpdate{..} ->
    magnify walletL $ handleWalletEvent $ WalletUpdateEvent wuTrees wuSelection
  _ -> return ()
