module Ariadne.UI.Qt.Widgets.StatusBar
       ( StatusBar
       , initStatusBar
       , displayBlockchainInfo
       ) where

import Universum

import Control.Lens (makeLensesWith)
import Formatting
import IiExtras

import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QStatusBar as QStatusBar

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI

data StatusBar =
  StatusBar
    { statusBar :: QStatusBar.QStatusBar
    , syncProgressLabel :: QLabel.QLabel
    , blockchainLocalLabel :: QLabel.QLabel
    , blockchainNetworkLabel :: QLabel.QLabel
    }

makeLensesWith postfixLFields ''StatusBar

initStatusBar :: QMainWindow.QMainWindow -> IO StatusBar
initStatusBar mainWindow = do
  statusBar <- QMainWindow.statusBar mainWindow

  syncProgressLabel <- QLabel.newWithParent statusBar
  blockchainLocalLabel <- QLabel.new
  blockchainNetworkLabel <- QLabel.new

  QStatusBar.addPermanentWidget statusBar blockchainLocalLabel
  QStatusBar.addPermanentWidget statusBar blockchainNetworkLabel

  return StatusBar{..}

displayBlockchainInfo :: UiCardanoStatusUpdate -> UI StatusBar ()
displayBlockchainInfo UiCardanoStatusUpdate{..} = do
  StatusBar{..} <- ask
  liftIO $ do
    QLabel.setText blockchainLocalLabel $ toString $
      format ("Local: " % stext) blockchainLocal
    QLabel.setText blockchainNetworkLabel $ toString $
      format ("Network: " % stext) blockchainNetwork
    case syncProgress of
      Just progress -> do
        QLabel.setText syncProgressLabel $ toString $
          format ("Syncing blockchain: " % stext % ". Balances can be inaccurate." ) progress
        QStatusBar.addWidget statusBar syncProgressLabel
      Nothing -> do
        QStatusBar.removeWidget statusBar syncProgressLabel
