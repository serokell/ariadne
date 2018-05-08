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
    , tipHeaderHashLabel :: QLabel.QLabel
    , tipSlotLabel :: QLabel.QLabel
    , currentSlotLabel :: QLabel.QLabel
    }

makeLensesWith postfixLFields ''StatusBar

initStatusBar :: QMainWindow.QMainWindow -> IO StatusBar
initStatusBar mainWindow = do
  statusBar <- QMainWindow.statusBar mainWindow

  tipHeaderHashLabel <- QLabel.new
  tipSlotLabel <- QLabel.new
  currentSlotLabel <- QLabel.new

  QStatusBar.addPermanentWidget statusBar tipHeaderHashLabel
  QStatusBar.addPermanentWidget statusBar tipSlotLabel
  QStatusBar.addPermanentWidget statusBar currentSlotLabel

  return StatusBar{..}

displayBlockchainInfo :: UiCardanoStatusUpdate -> UI StatusBar ()
displayBlockchainInfo UiCardanoStatusUpdate{..} = do
  tipHeaderHashLabel <- view tipHeaderHashLabelL
  tipSlotLabel <- view tipSlotLabelL
  currentSlotLabel <- view currentSlotLabelL

  liftIO $ QLabel.setText tipHeaderHashLabel $ toString $
    format ("Tip Hash: " % stext) tipHeaderHash
  liftIO $ QLabel.setText tipSlotLabel $ toString $
    format ("Tip slot: " % stext) tipSlot
  liftIO $ QLabel.setText currentSlotLabel $ toString $
    format ("Current slot: " % stext) currentSlot
