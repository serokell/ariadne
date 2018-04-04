module Ariadne.UI.Qt
  ( UiFace(..)
  , createAriadneUI
  ) where

import Universum

import Ariadne.UI.Qt.Face (UiEvent(..), UiFace(..), UiLangFace)
import Brick.BChan (BChan, newBChan, writeBChan)

import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

type UiAction = UiLangFace -> IO ()

createAriadneUI :: IO (UiFace, UiAction)
createAriadneUI = do
  eventChan <- mkEventChan
  return (mkUiFace eventChan, runUI eventChan)

runUI :: BChan UiEvent -> UiLangFace -> IO ()
runUI _ _ =
  withScopedPtr (getArgs >>= QApplication.new) $ \_ -> do
    window <- QWidget.new
    QWidget.resizeRaw window 500 350
    QWidget.setWindowTitle window ("ariadne-qt" :: String)
    QWidget.show window
    QCoreApplication.exec

-- Create a channel for application events that aren't user input. This channel
-- is bounded to avoid infinite accumulation of events, but the bound is
-- somewhat arbitrary.
mkEventChan :: IO (BChan UiEvent)
mkEventChan = newBChan 100

-- Create the API for interacting with the UI thread.
mkUiFace :: BChan UiEvent -> UiFace
mkUiFace eventChan =
  UiFace
    {
      putUiEvent = writeBChan eventChan
    }
