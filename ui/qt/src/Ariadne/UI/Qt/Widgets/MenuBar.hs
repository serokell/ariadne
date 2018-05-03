module Ariadne.UI.Qt.Widgets.MenuBar
  ( MenuBar
  , initMenuBar
  , doOnLogsAction
  ) where

import Universum

import Control.Lens (makeLensesWith)
import IiExtras

import Graphics.UI.Qtah.Signal (connect_)
import qualified Graphics.UI.Qtah.Widgets.QAction as QAction
import qualified Graphics.UI.Qtah.Widgets.QMenu as QMenu
import qualified Graphics.UI.Qtah.Widgets.QMenuBar as QMenuBar

import Ariadne.UI.Qt.UI

data MenuBar =
  MenuBar
    { menuBar :: QMenuBar.QMenuBar
    , logsAction :: QAction.QAction
    }

makeLensesWith postfixLFields ''MenuBar

initMenuBar :: IO (QMenuBar.QMenuBar, MenuBar)
initMenuBar = do
  -- Creating parentless menubar leads to global menubar on Mac -- probably what we want
  menuBar <- QMenuBar.new

  logsMenu <- QMenuBar.addNewMenu menuBar ("Help" :: String)

  logsAction <- QMenu.addNewAction logsMenu ("Logs" :: String)

  return (menuBar, MenuBar{..})

doOnLogsAction :: IO () -> UI MenuBar ()
doOnLogsAction handler = do
  logsAction <- view logsActionL
  liftIO $ connect_ logsAction QAction.triggeredSignal $ const handler
