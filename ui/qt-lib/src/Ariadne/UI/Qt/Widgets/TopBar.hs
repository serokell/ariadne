module Ariadne.UI.Qt.Widgets.TopBar
  ( TopBar
  , initTopBar
  , doOnLogsAction
  , doOnHelpAction
  ) where

import Universum

import Control.Lens (makeLensesWith)
import IiExtras

import Data.Bits

import Graphics.UI.Qtah.Core.Types (QtCursorShape(..), alignHCenter, alignVCenter)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Event as Event
import qualified Graphics.UI.Qtah.Gui.QCursor as QCursor
import qualified Graphics.UI.Qtah.Gui.QMouseEvent as QMouseEvent
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.UI

data TopBar =
  TopBar
    { walletHdr :: QLabel.QLabel
    , helpHdr :: QLabel.QLabel
    , logsHdr :: QLabel.QLabel
    , settingsHdr :: QLabel.QLabel
    }

makeLensesWith postfixLFields ''TopBar

initTopBar :: IO (QHBoxLayout.QHBoxLayout, TopBar)
initTopBar = do
  topBar <- QHBoxLayout.new
  QObject.setObjectName topBar ("topBar" :: String)

  -- Add a widget with a fixed height of 48 px.
  -- It will force the top bar to always be 48 px high,
  -- provided that all other widgets have `Maximum` vertical size policy.
  vertSpacer <- QWidget.new
  QObject.setObjectName vertSpacer ("vertSpacer" :: String)
  QWidget.setMaximumHeight vertSpacer 48
  QWidget.setMinimumHeight vertSpacer 48
  QWidget.setSizePolicyRaw vertSpacer Preferred Maximum
  QLayout.addWidget topBar vertSpacer

  labelLayout <- QHBoxLayout.new

  ariadneLabel <- QLabel.newWithText ("ARIADNE" :: String)
  QObject.setObjectName ariadneLabel ("ariadneLabel" :: String)
  QLabel.setAlignment ariadneLabel $ alignHCenter .|. alignVCenter
  QWidget.setSizePolicyRaw ariadneLabel Preferred Maximum

  QLayout.addWidget labelLayout ariadneLabel
  QBoxLayout.addLayout topBar labelLayout

  navMenu <- QHBoxLayout.new
  QBoxLayout.addStretchOf navMenu 277

  walletHdr <- QLabel.newWithText ("Wallet" :: String)
  helpHdr <- QLabel.newWithText ("Help" :: String)
  logsHdr <- QLabel.newWithText ("Logs" :: String)
  settingsHdr <- QLabel.newWithText ("Settings" :: String)

  pointingCursor <- QCursor.newWithCursorShape PointingHandCursor

  forM_ (zip [1..] [walletHdr, helpHdr, logsHdr, settingsHdr]) $ \(i, lbl) -> do
    QBoxLayout.addWidget navMenu lbl
    QBoxLayout.setStretch navMenu i 82
    QWidget.setCursor lbl pointingCursor
    QWidget.setSizePolicyRaw lbl Preferred Maximum

  QBoxLayout.addStretchOf navMenu 477

  QBoxLayout.addLayout topBar navMenu

  QBoxLayout.setStretch topBar 1 200
  QBoxLayout.setStretch topBar 2 1080

  return (topBar, TopBar{..})

doOnLogsAction :: IO () -> UI TopBar ()
doOnLogsAction handler = do
  logsHdr <- view logsHdrL
  void $ liftIO $ Event.onEvent logsHdr $
    \(_ :: QMouseEvent.QMouseEvent) -> handler >> return True

doOnHelpAction :: IO () -> UI TopBar ()
doOnHelpAction handler = do
  helpHdr <- view helpHdrL
  void $ liftIO $ Event.onEvent helpHdr $
    \(_ :: QMouseEvent.QMouseEvent) -> handler >> return True
