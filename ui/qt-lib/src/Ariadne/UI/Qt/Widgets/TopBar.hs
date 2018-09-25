module Ariadne.UI.Qt.Widgets.TopBar
  ( TopBar
  , initTopBar
  , doOnReplButtonClick
  , doOnLogsAction
  , doOnHelpAction
  , doOnSettingsAction
  , displayBlockchainInfo
  ) where

import Control.Lens (makeLensesWith)

import Data.Bits

import Graphics.UI.Qtah.Core.Types
  (QtCursorShape(..), alignHCenter, alignVCenter)
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Gui.QCursor as QCursor
import qualified Graphics.UI.Qtah.Gui.QIcon as QIcon
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QFrame as QFrame
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI
import Ariadne.Util

data TopBar =
  TopBar
    { syncLabel :: QPushButton.QPushButton
    , replButton :: QPushButton.QPushButton
    , helpButton :: QPushButton.QPushButton
    , logsButton :: QPushButton.QPushButton
    , settingsButton :: QPushButton.QPushButton
    , syncingIcon :: QIcon.QIcon
    , syncedIcon :: QIcon.QIcon
    }

makeLensesWith postfixLFields ''TopBar

initTopBar :: IO (QWidget.QWidget, TopBar)
initTopBar = do
  widget <- QWidget.new
  QObject.setObjectName widget ("topBar" :: String)

  topBar <- QHBoxLayout.new
  QWidget.setLayout widget topBar
  QLayout.setSpacing topBar 0
  QLayout.setContentsMarginsRaw topBar 0 0 0 0

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
  QLayout.setSpacing labelLayout 12

  yarnLabel <- QLabel.newWithText ("<img src=':/images/yarn-ic.png'>" :: String)
  QObject.setObjectName yarnLabel ("yarnLabel" :: String)
  QLabel.setAlignment yarnLabel $ alignHCenter .|. alignVCenter
  QWidget.setSizePolicyRaw yarnLabel Preferred Maximum

  ariadneLabel <- QLabel.newWithText ("<img src=':/images/ariadne.png'>" :: String)
  QObject.setObjectName ariadneLabel ("ariadneLabel" :: String)
  QLabel.setAlignment ariadneLabel $ alignHCenter .|. alignVCenter
  QWidget.setSizePolicyRaw ariadneLabel Preferred Maximum

  QBoxLayout.addStretch labelLayout
  QLayout.addWidget labelLayout yarnLabel
  QLayout.addWidget labelLayout ariadneLabel
  QBoxLayout.addStretch labelLayout
  QBoxLayout.addLayout topBar labelLayout

  navMenu <- QHBoxLayout.new
  QLayout.setContentsMarginsRaw navMenu 0 8 32 8
  QLayout.setSpacing navMenu 5
  QBoxLayout.addStretch navMenu

  syncLabel <- QPushButton.newWithText ("Syncing" :: String)
  replButton <- QPushButton.new
  helpButton <- QPushButton.newWithText ("Help" :: String)
  logsButton <- QPushButton.newWithText ("Logs" :: String)
  settingsButton <- QPushButton.newWithText ("Settings" :: String)
  pointingCursor <- QCursor.newWithCursorShape PointingHandCursor

  let createSeparator = do
        separator <- QFrame.new
        QFrame.setFrameShape separator QFrame.VLine
        void $ setProperty separator ("styleRole" :: Text) ("separator" :: Text)
        void $ setProperty separator ("orientation" :: Text) ("vertical" :: Text)
        QBoxLayout.addWidget navMenu separator

      createButton btn = do
        QBoxLayout.addWidget navMenu btn
        QWidget.setCursor btn pointingCursor
        QWidget.setSizePolicyRaw btn Preferred Maximum

  sequence_ $ intersperse createSeparator $ map createButton
    [syncLabel, replButton, helpButton, logsButton, settingsButton]

  QBoxLayout.addLayout topBar navMenu

  QBoxLayout.setStretch topBar 1 200
  QBoxLayout.setStretch topBar 2 1080

  syncingIcon <- QIcon.newWithFile (":/images/syncing.png" :: String)
  syncedIcon <- QIcon.newWithFile (":/images/synced.png" :: String)
  terminalIcon <- QIcon.newWithFile (":/images/terminal-ic.png" :: String)

  QAbstractButton.setIcon syncLabel syncingIcon
  QAbstractButton.setIcon replButton terminalIcon

  return (widget, TopBar{..})

doOnReplButtonClick :: IO () -> UI TopBar ()
doOnReplButtonClick handler = do
  replButton <- view replButtonL
  void $ liftIO $ connect_ replButton QAbstractButton.clickedSignal $ const handler

doOnLogsAction :: IO () -> UI TopBar ()
doOnLogsAction handler = do
  logsButton <- view logsButtonL
  void $ liftIO $ connect_ logsButton QAbstractButton.clickedSignal $ const handler

doOnHelpAction :: IO () -> UI TopBar ()
doOnHelpAction handler = do
  helpButton <- view helpButtonL
  void $ liftIO $ connect_ helpButton QAbstractButton.clickedSignal $ const handler

doOnSettingsAction :: IO () -> UI TopBar ()
doOnSettingsAction handler = do
  settingsButton <- view settingsButtonL
  void $ liftIO $ connect_ settingsButton QAbstractButton.clickedSignal $ const handler

displayBlockchainInfo :: UiBackendStatusUpdate -> UI TopBar ()
displayBlockchainInfo UiBackendStatusUpdate{..} = do
  syncLabel <- view syncLabelL
  liftIO $ QWidget.setToolTip syncLabel $ toString $
    "Local: <b>" <> blockchainLocal <> "</b><br>"
    <> "Network: <b>" <> blockchainNetwork <> "</b>"

  syncingIcon <- view syncingIconL
  syncedIcon <- view syncedIconL

  liftIO $ case syncProgress of
    Just progress -> do
      QAbstractButton.setText syncLabel $ toString $ "Syncing (" <> progress <> ")"
      QAbstractButton.setIcon syncLabel syncingIcon
    Nothing -> do
      QAbstractButton.setText syncLabel ("Synced" :: String)
      QAbstractButton.setIcon syncLabel syncedIcon
