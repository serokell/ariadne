module Ariadne.UI.Qt.Widgets.Dialogs.Settings
    ( Settings
    , initSettings
    , showSettingsWindow
    ) where

import Control.Lens (makeLensesWith)
import Data.Version (showVersion)
import Web.Browser (openBrowser)

import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QComboBox as QComboBox
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.Dialogs.Util
import Ariadne.UIConfig (changelogUrl, licenseUrl)
import Ariadne.Util
import Ariadne.Version (currentAriadneVersion)



data Settings =
  Settings
    { settings :: QDialog.QDialog
    , generalButton :: QPushButton.QPushButton
    , aboutButton :: QPushButton.QPushButton
    , supportButton :: QPushButton.QPushButton
    , generalWidget :: QWidget.QWidget
    , generalSettings :: GeneralSettings
    , aboutWidget :: QWidget.QWidget
    , aboutSettings :: AboutSettings
    , supportWidget :: QWidget.QWidget
    , supportSettings :: SupportSettings
    }

data SettingsMode = GeneralMode | AboutMode | SupportMode deriving (Show, Eq, Enum, Bounded)

data GeneralSettings =
  GeneralSettings
    { languageSelector :: QComboBox.QComboBox
    , countervalueSelector :: QComboBox.QComboBox
    , rateProviderSelector :: QComboBox.QComboBox
    , themeSelector :: QComboBox.QComboBox
    }

data AboutSettings =
  AboutSettings
    { releaseNotesButton :: QPushButton.QPushButton
    , licenseButton :: QPushButton.QPushButton
    }

data SupportSettings =
  SupportSettings
    { faqButton :: QPushButton.QPushButton
    , clearCacheButton :: QPushButton.QPushButton
    , resetButton :: QPushButton.QPushButton
    }

makeLensesWith postfixLFields ''Settings

initSettings :: IO (QDialog.QDialog, Settings)
initSettings = do
  settings <- QDialog.new
  QWidget.resizeRaw settings 776 363

  QObject.setObjectName settings ("settingsDialog" :: String)
  QWidget.setWindowTitle settings ("Settings" :: String)
  QWidget.adjustSize settings

  settingsLayout <- createLayout settings

  label <- QLabel.newWithText ("SETTINGS" :: String)

  addHeader settingsLayout label
  buttonsLayout <- QHBoxLayout.new
  QBoxLayout.setSpacing buttonsLayout 6
  QLayout.setContentsMarginsRaw buttonsLayout 202 0 202 0

  generalButton <- QPushButton.newWithText ("GENERAL" :: String)
  aboutButton <- QPushButton.newWithText ("ABOUT" :: String)
  supportButton <- QPushButton.newWithText ("SUPPORT" :: String)
  QBoxLayout.addStretch buttonsLayout
  for_ [generalButton, aboutButton, supportButton] $ \b -> do
    setProperty b ("styleRole" :: Text) ("settingsTopbarButton" :: Text)
    QAbstractButton.setCheckable b True
    QBoxLayout.addWidget buttonsLayout b
  QBoxLayout.addStretch buttonsLayout

  QAbstractButton.setChecked generalButton True

  QBoxLayout.addLayout settingsLayout buttonsLayout

  (generalWidget, generalSettings) <- initGeneralSettings
  QBoxLayout.addWidget settingsLayout generalWidget

  (aboutWidget, aboutSettings) <- initAboutSettings
  QBoxLayout.addWidget settingsLayout aboutWidget

  (supportWidget, supportSettings) <- initSupportSettings
  QBoxLayout.addWidget settingsLayout supportWidget

  QWidget.hide aboutWidget
  QWidget.hide supportWidget

  QWidget.setLayout settings settingsLayout
  QBoxLayout.addStretch settingsLayout
  QWidget.adjustSize settings
  let st = Settings{..}

  connect_ generalButton QAbstractButton.clickedSignal $ \_ -> showWidget st generalWidget generalButton
  connect_ aboutButton QAbstractButton.clickedSignal $ \_ -> showWidget st aboutWidget aboutButton
  connect_ supportButton QAbstractButton.clickedSignal $ \_ -> showWidget st supportWidget supportButton
  return (settings, st)

initGeneralSettings :: IO (QWidget.QWidget, GeneralSettings)
initGeneralSettings = do
  generalWidget <- QWidget.new
  generalLayout <- QVBoxLayout.new
  QLayout.setContentsMarginsRaw generalLayout 24 42 24 42
  QBoxLayout.setSpacing generalLayout 18

  languageLabel <- QLabel.newWithText ("<b>LANGUAGE</b>" :: String)
  languageSelector <- QComboBox.new
  QComboBox.addItem languageSelector ("English" :: String)
  addRow generalLayout languageLabel languageSelector
  addSeparator generalLayout

  countervalueLabel <- QLabel.newWithText ("<b>COUNTERVALUE</b>" :: String)
  countervalueSelector <- QComboBox.new
  QComboBox.addItem countervalueSelector ("USD" :: String)
  addRow generalLayout countervalueLabel countervalueSelector
  addSeparator generalLayout

  rateProviderLabel <- QLabel.newWithText ("<b>RATE PROVIDER</b>" :: String)
  rateProviderSelector <- QComboBox.new
  QComboBox.addItem rateProviderSelector ("Bittrex" :: String)
  addRow generalLayout rateProviderLabel rateProviderSelector
  addSeparator generalLayout

  themeLabel <- QLabel.newWithText ("<b>THEME</b>" :: String)
  themeSelector <- QComboBox.new
  QComboBox.addItem themeSelector ("Mint" :: String)
  addRow generalLayout themeLabel themeSelector

  for_ [languageSelector, countervalueSelector, rateProviderSelector, themeSelector] $ \w -> do
    QWidget.setSizePolicyRaw w Maximum Fixed
    QWidget.setEnabled w False

  QWidget.setLayout generalWidget generalLayout

  let gs = GeneralSettings{..}

  return (generalWidget, gs)

initAboutSettings :: IO (QWidget.QWidget, AboutSettings)
initAboutSettings = do
  aboutWidget <- QWidget.new
  aboutLayout <- QVBoxLayout.new
  QLayout.setContentsMarginsRaw aboutLayout 24 42 24 42
  QBoxLayout.setSpacing aboutLayout 18

  let currentVersion = "Ariadne " <> showVersion currentAriadneVersion

  versionLabel <- createTwoLinesLabel "VERSION" currentVersion
  releaseNotesButton <- QPushButton.newWithText("Release notes" :: String)
  void $ setProperty releaseNotesButton ("styleRole" :: Text) ("inverseButton" :: Text)
  addRow aboutLayout versionLabel releaseNotesButton
  addSeparator aboutLayout

  let license = "Ariadne is distributed under the terms of the MPL 2.0 license"

  licenseLabel <- createTwoLinesLabel "LICENSE" license
  licenseButton <- QPushButton.new
  void $ setProperty licenseButton ("styleRole" :: Text) ("linkButton" :: Text)
  addRow aboutLayout licenseLabel licenseButton

  for_ [releaseNotesButton, licenseButton] $ \w ->
    QWidget.setSizePolicyRaw w Maximum Fixed

  QWidget.setLayout aboutWidget aboutLayout

  let as = AboutSettings{..}

  connect_ releaseNotesButton QAbstractButton.clickedSignal
        $ \_ -> void $ openBrowser $ toString changelogUrl
  connect_ licenseButton QAbstractButton.clickedSignal
        $ \_ -> void $ openBrowser $ toString licenseUrl

  return (aboutWidget, as)

initSupportSettings :: IO (QWidget.QWidget, SupportSettings)
initSupportSettings = do
  supportWidget <- QWidget.new
  supportLayout <- QVBoxLayout.new
  QLayout.setContentsMarginsRaw supportLayout 24 42 24 42
  QBoxLayout.setSpacing supportLayout 18

  let faqText = "If you are experiencing issues, please see the FAQ on Ariadne \
                \website for guidance on known issues"
  faqLabel <- createTwoLinesLabel "FREQUENTLY ASKED QUESTIONS" faqText
  faqButton <- QPushButton.new
  void $ setProperty faqButton ("styleRole" :: Text) ("linkButton" :: Text)
  addRow supportLayout faqLabel faqButton
  addSeparator supportLayout

  let clearCacheText = "Clear Ariadne Wallet cache to force resynchronization with the blockchain"
  clearCacheLabel <- createTwoLinesLabel "CLEAR CACHE" clearCacheText
  clearCacheButton <- QPushButton.newWithText ("Clear cache" :: String)
  addRow supportLayout clearCacheLabel clearCacheButton
  addSeparator supportLayout

  let resetText = "Erase all Ariadne Wallet data stored on your computer, \
  \including your wallets, accounts, transaction history and settings"
  resetLabel <- createTwoLinesLabel "RESET ARIADNE" resetText
  resetButton <- QPushButton.newWithText ("Reset" :: String)
  addRow supportLayout resetLabel resetButton

  for_ [faqButton, clearCacheButton, resetButton] $ \w ->
    QWidget.setSizePolicyRaw w Maximum Fixed

  void $ setProperty clearCacheButton ("styleRole" :: Text) ("inverseButton" :: Text)
  void $ setProperty resetButton ("styleRole" :: Text) ("inverseSecondaryButton" :: Text)

  for_ [faqButton, clearCacheButton, resetButton] $ \b ->
    QWidget.setEnabled b False

  QWidget.setLayout supportWidget supportLayout

  let ss = SupportSettings{..}
  return (supportWidget, ss)

showWidget :: Settings -> QWidget.QWidget -> QPushButton.QPushButton -> IO ()
showWidget Settings{..} widget button = do
  for_ [generalButton, aboutButton, supportButton] $ \w ->
    QAbstractButton.setChecked w False
  QAbstractButton.setChecked button True
  for_ [generalWidget, aboutWidget, supportWidget] $ \w ->
    QWidget.setVisible w False
  QWidget.setVisible widget True
  QWidget.adjustSize settings

showSettingsWindow :: UI Settings ()
showSettingsWindow = do
  settings <- view settingsL

  liftIO $ QWidget.show settings

createTwoLinesLabel :: String -> String -> IO QWidget.QWidget
createTwoLinesLabel line1 line2 = do
  widget <- QWidget.new
  layout <- QVBoxLayout.new
  label1 <- QLabel.newWithText("<b>" <> line1 <> "</b>")
  void $ setProperty label1 ("styleRole" :: Text) ("dialogHeader" :: Text)
  label2 <- QLabel.newWithText(line2 :: String)
  QLabel.setWordWrap label2 True
  QWidget.setMinimumWidth label2 572
  QBoxLayout.addWidget layout label1
  QBoxLayout.addWidget layout label2
  QWidget.setLayout widget layout
  return widget
