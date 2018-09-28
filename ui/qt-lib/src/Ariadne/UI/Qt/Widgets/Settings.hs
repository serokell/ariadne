module Ariadne.UI.Qt.Widgets.Settings
    ( Settings
    , initSettings
    , showSettingsWindow
    ) where

import Control.Lens (makeLensesWith)
import Data.Bits
import Web.Browser (openBrowser)

import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Core.Types (alignHCenter, alignVCenter)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QComboBox as QComboBox

import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.Dialogs.Util
import Ariadne.Util

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
    {  languageSelector :: QComboBox.QComboBox
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
    , reportButton :: QPushButton.QPushButton
    , downloadLogsButton :: QPushButton.QPushButton
    , enableRepots :: QCheckBox.QCheckBox
    , hasAnalytics :: QCheckBox.QCheckBox
    , resetButton :: QPushButton.QPushButton
    }

makeLensesWith postfixLFields ''Settings

initSettings :: IO (QDialog.QDialog, Settings)
initSettings = do
  settings <- QDialog.new
  QWidget.resizeRaw settings 776 363

  QObject.setObjectName settings ("settingsDialog" :: String)

  settingsLayout <- QVBoxLayout.new
  QLayout.setContentsMarginsRaw settingsLayout 0 0 0 0
  QLayout.setSpacing settingsLayout 0

  labelLayout <- QHBoxLayout.new
  QLayout.setContentsMarginsRaw labelLayout 0 24 0 24
  label <- QLabel.newWithText ("SETTINGS" :: String)
  void $ setProperty label ("styleRole" :: Text) ("dialogHeader" :: Text)
  QBoxLayout.addWidget labelLayout label
  void $ QLayout.setWidgetAlignment labelLayout label $ alignHCenter .|. alignVCenter

  QBoxLayout.addLayout settingsLayout labelLayout

  buttonsLayout <-QHBoxLayout.new
  QLayout.setContentsMarginsRaw buttonsLayout 202 0 202 0
  void $ QLayout.setWidgetAlignment buttonsLayout label $ alignHCenter .|. alignVCenter
  generalButton <- QPushButton.newWithText ("GENERAL" :: String)
  aboutButton <- QPushButton.newWithText ("ABOUT" :: String)
  supportButton <- QPushButton.newWithText ("SUPPORT" :: String)
  hSpacer <- QWidget.new
  for_ [generalButton, aboutButton, supportButton] $ \b ->
    setProperty b ("styleRole" :: Text) ("settingsTopbarButton" :: Text)

  for_ [generalButton, aboutButton, supportButton] $ \b ->
    QAbstractButton.setCheckable b True

  QAbstractButton.setChecked generalButton True

  QBoxLayout.addWidget buttonsLayout generalButton
  QBoxLayout.addWidget buttonsLayout hSpacer
  QBoxLayout.addWidget buttonsLayout aboutButton
  QBoxLayout.addWidget buttonsLayout hSpacer
  QBoxLayout.addWidget buttonsLayout supportButton

  QBoxLayout.addLayout settingsLayout buttonsLayout


  --general widget
  (generalWidget, generalSettings) <- initGeneralSettings
  QBoxLayout.addWidget settingsLayout generalWidget

  --about widget
  (aboutWidget, aboutSettings) <- initAboutSettings
  QBoxLayout.addWidget settingsLayout aboutWidget

  --support widget
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
  QLayout.setContentsMarginsRaw generalLayout 42 42 24 42


  languageLabel <- QLabel.newWithText ("<b>LANGUAGE</b>" :: String)
  languageSelector <- QComboBox.new
  QComboBox.addItem languageSelector ("English" :: String)
  addRow generalLayout languageLabel languageSelector
  addSeparator generalLayout
  

  countervalueLabel <- QLabel.newWithText ("<b>COUTNERVALUE</b>" :: String)
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

  for_ [languageSelector, countervalueSelector, rateProviderSelector, themeSelector] $ \w ->
    QWidget.setSizePolicyRaw w Maximum Fixed

  for_ [languageSelector, countervalueSelector, rateProviderSelector, themeSelector] $ \w ->
    QWidget.setEnabled w False

  QWidget.setLayout generalWidget generalLayout


  let gs = GeneralSettings{..}

  return (generalWidget, gs)


initAboutSettings :: IO (QWidget.QWidget, AboutSettings)
initAboutSettings = do
  aboutWidget <- QWidget.new
  aboutLayout <- QVBoxLayout.new
  
  let currentVersion = "Ariadne 1.0.2"
  
  versionLabel <- createTwoLinesLabel "VERSION" currentVersion
  releaseNotesButton <- QPushButton.newWithText("Release notes" :: String)
  void $ setProperty releaseNotesButton ("styleRole" :: Text) ("rightPaneSettingsButton" :: Text)
  addRow aboutLayout versionLabel releaseNotesButton
  addSeparator aboutLayout


  let license = "Sample text about license"

  licenseLabel <- createTwoLinesLabel "LICENSE" license
  licenseButton <- QPushButton.new
  void $ setProperty licenseButton ("styleRole" :: Text) ("linkButton" :: Text)
  addRow aboutLayout licenseLabel licenseButton

  for_ [releaseNotesButton, licenseButton] $ \w ->
    QWidget.setSizePolicyRaw w Maximum Fixed

  QWidget.setLayout aboutWidget aboutLayout

  let as = AboutSettings{..}

  connect_ releaseNotesButton QAbstractButton.clickedSignal 
        $ \_ -> (openBrowser "https://github.com/serokell/ariadne/blob/master/CHANGELOG.md" >> pass)
  connect_ licenseButton QAbstractButton.clickedSignal 
        $ \_ -> (openBrowser "https://github.com/serokell/ariadne/blob/master/COPYING.md" >> pass)  

  return (aboutWidget, as)

initSupportSettings :: IO (QWidget.QWidget, SupportSettings)
initSupportSettings = do
  supportWidget <- QWidget.new
  supportLayout <- QVBoxLayout.new

  let faqText = "If you are experiencing issues, please see the FAQ on Ariadne \
                \website for guidance on known issues."
  faqLabel <- createTwoLinesLabel "FREQUENTLY ASKED QUESTIONS" faqText
  faqButton <- QPushButton.new
  void $ setProperty faqButton ("styleRole" :: Text) ("linkButton" :: Text)
  addRow supportLayout faqLabel faqButton
  addSeparator supportLayout


  let clearCacheText = "Clear Ariadne Wallet cache to force resynchronization with the blockchain."
  clearCacheLabel <- createTwoLinesLabel "CLEAR CACHE" clearCacheText
  clearCacheButton <- QPushButton.newWithText ("Clear cache" :: String)
  addRow supportLayout clearCacheLabel clearCacheButton
  addSeparator supportLayout

  let reportText = "If the FAQ does not solve the issue you are experiencing,\
                   \ please use support request."
  reportLabel <- createTwoLinesLabel "REPORT A PROBLEM" reportText
  reportButton <- QPushButton.newWithText ("Support request" :: String)
  addRow supportLayout reportLabel reportButton
  addSeparator supportLayout

  let downloadLogsText = "If you want to inspect logs, you can download them here. \
                         \Logs do not contain sensitive information, \
                         \and it would be helpful to attach them to problem reports to help \
                         \the team investigate the issue you are experiencing \ 
                         \Logs can be attached automatically when using the bug report feature."

  downloadLogsLabel <- createTwoLinesLabel "EXPORT LOGS" downloadLogsText
  downloadLogsButton <- QPushButton.newWithText ("Download logs" ::  String)
  addRow supportLayout downloadLogsLabel downloadLogsButton
  addSeparator supportLayout


  let enableReportsText = "Share anonymous usage and diagnostic data to help \
                          \improve Ariadne Wallet, services and security features."
  enableReportsLabel <- createTwoLinesLabel "AUTOMATIC REPORTS" enableReportsText
  enableRepots <- QCheckBox.new
  addRow supportLayout enableReportsLabel enableRepots
  addSeparator supportLayout

  let analyticsText = "Enable analytics of anonymous data to help improve the user experience. \
                      \This includes the operating system, language, firmware versions and the numbebr of added accounts."
  analyticsLabel <- createTwoLinesLabel "ANALYTICS" analyticsText
  hasAnalytics <- QCheckBox.new
  addRow supportLayout analyticsLabel hasAnalytics
  addSeparator supportLayout

  let resetText = "Erase all Ariadne Wallet data stored on your computer, \
  \including your wallets, accounts, transaction history and settings."
  resetLabel <- createTwoLinesLabel "RESET ARIADNE" resetText
  resetButton <- QPushButton.newWithText ("Reset" :: String)
  addRow supportLayout resetLabel resetButton

  for_ [faqButton, clearCacheButton, reportButton, downloadLogsButton, resetButton] $ \w ->
    QWidget.setSizePolicyRaw w Maximum Fixed

  for_ [enableRepots, hasAnalytics] $ \w ->
    QWidget.setSizePolicyRaw w Maximum Fixed

  for_ [clearCacheButton, reportButton, downloadLogsButton] $ \b ->
    setProperty b ("styleRole" :: Text) ("rightPaneSettingsButton" :: Text)

  void $ setProperty resetButton ("styleRole" :: Text) ("resetSettingsButton" :: Text)

  for_ [faqButton, clearCacheButton, reportButton, downloadLogsButton, resetButton] $ \b ->
    QWidget.setEnabled b False

  for_ [hasAnalytics, enableRepots] $ \b ->
    QWidget.setEnabled b False

  QWidget.setLayout supportWidget supportLayout

  let ss = SupportSettings{..}
  return (supportWidget, ss)

showWidget :: Settings -> QWidget.QWidget -> QPushButton.QPushButton -> IO()
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
  label1 <- QLabel.newWithText("<b>" ++ line1 ++ "</b>" :: String)
  void $ setProperty label1 ("styleRole" :: Text) ("dialogHeader" :: Text)
  label2 <- QLabel.newWithText(line2 :: String)
  QLabel.setWordWrap label2 True
  QWidget.setMinimumSizeRaw label2 572 30
  QBoxLayout.addWidget layout label1
  QBoxLayout.addWidget layout label2
  QWidget.setLayout widget layout
  return widget

