module Ariadne.UI.Qt.Widgets.Settings
    ( Settings
    , initSettings
    , showSettingsWindow
    ) where

-- import qualified Data.Text as T

import Control.Lens (makeLensesWith)
import Data.Bits

-- import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Core.Types (alignHCenter, alignVCenter)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

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

-- import Ariadne.UI.Qt.AnsiToHTML
-- import Ariadne.UI.Qt.Face
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
    , termsButton :: QPushButton.QPushButton
    , privacyButton :: QPushButton.QPushButton
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

  settingsLayout <- QVBoxLayout.new
  QLayout.setContentsMarginsRaw settingsLayout 0 0 0 0
  QLayout.setSpacing settingsLayout 0

  labelLayout <- QHBoxLayout.new
  QLayout.setContentsMarginsRaw labelLayout 0 24 0 24
  label <- QLabel.newWithText ("<b>SETTINGS</b>" :: String)
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
  QWidget.setMinimumWidth hSpacer 20
  QWidget.setMaximumWidth hSpacer 20

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

  connect_ generalButton QAbstractButton.clickedSignal $ \_ -> showWidget st generalWidget
  connect_ aboutButton QAbstractButton.clickedSignal $ \_ -> showWidget st aboutWidget
  connect_ supportButton QAbstractButton.clickedSignal $ \_ -> showWidget st supportWidget
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
  QWidget.setSizePolicyRaw releaseNotesButton Maximum Fixed
  addRow aboutLayout versionLabel releaseNotesButton
  addSeparator aboutLayout

  let conditions = "Sample text about terms and conditions"

  termsLabel <- createTwoLinesLabel "TERMS AND CONDITIONS" conditions
  --missing icon
  termsButton <- QPushButton.new
  QWidget.setSizePolicyRaw termsButton Maximum Fixed
  addRow aboutLayout termsLabel termsButton
  addSeparator aboutLayout

  let privacyPolicy = "Sample text about privacy policy"

  privacyLabel <- createTwoLinesLabel "PRIVACY POLICY" privacyPolicy
  --missing icon
  privacyButton <- QPushButton.new
  QWidget.setSizePolicyRaw privacyButton Maximum Fixed
  addRow aboutLayout privacyLabel privacyButton

  QWidget.setLayout aboutWidget aboutLayout

  let as = AboutSettings{..}

  return (aboutWidget, as)

initSupportSettings :: IO (QWidget.QWidget, SupportSettings)
initSupportSettings = do
  supportWidget <- QWidget.new
  supportLayout <- QVBoxLayout.new

  faqLabel <- createTwoLinesLabel "FAQ" "Sample text about FAQ"
  --missing icon
  faqButton <- QPushButton.new
  QWidget.setSizePolicyRaw faqButton Maximum Fixed
  addRow supportLayout faqLabel faqButton

  clearCacheLabel <- createTwoLinesLabel "CLEAR CACHE" "Sample text about clear cache"
  --missing icon
  clearCacheButton <- QPushButton.newWithText ("Clear cache" :: String)
  QWidget.setSizePolicyRaw clearCacheButton Maximum Fixed
  addRow supportLayout clearCacheLabel clearCacheButton

  reportLabel <- createTwoLinesLabel "REPORT A PROBLEM" "Sample text about report"
  reportButton <- QPushButton.newWithText ("Support request" :: String)
  QWidget.setSizePolicyRaw reportButton Maximum Fixed
  addRow supportLayout reportLabel reportButton

  downloadLogsLabel <- createTwoLinesLabel "EXPORT LOGS" "Sample text about logs"
  downloadLogsButton <- QPushButton.newWithText ("Download logs" ::  String)
  QWidget.setSizePolicyRaw downloadLogsLabel Maximum Fixed
  addRow supportLayout downloadLogsLabel downloadLogsButton

  enableReportsLabel <- createTwoLinesLabel "AUTOMATIC REPORTS" "Sample text about reports which is long enough to not fit a single line Sample text about reports which is long enough to not fit a single line"
  enableRepots <- QCheckBox.new
  addRow supportLayout enableReportsLabel enableRepots

  analyticsLabel <- createTwoLinesLabel "ANALYTICS" "Sample text about analytics"
  hasAnalytics <- QCheckBox.new
  addRow supportLayout analyticsLabel hasAnalytics


  resetLabel <- createTwoLinesLabel "RESET ARIADNE" "Sample text about reset"
  resetButton <- QPushButton.newWithText ("Reset" :: String)
  QWidget.setSizePolicyRaw resetButton Maximum Fixed
  addRow supportLayout resetLabel resetButton

  QWidget.setLayout supportWidget supportLayout

  let ss = SupportSettings{..}
  return (supportWidget, ss)

showWidget :: Settings -> QWidget.QWidget -> IO()
showWidget Settings{..} widget = do
  QWidget.setVisible generalWidget False
  QWidget.setVisible aboutWidget False
  QWidget.setVisible supportWidget False
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
  label2 <- QLabel.newWithText(line2 :: String)
  QLabel.setWordWrap label2 True
  QWidget.setMinimumSizeRaw label2 572 30
  QBoxLayout.addWidget layout label1
  QBoxLayout.addWidget layout label2
  QWidget.setLayout widget layout
  return widget

