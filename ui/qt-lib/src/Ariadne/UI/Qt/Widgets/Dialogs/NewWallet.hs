module Ariadne.UI.Qt.Widgets.Dialogs.NewWallet
  ( runNewWallet
  , NewWalletParameters(..)
  , NewWalletSpecifier(..)
  , NewWalletResult(..)
  ) where

import Universum

import Control.Lens (makeLensesWith)

import Data.Bits
import qualified Data.Text as T

import Graphics.UI.Qtah.Core.Types (alignHCenter, alignVCenter)
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Gui.QIcon as QIcon
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Widgets.QComboBox as QComboBox
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.Dialogs.Util
import Ariadne.Util

data NewWalletMode = CreateWallet | ImportWallet deriving (Show, Eq, Enum, Bounded)

newWalletModeName :: NewWalletMode -> Text
newWalletModeName CreateWallet = "Create new wallet"
newWalletModeName ImportWallet = "Import wallet"

data NewWallet =
  NewWallet
    { newWallet :: QDialog.QDialog
    , walletName :: QLineEdit.QLineEdit
    , hasPassword :: QCheckBox.QCheckBox
    , password :: QLineEdit.QLineEdit
    , repeatPassword :: QLineEdit.QLineEdit
    , passwordWidget :: QWidget.QWidget
    , createButton :: QPushButton.QPushButton
    , modeSelector :: QComboBox.QComboBox
    , walletMnemonicWidget :: QWidget.QWidget
    , walletMnemonic :: QLineEdit.QLineEdit
    , fullRestore :: QCheckBox.QCheckBox
    }

data NewWalletSpecifier
  = NewWalletName
  | NewWalletMnemonic !Text !Bool

data NewWalletParameters =
  NewWalletParameters
    { nwName :: !Text
    , nwSpecifier :: !NewWalletSpecifier
    , nwPassword :: !(Maybe Text)
    }

data NewWalletResult = NewWalletCanceled | NewWalletAccepted NewWalletParameters

makeLensesWith postfixLFields ''NewWallet
makeLensesWith postfixLFields ''NewWalletParameters

createPasswordField :: Text -> IO (QHBoxLayout.QHBoxLayout, QLineEdit.QLineEdit)
createPasswordField placeholder = do
  layout <- QHBoxLayout.new
  field <- QLineEdit.new
  QLineEdit.setEchoMode field QLineEdit.Password
  QLineEdit.setPlaceholderText field $ toString placeholder

  visibleButton <- QPushButton.new
  void $ setProperty visibleButton ("styleRole" :: Text) ("passwordVisibilityToggle" :: Text)
  QAbstractButton.setIcon visibleButton =<< QIcon.newWithFile (":/images/hide-pass-ic.png" :: String)
  QAbstractButton.setCheckable visibleButton True
  QWidget.setSizePolicyRaw visibleButton Maximum Maximum

  QBoxLayout.addWidget layout field
  QBoxLayout.addWidget layout visibleButton
  QLayout.setSpacing layout 12
  QBoxLayout.setStretch layout 0 1
  QBoxLayout.setStretch layout 1 0

  let
    togglePasswordVisibility checked = do
      QLineEdit.setEchoMode field $
        case checked of
          True -> QLineEdit.Normal
          False -> QLineEdit.Password

  connect_ visibleButton QAbstractButton.toggledSignal togglePasswordVisibility

  return (layout, field)

createModeSelector :: IO QComboBox.QComboBox
createModeSelector = do
  modeSelector <- QComboBox.new

  for_ [minBound..maxBound] $ \mode ->
    QComboBox.addItem modeSelector $ toString $ T.toUpper $ newWalletModeName mode

  return modeSelector

initNewWallet :: IO NewWallet
initNewWallet = do
  newWallet <- QDialog.new
  QObject.setObjectName newWallet ("newWalletDialog" :: String)
  QWidget.setWindowTitle newWallet ("Create new wallet" :: String)
  QWidget.adjustSize newWallet

  layout <- createLayout newWallet

  modeSelector <- createModeSelector

  addHeader layout modeSelector
  void $ QLayout.setWidgetAlignment layout modeSelector $ alignHCenter .|. alignVCenter
  void $ setProperty modeSelector ("styleRole" :: Text) ("dialogHeader" :: Text)

  walletNameLabel <- QLabel.newWithText ("<b>WALLET NAME</b>" :: String)
  walletName <- QLineEdit.new
  QLineEdit.setPlaceholderText walletName ("wallet name" :: String)

  addRow layout walletNameLabel walletName

  walletMnemonicLabel <- QLabel.newWithText ("<b>MNEMONIC PHRASE</b>" :: String)
  walletMnemonic <- QLineEdit.new

  (walletMnemonicWidget, walletMnemonicLayout) <- createSubWidget
  addRow walletMnemonicLayout walletMnemonicLabel walletMnemonic

  fullRestoreLabel <- QLabel.newWithText ("Perform full restore" :: String)
  fullRestore <- QCheckBox.new
  QAbstractButton.setChecked fullRestore True
  QWidget.setSizePolicyRaw fullRestore Maximum Preferred

  addRow walletMnemonicLayout fullRestoreLabel fullRestore

  QBoxLayout.addWidget layout walletMnemonicWidget
  QWidget.hide walletMnemonicWidget

  addSeparator layout

  hasPasswordLabel <- QLabel.newWithText $ toString passwordLabelText
  QLabel.setWordWrap hasPasswordLabel True
  QWidget.setMinimumSizeRaw hasPasswordLabel 600 30
  hasPassword <- QCheckBox.new
  QAbstractButton.setChecked hasPassword True
  QWidget.setSizePolicyRaw hasPassword Maximum Preferred

  addRow layout hasPasswordLabel hasPassword

  (passwordWidget, passwordLayout) <- createSubWidget
  passwordLabel <- QLabel.newWithText ("<b>PASSWORD</b>" :: String)
  (passwordFieldLayout, password) <- createPasswordField "password"

  addRowLayout passwordLayout passwordLabel passwordFieldLayout
  addSeparator passwordLayout

  repeatPasswordLabel <- QLabel.newWithText ("<b>REPEAT PASSWORD</b>" :: String)
  (repeatPasswordFieldLayout, repeatPassword) <- createPasswordField "password"

  addRowLayout passwordLayout repeatPasswordLabel repeatPasswordFieldLayout

  QBoxLayout.addWidget layout passwordWidget

  QBoxLayout.addSpacing layout 24
  createButton <- QPushButton.newWithText ("CREATE" :: String)
  QObject.setObjectName createButton ("createButton" :: String)
  QWidget.setSizePolicyRaw createButton Maximum Preferred
  QBoxLayout.addWidget layout createButton
  void $ QLayout.setWidgetAlignment layout createButton $ alignHCenter .|. alignVCenter

  QBoxLayout.addStretch layout

  let nw = NewWallet{..}

  connect_ hasPassword QAbstractButton.toggledSignal $ hasPasswordToggled nw
  connect_ walletName QLineEdit.textChangedSignal $ \_ -> revalidate nw
  connect_ walletMnemonic QLineEdit.textChangedSignal $ \_ -> revalidate nw
  connect_ password QLineEdit.textChangedSignal $ \_ -> revalidate nw
  connect_ repeatPassword QLineEdit.textChangedSignal $ \_ -> revalidate nw
  connect_ createButton QAbstractButton.clickedSignal $ \_ -> QDialog.accept newWallet
  connect_ modeSelector QComboBox.activatedSignal $ \ix -> modeChanged nw $ toEnum ix

  revalidate nw

  return nw

runNewWallet:: IO NewWalletResult
runNewWallet = do
  nw@NewWallet{..} <- initNewWallet
  result <- fmap toEnum . liftIO . QDialog.exec $ newWallet

  case result of
    QDialog.Accepted -> maybe NewWalletCanceled NewWalletAccepted <$> fillWaletParameters nw
    QDialog.Rejected -> return NewWalletCanceled

passwordLabelText :: Text
passwordLabelText =
  "Activate to create password. Note that good password should be at least \
  \<b>7 characters long</b>, and have at least <b>1 uppercase character</b>\
  \, <b>1 lowercase character</b> and <b>1 digit</b>."

hasPasswordToggled :: NewWallet -> Bool -> IO ()
hasPasswordToggled nw@NewWallet{..} checked = do
  QWidget.setVisible passwordWidget checked
  QWidget.adjustSize newWallet
  revalidate nw

fillWaletParameters :: NewWallet -> IO (Maybe NewWalletParameters)
fillWaletParameters NewWallet{..} = do
  nwName <- T.strip . fromString <$> QLineEdit.text walletName
  nwMnemonic <- T.strip . fromString <$> QLineEdit.text walletMnemonic
  nwFullRestore <- QAbstractButton.isChecked fullRestore

  nwHasPassword <- QAbstractButton.isChecked hasPassword
  nwPasswordCandidate <- T.strip . fromString <$> QLineEdit.text password
  nwRepeatPassword <- T.strip . fromString <$> QLineEdit.text repeatPassword

  nwModeIx <- QComboBox.currentIndex modeSelector
  let
    nwMode =
      if nwModeIx < fromEnum @NewWalletMode minBound || nwModeIx > fromEnum @NewWalletMode maxBound
         then Nothing
         else Just $ toEnum nwModeIx

  return $ do
    mode <- nwMode
    guard $ not $ null nwName
    when nwHasPassword $
      guard $ not (null nwPasswordCandidate) && nwPasswordCandidate == nwRepeatPassword
    let nwPassword = if nwHasPassword then Just nwPasswordCandidate else Nothing

    nwSpecifier <- case mode of
      CreateWallet -> do
        return $ NewWalletName
      ImportWallet -> do
        guard $ not $ null nwMnemonic
        guard $ length (words nwMnemonic) >= 12

        return $ NewWalletMnemonic nwMnemonic nwFullRestore

    return NewWalletParameters{..}

isValid :: NewWallet -> IO Bool
isValid = fmap isJust . fillWaletParameters

revalidate :: NewWallet -> IO ()
revalidate nw@NewWallet{..} = do
  valid <- isValid nw
  QWidget.setEnabled createButton valid

modeChanged :: NewWallet -> NewWalletMode -> IO ()
modeChanged nw@NewWallet{..} newMode = do
  let
    modeIsRestore = case newMode of
      CreateWallet -> False
      ImportWallet -> True

  QWidget.setVisible walletMnemonicWidget modeIsRestore
  QWidget.adjustSize newWallet

  revalidate nw
