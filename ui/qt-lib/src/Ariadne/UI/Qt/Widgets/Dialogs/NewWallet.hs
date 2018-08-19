module Ariadne.UI.Qt.Widgets.Dialogs.NewWallet
  ( runNewWallet
  , NewWalletParameters(..)
  , NewWalletResult(..)
  ) where

import Universum

import Control.Lens (makeLensesWith)
import IiExtras (postfixLFields)

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
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QFrame as QFrame
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.UI

data NewWallet =
  NewWallet
    { newWallet :: QDialog.QDialog
    , walletName :: QLineEdit.QLineEdit
    , hasPassword :: QCheckBox.QCheckBox
    , password :: QLineEdit.QLineEdit
    , repeatPassword :: QLineEdit.QLineEdit
    , passwordWidget :: QWidget.QWidget
    , createButton :: QPushButton.QPushButton
    }

data NewWalletParameters =
  NewWalletParameters
    { nwName :: Text
    , nwPassword :: Maybe Text
    }

data NewWalletResult = NewWalletCanceled | NewWalletAccepted NewWalletParameters

makeLensesWith postfixLFields ''NewWallet
makeLensesWith postfixLFields ''NewWalletParameters

addRow :: (QWidget.QWidgetPtr lbl, QWidget.QWidgetPtr wgt)
       => QVBoxLayout.QVBoxLayout -> lbl -> wgt -> IO ()
addRow layout label widget = do
  rowLayout <- QHBoxLayout.new
  QBoxLayout.addWidget rowLayout label
  QBoxLayout.addWidget rowLayout widget
  QBoxLayout.setStretch rowLayout 0 2
  QBoxLayout.setStretch rowLayout 1 1
  QLayout.setContentsMarginsRaw rowLayout 18 0 18 0

  QBoxLayout.addLayout layout rowLayout

addRowLayout :: (QWidget.QWidgetPtr lbl, QLayout.QLayoutPtr lay)
       => QVBoxLayout.QVBoxLayout -> lbl -> lay -> IO ()
addRowLayout layout label widgetLayout = do
  rowLayout <- QHBoxLayout.new
  QBoxLayout.addWidget rowLayout label
  QBoxLayout.addLayout rowLayout widgetLayout
  QBoxLayout.setStretch rowLayout 0 2
  QBoxLayout.setStretch rowLayout 1 1
  QLayout.setContentsMarginsRaw rowLayout 18 0 18 0

  QBoxLayout.addLayout layout rowLayout

addSeparator :: QVBoxLayout.QVBoxLayout -> IO ()
addSeparator layout = do
  separator <- QFrame.new
  QFrame.setFrameShape separator QFrame.HLine
  void $ setProperty separator ("styleRole" :: Text) ("separator" :: Text)
  void $ setProperty separator ("orientation" :: Text) ("horizontal" :: Text)

  QBoxLayout.addWidget layout separator

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

initNewWallet :: IO NewWallet
initNewWallet = do
  newWallet <- QDialog.new
  QObject.setObjectName newWallet ("newWalletDialog" :: String)
  QWidget.setWindowTitle newWallet ("Create new wallet" :: String)
  QWidget.adjustSize newWallet

  layout <- QVBoxLayout.new
  QWidget.setLayout newWallet layout
  QBoxLayout.setSpacing layout 18
  QLayout.setContentsMarginsRaw layout 24 42 24 24

  walletNameLabel <- QLabel.newWithText ("<b>WALLET NAME</b>" :: String)
  walletName <- QLineEdit.new
  QLineEdit.setPlaceholderText walletName ("wallet name" :: String)

  addRow layout walletNameLabel walletName
  addSeparator layout

  hasPasswordLabel <- QLabel.newWithText $ toString passwordLabelText
  QLabel.setWordWrap hasPasswordLabel True
  QWidget.setMinimumSizeRaw hasPasswordLabel 600 30
  hasPassword <- QCheckBox.new
  QAbstractButton.setChecked hasPassword True
  QWidget.setSizePolicyRaw hasPassword Maximum Preferred

  addRow layout hasPasswordLabel hasPassword

  passwordWidget <- QWidget.new
  passwordLayout <- QVBoxLayout.new
  QWidget.setLayout passwordWidget passwordLayout
  QLayout.setContentsMarginsRaw passwordLayout 0 0 0 0
  QBoxLayout.setSpacing passwordLayout 18
  QWidget.setSizePolicyRaw passwordWidget Preferred Minimum
  QLayout.setSizeConstraint passwordLayout QLayout.SetMinimumSize

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
  connect_ password QLineEdit.textChangedSignal $ \_ -> revalidate nw
  connect_ repeatPassword QLineEdit.textChangedSignal $ \_ -> revalidate nw
  connect_ createButton QAbstractButton.clickedSignal $ \_ -> QDialog.accept newWallet

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
  "Activate to create password. Note that good password should be at least\
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

  nwHasPassword <- QAbstractButton.isChecked hasPassword
  nwPasswordCandidate <- T.strip . fromString <$> QLineEdit.text password
  nwRepeatPassword <- T.strip . fromString <$> QLineEdit.text repeatPassword

  let
    nameValid = not $ null nwName
    passwordValid =
      (not nwHasPassword) || (not (null nwPasswordCandidate) && nwPasswordCandidate == nwRepeatPassword)
    nwPassword = if nwHasPassword then Just nwPasswordCandidate else Nothing

  return $
    if nameValid && passwordValid
       then Just $ NewWalletParameters{..}
       else Nothing

isValid :: NewWallet -> IO Bool
isValid = fmap isJust . fillWaletParameters

revalidate :: NewWallet -> IO ()
revalidate nw@NewWallet{..} = do
  valid <- isValid nw
  QWidget.setEnabled createButton valid
