module Ariadne.UI.Qt.Widgets.Dialogs.InsertPassword
  ( runInsertPassword
  , InsertPasswordResult(..)
  ) where

import Universum

import Control.Lens (makeLensesWith)

import Data.Bits
import qualified Data.Text as T

import Graphics.UI.Qtah.Core.Types (alignHCenter, alignVCenter)
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.Dialogs.Util
import Ariadne.Util

data InsertPassword = InsertPassword
    { insertPassword :: QDialog.QDialog
    , hasPassword :: QCheckBox.QCheckBox
    , password :: QLineEdit.QLineEdit
    , passwordWidget :: QWidget.QWidget
    , confirmButton :: QPushButton.QPushButton
    }

data InsertPasswordResult
    = InsertPasswordCanceled
    | InsertPasswordAccepted (Maybe Text)

makeLensesWith postfixLFields ''InsertPassword

initInsertPassword :: IO InsertPassword
initInsertPassword = do
    insertPassword <- QDialog.new
    QObject.setObjectName insertPassword ("insertPasswordDialog" :: String)
    let headerText = "Insert Password"
    QWidget.setWindowTitle insertPassword $ toString headerText
    QWidget.adjustSize insertPassword
    layout <- createLayout insertPassword

    header <- QLabel.newWithText . toString $ T.toUpper headerText
    addHeader layout header

    hasPassword <- createCheckBox layout CheckboxOnRight passwordLabelText
    QAbstractButton.setChecked hasPassword True

    (passwordWidget, passwordLayout) <- createSubWidget
    passwordLabel <- QLabel.newWithText ("<b>PASSWORD</b>" :: String)
    (passwordFieldLayout, password) <- createPasswordField "password"

    addRowLayout passwordLayout passwordLabel passwordFieldLayout

    QBoxLayout.addWidget layout passwordWidget

    QBoxLayout.addSpacing layout 24
    confirmButton <- QPushButton.newWithText ("CONFIRM" :: String)
    QObject.setObjectName confirmButton ("confirmButton" :: String)
    QWidget.setSizePolicyRaw confirmButton Maximum Preferred
    QBoxLayout.addWidget layout confirmButton
    void $ QLayout.setWidgetAlignment layout confirmButton $ alignHCenter .|. alignVCenter
    void $ setProperty confirmButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)

    QBoxLayout.addStretch layout

    let ip = InsertPassword{..}

    connect_ hasPassword QAbstractButton.toggledSignal $ hasPasswordToggled ip
    connect_ password QLineEdit.textChangedSignal $ \_ -> revalidate ip
    connect_ confirmButton QAbstractButton.clickedSignal $ \_ -> QDialog.accept insertPassword

    revalidate ip

    return ip

runInsertPassword:: IO InsertPasswordResult
runInsertPassword = do
    ip@InsertPassword{..} <- initInsertPassword
    result <- toEnum <$> QDialog.exec insertPassword

    case result of
        QDialog.Accepted -> maybe InsertPasswordCanceled InsertPasswordAccepted <$> 
            fillPasswordParameter ip
        QDialog.Rejected -> return InsertPasswordCanceled

passwordLabelText :: Text
passwordLabelText = "Activate to use a password."

hasPasswordToggled :: InsertPassword -> Bool -> IO ()
hasPasswordToggled ip@InsertPassword{..} checked = do
    QWidget.setVisible passwordWidget checked
    QWidget.adjustSize insertPassword
    revalidate ip

fillPasswordParameter :: InsertPassword -> IO (Maybe (Maybe Text))
fillPasswordParameter InsertPassword{..} = do
    ipHasPassword <- QAbstractButton.isChecked hasPassword
    ipPasswordCandidate <- T.strip . fromString <$> QLineEdit.text password

    return $ do
        when ipHasPassword $ guard $ not (null ipPasswordCandidate)
        return $ if ipHasPassword then Just ipPasswordCandidate else Nothing

revalidate :: InsertPassword -> IO ()
revalidate ip@InsertPassword{..} = do
    valid <- isJust <$> fillPasswordParameter ip
    QWidget.setEnabled confirmButton valid
