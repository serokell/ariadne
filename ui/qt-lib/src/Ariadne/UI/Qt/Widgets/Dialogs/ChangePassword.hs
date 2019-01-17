module Ariadne.UI.Qt.Widgets.Dialogs.ChangePassword
  ( runChangePassword
  , ChangePasswordResult (..)
  ) where

import Control.Lens (has, makeLensesWith)

import Data.Bits
import qualified Data.Text as T
import Data.Validation (Validation(..), validate, _Success)

import Graphics.UI.Qtah.Core.Types (alignHCenter, alignVCenter)
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.Dialogs.Util
import Ariadne.UI.Qt.Widgets.Dialogs.Validation
import Ariadne.UIConfig
import Ariadne.Util

data Error = PasswordsDontMatch

data ChangePassword = ChangePassword
    { changePassword :: QDialog.QDialog
    , newPassword :: QLineEdit.QLineEdit
    , confirmPassword :: QLineEdit.QLineEdit
    , changePasswordButton :: QPushButton.QPushButton
    , validations :: Validations
    }

makeLensesWith postfixLFields ''ChangePassword

data ChangePasswordResult
    = ChangePasswordCanceled
    | ChangePasswordAccepted (Maybe Text)

initChangePassword :: IO ChangePassword
initChangePassword = do
    changePassword <- QDialog.new
    QObject.setObjectName changePassword ("changePasswordDialog" :: String)
    QWidget.setWindowTitle changePassword $ toString changePasswordHeaderMessage
    QWidget.adjustSize changePassword
    layout <- createLayout changePassword

    header <- QLabel.newWithText . toString $ T.toUpper changePasswordHeaderMessage
    addHeader layout header

    infoLabel <- QLabel.newWithText $ toString changePasswordInfoMessage
    QBoxLayout.addWidget layout infoLabel
    addSeparator layout

    newPasswordLabel <- QLabel.newWithText ("<b>NEW PASSWORD</b>" :: String)
    (newPasswordLayout, newPassword) <- createPasswordField "new password"
    addRowLayout layout newPasswordLabel newPasswordLayout
    addSeparator layout

    confirmPasswordLabel <- QLabel.newWithText ("<b>CONFIRM PASSWORD</b>" :: String)
    (confirmPasswordLayout, confirmPassword) <- createPasswordField "confirmPassword"
    addRowLayout layout confirmPasswordLabel confirmPasswordLayout

    QBoxLayout.addSpacing layout 43
    changePasswordButton <- QPushButton.newWithText ("CHANGE PASSWORD" :: String)
    QWidget.setSizePolicyRaw changePasswordButton Maximum Preferred
    QBoxLayout.addWidget layout changePasswordButton
    void $ QLayout.setWidgetAlignment layout changePasswordButton $ alignHCenter .|. alignVCenter
    void $ setProperty changePasswordButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)

    validations <- createValidations changePassword $
        [("Passwords don't match", QWidget.cast confirmPassword)]

    let cp = ChangePassword {..}

    connect_ newPassword QLineEdit.textChangedSignal $ \_ -> revalidate cp
    connect_ confirmPassword QLineEdit.textChangedSignal $ \_ -> revalidate cp
    connect_ changePasswordButton QAbstractButton.clickedSignal $ \_ -> QDialog.accept changePassword

    revalidate cp

    return cp

runChangePassword :: IO ChangePasswordResult
runChangePassword = do
    cp@ChangePassword{..} <- initChangePassword
    result <- toEnum <$> QDialog.exec changePassword
    parameters <- fillPasswordParameter cp

    return $ case result of
        QDialog.Accepted -> case parameters of
            Success pp -> if null pp
                            then ChangePasswordAccepted Nothing
                            else ChangePasswordAccepted $ Just pp
            Failure _ -> ChangePasswordCanceled
        QDialog.Rejected -> ChangePasswordCanceled

fillPasswordParameter :: ChangePassword -> IO (Validation [Error] Text)
fillPasswordParameter ChangePassword{..} = do
    newPasswordText <- T.strip . fromString <$> QLineEdit.text newPassword
    confirmPasswordText <- T.strip . fromString <$> QLineEdit.text confirmPassword
    let vNewPassword = validate ([PasswordsDontMatch]) (newPasswordText ==) confirmPasswordText
    return vNewPassword

isSuccess :: Validation e b -> Bool
isSuccess = has _Success

isValid :: ChangePassword -> IO Bool
isValid = fmap isSuccess . fillPasswordParameter

revalidate :: ChangePassword -> IO ()
revalidate cp@ChangePassword{..} = do
    validation <- fillPasswordParameter cp
    let valid = isSuccess validation
    showErrorsV validations validation (\_ -> Just (Nothing, QWidget.cast confirmPassword))
    QWidget.setEnabled changePasswordButton valid
