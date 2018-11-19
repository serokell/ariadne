module Ariadne.UI.Qt.Widgets.Dialogs.AccountSettings
  ( RenameHandler
  , DeleteHandler
  , runAccountSettings
  ) where

import qualified Data.Text as T
import Graphics.UI.Qtah.Core.HSize (HSize(..))
import Graphics.UI.Qtah.Core.Types (QtCursorShape(..))
import Graphics.UI.Qtah.Signal (connect_)

import qualified Graphics.UI.Qtah.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Event as Event
import qualified Graphics.UI.Qtah.Gui.QCursor as QCursor
import qualified Graphics.UI.Qtah.Gui.QMouseEvent as QMouseEvent
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.Dialogs.Util

data AccountSettings =
  AccountSettings
    { accountSettings :: QDialog.QDialog
    }

type RenameHandler = Text -> IO ()
type DeleteHandler = IO ()

initAccountSettings :: Text -> RenameHandler -> DeleteHandler -> IO AccountSettings
initAccountSettings currentName renameHandler deleteHandler = do
  accountSettings <- QDialog.new
  layout <- createLayout accountSettings

  let headerString = toString $ T.toUpper "ACCOUNT SETTINGS"

  QWidget.setWindowTitle accountSettings headerString

  header <- QLabel.newWithText headerString
  addHeader layout header

  accountNameLabel <- QLabel.newWithText ("ACCOUNT NAME" :: String)
  accountNameEdit <- QLineEdit.newWithText $ toString currentName

  addRow layout accountNameLabel accountNameEdit

  pointingCursor <- QCursor.newWithCursorShape PointingHandCursor
  buttonsLayout <- QHBoxLayout.new
  deleteButton <- QPushButton.newWithText ("Delete account" :: String)
  QWidget.setCursor deleteButton pointingCursor

  QBoxLayout.addStretch buttonsLayout
  QBoxLayout.addWidget buttonsLayout deleteButton
  QBoxLayout.addStretch buttonsLayout

  QBoxLayout.addLayout layout buttonsLayout

  QPushButton.setDefault deleteButton False
  QPushButton.setAutoDefault deleteButton False

  setProperty deleteButton ("dialogButtonRole" :: Text) ("textDangerButton" :: Text)

  let asettings = AccountSettings{..}

  connect_ deleteButton QAbstractButton.clickedSignal $ \_ ->
    deleteHandler >> QDialog.accept accountSettings
  connect_ accountNameEdit QLineEdit.editingFinishedSignal $
    QLineEdit.text accountNameEdit <&> fromString >>= renameHandler

  QWidget.adjustSize accountSettings
  -- Let user resize the dialog, but not too much
  HSize{width = asWidth, height = asHeight} <- QWidget.size accountSettings
  QWidget.setMinimumSize accountSettings $ HSize{width = asWidth, height = asHeight}
  QWidget.setMaximumSize accountSettings $ HSize{width = 2 * asWidth, height = asHeight}

  -- This unfocuses any focused widget when user clicks outside input fields,
  -- essentially triggering editingFinished signal.
  void $ Event.onEvent accountSettings $ \(ev :: QMouseEvent.QMouseEvent) -> do
    evType <- QEvent.eventType ev
    when (evType == QEvent.MouseButtonRelease) $
      QApplication.focusWidget >>= QWidget.clearFocus
    return False

  return asettings

runAccountSettings :: Text -> RenameHandler -> DeleteHandler -> IO ()
runAccountSettings currentName renameHandler deleteHandler = do
  AccountSettings{..} <- initAccountSettings currentName renameHandler deleteHandler

  void $ QDialog.exec accountSettings
