module Ariadne.UI.Qt.Widgets.Dialogs.Delete
  ( DeletingItem(..)
  , DeletionResult(..)
  , runDelete
  ) where

import Universum

import qualified Data.Text as T
import Formatting

import Graphics.UI.Qtah.Signal (connect_)

import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Widgets.Dialogs.Util
import Ariadne.UI.Qt.UI

data DeletingItem = DelWallet | DelAccount deriving Eq
data DeletionResult = DoDelete | Cancel deriving Eq

data Delete =
  Delete
    { delete :: QDialog.QDialog
    , isSure :: QCheckBox.QCheckBox
    , itemName :: Text
    , retypeWidget :: QWidget.QWidget
    , retypeName :: QLineEdit.QLineEdit
    , deleteButton :: QPushButton.QPushButton
    , itemType :: DeletingItem
    }

initDelete :: DeletingItem -> Text -> IO Delete
initDelete itemType itemName = do
  delete <- QDialog.new
  layout <- createLayout delete

  let headerString = toString . T.toUpper $ sformat ("delete " % itemTypeFormat) itemType

  QWidget.setWindowTitle delete headerString

  header <- QLabel.newWithText headerString
  addHeader layout header

  warningLabel <- QLabel.newWithText . toString $ sformat
    ("Do you really want to delete <b>" % stext % "</b> " % itemTypeFormat % "?") itemName itemType
  QBoxLayout.addWidget layout warningLabel

  isSure <- createCheckBox layout $ sformat
    ("Make sure you have access to backup before continuing. \
     \Otherwise you will lose all your funds connected to this " % itemTypeFormat % ".")
    itemType

  (retypeWidget, retypeLayout) <- createSubWidget
  addSeparator retypeLayout

  retypeLabel <- QLabel.newWithText . toString $ sformat
    ("Type " % itemTypeFormat % " name to confirm deletion") itemType
  retypeName <- QLineEdit.new
  addRow retypeLayout retypeLabel retypeName

  QBoxLayout.addWidget layout retypeWidget
  QWidget.hide retypeWidget

  buttonsLayout <- QHBoxLayout.new
  cancelButton <- QPushButton.newWithText ("CANCEL" :: String)
  deleteButton <- QPushButton.newWithText ("DELETE" :: String)
  QBoxLayout.addWidget buttonsLayout cancelButton
  QBoxLayout.addWidget buttonsLayout deleteButton
  QBoxLayout.addLayout layout buttonsLayout

  void $ setProperty cancelButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  void $ setProperty deleteButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  void $ setProperty cancelButton ("styleRole" :: Text) ("secondaryButton" :: Text)
  void $ setProperty deleteButton ("styleRole" :: Text) ("dangerButton" :: Text)

  let del = Delete{..}

  connect_ isSure QAbstractButton.toggledSignal $ isSureToggled del
  connect_ retypeName QLineEdit.textChangedSignal $ \_ -> revalidate del
  connect_ cancelButton QAbstractButton.clickedSignal $ \_ -> QDialog.reject delete
  connect_ deleteButton QAbstractButton.clickedSignal $ \_ -> QDialog.accept delete

  revalidate del

  return del

runDelete :: DeletingItem -> Text -> IO DeletionResult
runDelete itemType itemName = do
  del@Delete{delete = delete} <- initDelete itemType itemName
  result <- toEnum <$> QDialog.exec delete
  valid <- isValid del

  return $ case result of
    QDialog.Accepted -> if valid then DoDelete else Cancel
    QDialog.Rejected -> Cancel

isValid :: Delete -> IO Bool
isValid Delete{..} = do
  delIsSure <- QAbstractButton.isChecked isSure
  delItemName <- T.strip . fromString <$> QLineEdit.text retypeName

  -- We do not ask to retype name for accounts
  return $ delIsSure && (itemType /= DelWallet || delItemName == itemName)

revalidate :: Delete -> IO ()
revalidate del@Delete{..} = isValid del >>= QWidget.setEnabled deleteButton

itemTypeFormat :: Format r (DeletingItem -> r)
itemTypeFormat = later $ \case
  DelWallet -> "wallet"
  DelAccount -> "account"

isSureToggled :: Delete -> Bool -> IO ()
isSureToggled del@Delete{..} checked = do
  revalidate del
  when (itemType == DelWallet) $ do
    QWidget.setVisible retypeWidget checked
    QWidget.adjustSize delete
