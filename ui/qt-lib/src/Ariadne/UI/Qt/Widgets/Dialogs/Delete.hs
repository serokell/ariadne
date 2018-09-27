module Ariadne.UI.Qt.Widgets.Dialogs.Delete
       ( DeletingItem(..)
       , DeletionResult(..)
       , runDelete
       ) where

import qualified Data.Text as T
import Formatting

import Graphics.UI.Qtah.Signal (connect_)

import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.Dialogs.Util

data DeletionResult = DoDelete | Cancel deriving Eq

data Delete =
  Delete
    { delete :: QDialog.QDialog
    , isSure :: QCheckBox.QCheckBox
    , deleteButton :: QPushButton.QPushButton
    , itemType :: UiDeletingItem
    }

initDelete :: UiDeletingItem -> IO Delete
initDelete itemType = do
  delete <- QDialog.new
  layout <- createLayout delete

  let headerString = toString . T.toUpper $ sformat ("delete " % itemTypeFormat) itemType

  QWidget.setWindowTitle delete headerString

  header <- QLabel.newWithText headerString
  addHeader layout header

  warningLabel <- QLabel.newWithText . toString $ sformat
    ("Do you really want to delete this " % itemTypeFormat % "?") itemType
  QBoxLayout.addWidget layout warningLabel

  isSure <- createCheckBox layout CheckboxOnLeft $ sformat
    ("Make sure you have access to backup before continuing. \
     \Otherwise you will lose all your funds connected to this " % itemTypeFormat % ".")
    itemType

  buttonsLayout <- QHBoxLayout.new
  cancelButton <- QPushButton.newWithText ("CANCEL" :: String)
  deleteButton <- QPushButton.newWithText ("DELETE" :: String)
  QBoxLayout.addWidget buttonsLayout cancelButton
  QBoxLayout.addWidget buttonsLayout deleteButton
  QBoxLayout.addLayout layout buttonsLayout

  setProperty cancelButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  setProperty deleteButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  setProperty cancelButton ("styleRole" :: Text) ("secondaryButton" :: Text)
  setProperty deleteButton ("styleRole" :: Text) ("dangerButton" :: Text)

  let del = Delete{..}

  connect_ isSure QAbstractButton.toggledSignal $ isSureToggled del
  connect_ cancelButton QAbstractButton.clickedSignal $ \_ -> QDialog.reject delete
  connect_ deleteButton QAbstractButton.clickedSignal $ \_ -> QDialog.accept delete

  revalidate del

  return del

runDelete :: UiDeletingItem -> IO DeletionResult
runDelete itemType = do
  del@Delete{delete = delete} <- initDelete itemType
  result <- toEnum <$> QDialog.exec delete
  valid <- isValid del

  return $ case result of
    QDialog.Accepted -> if valid then DoDelete else Cancel
    QDialog.Rejected -> Cancel

isValid :: Delete -> IO Bool
isValid Delete{..} = QAbstractButton.isChecked isSure

revalidate :: Delete -> IO ()
revalidate del@Delete{..} = isValid del >>= QWidget.setEnabled deleteButton

itemTypeFormat :: Format r (UiDeletingItem -> r)
itemTypeFormat = later $ \case
  UiDelWallet -> "wallet"
  UiDelAccount -> "account"

isSureToggled :: Delete -> Bool -> IO ()
isSureToggled del@Delete{..} _checked = do
  revalidate del
  when (itemType == UiDelWallet) $ do
    QWidget.adjustSize delete
