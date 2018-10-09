module Ariadne.UI.Qt.Widgets.Dialogs.Delete
       ( DeletionResult(..)
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
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UIConfig
import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.Dialogs.Util

data DeletionResult = DoDelete | Cancel deriving Eq

data Delete =
  Delete
    { delete :: QDialog.QDialog
    , isSure :: QCheckBox.QCheckBox
    , retypeWidget :: QWidget.QWidget
    , retypeName :: QLineEdit.QLineEdit
    , deleteButton :: QPushButton.QPushButton
    , itemType :: UiDeletingItem
    }

initDelete :: UiDeletingItem -> IO Delete
initDelete itemType = do
  delete <- QDialog.new
  layout <- createLayout delete

  let headerString = toString . T.toUpper $ deleteHeaderMkMessage itemTypeFormat itemType
      itemName = fromMaybe "this" $ itemTypeName itemType

  QWidget.setWindowTitle delete headerString

  header <- QLabel.newWithText headerString
  addHeader layout header

  warningLabel <- QLabel.newWithText . toString $
      deleteIntroMkMessage itemTypeFormat ("<b>" <> itemName <> "</b>") itemType
  QBoxLayout.addWidget layout warningLabel

  isSure <- createCheckBox layout CheckboxOnLeft $
      deleteSureMkMessage itemTypeFormat itemType

  (retypeWidget, retypeLayout) <- createSubWidget
  addSeparator retypeLayout

  retypeLabel <- QLabel.newWithText . toString $
      deleteRetypeMkMessage itemTypeFormat itemType
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

  setProperty cancelButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  setProperty deleteButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  setProperty cancelButton ("styleRole" :: Text) ("secondaryButton" :: Text)
  setProperty deleteButton ("styleRole" :: Text) ("dangerButton" :: Text)

  let del = Delete{..}

  connect_ isSure QAbstractButton.toggledSignal $ isSureToggled del
  connect_ retypeName QLineEdit.textChangedSignal $ \_ -> revalidate del
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
isValid Delete{..} = do
  delIsSure <- QAbstractButton.isChecked isSure
  delItemName <- T.strip . fromString <$> QLineEdit.text retypeName

  -- We do not ask to retype name for accounts
  -- (or anything that does not have a name)
  let isNameChecked = case itemType of
        UiDelAccount _ -> True
        UiDelWallet maybeName -> case maybeName of
          Nothing -> True
          Just itemName -> delItemName == itemName
  return $ delIsSure && isNameChecked

isDelWallet :: UiDeletingItem -> Bool
isDelWallet = \case
  UiDelWallet _ -> True
  _ -> False

revalidate :: Delete -> IO ()
revalidate del@Delete{..} = isValid del >>= QWidget.setEnabled deleteButton

itemTypeFormat :: Format r (UiDeletingItem -> r)
itemTypeFormat = later $ \case
  UiDelWallet _ -> "wallet"
  UiDelAccount _ -> "account"

itemTypeName :: UiDeletingItem -> Maybe Text
itemTypeName = \case
  UiDelWallet name -> name
  UiDelAccount name -> name

isSureToggled :: Delete -> Bool -> IO ()
isSureToggled del@Delete{..} checked = do
  revalidate del
  when (isDelWallet itemType && isJust (itemTypeName itemType)) $ do
    QWidget.setVisible retypeWidget checked
    QWidget.adjustSize delete
