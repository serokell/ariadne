module Ariadne.UI.Qt.Widgets.Dialogs.Delete
  ( DeletingEntity(..)
  , DeletionResult
  , runDelete
  ) where

import Universum

import qualified Data.Text as T

import Graphics.UI.Qtah.Signal (connect_)

import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Widgets.Dialogs.Util
import Ariadne.UI.Qt.UI

data DeletingEntity = Wallet | Account
data DeletionResult = DoDelete | Cancel

data Delete =
  Delete
    { delete :: QDialog.QDialog
    , isSure :: QCheckBox.QCheckBox
    , entityName :: Text
    , retypeName :: QLineEdit.QLineEdit
    , deleteButton :: QPushButton.QPushButton
    }

initDelete :: DeletingEntity -> Text -> IO Delete
initDelete entity entityName = do
  delete <- QDialog.new
  layout <- createLayout delete

  header <- QLabel.newWithText ("DELETE" :: String)
  addHeader layout header


  isSureLabel <- QLabel.newWithText ("Make sure" :: String)
  isSure <- QCheckBox.new
  addRow layout isSureLabel isSure

  (retypeWidget, retypeLayout) <- createSubWidget
  addSeparator retypeLayout

  retypeLabel <- QLabel.newWithText ("Type to confirm" :: String)
  retypeName <- QLineEdit.new
  addRow retypeLayout retypeLabel retypeName

  QBoxLayout.addWidget layout retypeWidget

  let del = Delete{..}

  connect_ retypeName QLineEdit.textChangedSignal $ \_ -> revalidate del

  return del

runDelete :: DeletingEntity -> Text -> IO DeletionResult
runDelete entity entityName = do
  del@Delete{..} <- initDelete entity entityName
  result <- fmap toEnum . liftIO . QDialog.exec $ delete
  valid <- isValid del

  return $ case result of
    QDialog.Accepted -> if valid then DoDelete else Cancel
    QDialog.Rejected -> Cancel

isValid :: Delete -> IO Bool
isValid Delete{..} = do
  delIsSure <- QAbstractButton.isChecked isSure
  delEntityName <- T.strip . fromString <$> QLineEdit.text retypeName

  return $ delIsSure && delEntityName == entityName

revalidate :: Delete -> IO ()
revalidate del@Delete{..} = isValid del >>= QWidget.setEnabled deleteButton
