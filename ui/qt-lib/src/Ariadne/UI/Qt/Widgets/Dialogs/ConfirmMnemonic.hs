module Ariadne.UI.Qt.Widgets.Dialogs.ConfirmMnemonic
  ( runConfirmMnemonic
  ) where

import Universum

import qualified Data.Text as T
import Formatting

import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

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

data ConfirmMnemonic =
  ConfirmMnemonic
    { confirmMnemonic :: QDialog.QDialog
    , mnemonic :: [Text]
    , noOneLooks :: QCheckBox.QCheckBox
    , retypeMnemonic :: QLineEdit.QLineEdit
    , clearButton :: QPushButton.QPushButton
    , actionButton :: QPushButton.QPushButton
    }

initConfirmMnemonic :: [Text] -> IO ConfirmMnemonic
initConfirmMnemonic mnemonic = do
  confirmMnemonic <- QDialog.new
  layout <- createLayout confirmMnemonic

  let headerString :: String = "RECOVERY PHRASE"

  QWidget.setWindowTitle confirmMnemonic headerString

  header <- QLabel.newWithText headerString
  addHeader layout header

  instructionLabel <- QLabel.newWithText ("" :: String)
  QBoxLayout.addWidget layout instructionLabel

  noOneLooks <- QCheckBox.new
  QWidget.setSizePolicyRaw noOneLooks Maximum Maximum
  noOneLooksLabel <- QLabel.newWithText
    ("Make sure nobody looks into your screen unless you want them to have access to your funds." :: String)
  QLabel.setWordWrap noOneLooksLabel True
  QWidget.setMinimumSizeRaw noOneLooksLabel 600 30
  noOneLooksLayout <- QHBoxLayout.new
  QBoxLayout.addWidget noOneLooksLayout noOneLooks
  QBoxLayout.addWidget noOneLooksLayout noOneLooksLabel
  QBoxLayout.addLayout layout noOneLooksLayout

  --(retypeWidget, retypeLayout) <- createSubWidget
  --addSeparator retypeLayout

  --retypeLabel <- QLabel.newWithText . toString $ sformat
    --("Type " % itemTypeFormat % " name to confirm deletion") itemType
  --retypeName <- QLineEdit.new
  --addRow retypeLayout retypeLabel retypeName

  --QBoxLayout.addWidget layout retypeWidget
  --QWidget.hide retypeWidget

  buttonsLayout <- QHBoxLayout.new
  clearButton <- QPushButton.newWithText ("CLEAR" :: String)
  actionButton <- QPushButton.newWithText ("CONTINUE" :: String)
  QBoxLayout.addWidget buttonsLayout clearButton
  QBoxLayout.addWidget buttonsLayout actionButton
  QBoxLayout.addLayout layout buttonsLayout

  void $ setProperty clearButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  void $ setProperty actionButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  void $ setProperty clearButton ("styleRole" :: Text) ("secondaryButton" :: Text)

  let confirm = ConfirmMnemonic{..}

  --connect_ isSure QAbstractButton.toggledSignal $ isSureToggled del
  --connect_ retypeName QLineEdit.textChangedSignal $ \_ -> revalidate del
  --connect_ cancelButton QAbstractButton.clickedSignal $ \_ -> QDialog.reject delete
  --connect_ deleteButton QAbstractButton.clickedSignal $ \_ -> QDialog.accept delete

  --revalidate del

  return confirm

runConfirmMnemonic :: Text -> IO Bool
runConfirmMnemonic mnemonic = do
  confirm@ConfirmMnemonic{confirmMnemonic = confirmMnemonic} <- initConfirmMnemonic $ words mnemonic
  result <- toEnum <$> QDialog.exec confirmMnemonic
  valid <- isValid confirm

  return $ case result of
    QDialog.Accepted -> if valid then True else False
    QDialog.Rejected -> False

isValid :: ConfirmMnemonic -> IO Bool
isValid ConfirmMnemonic{..} = do
--  delIsSure <- QAbstractButton.isChecked isSure
--  delItemName <- T.strip . fromString <$> QLineEdit.text retypeName

  --return $ delIsSure && (itemType /= Wallet || delItemName == itemName)
  return True

--revalidate :: Delete -> IO ()
--revalidate del@Delete{..} = isValid del >>= QWidget.setEnabled deleteButton
