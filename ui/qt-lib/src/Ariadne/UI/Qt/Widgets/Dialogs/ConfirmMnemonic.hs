module Ariadne.UI.Qt.Widgets.Dialogs.ConfirmMnemonic
  ( ConfirmMnemonicResult(..)
  , runConfirmMnemonic
  ) where

import qualified Data.Text as T
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
import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Widgets.Dialogs.Util

data ConfirmMnemonicResult = ConfirmMnemonicSuccess | ConfirmMnemonicFailure

data ConfirmationState
  = Before
  | DisplayMnemonic
  | RetypeMnemonic
  | Done
  deriving (Eq)

data ConfirmMnemonic =
  ConfirmMnemonic
    { confirmMnemonic :: QDialog.QDialog
    , mnemonic :: [Text]
    , instructionLabel :: QLabel.QLabel
    , noOneLooks :: QCheckBox.QCheckBox
    , retypeMnemonic :: QLineEdit.QLineEdit
    , clearButton :: QPushButton.QPushButton
    , actionButton :: QPushButton.QPushButton
    , confirmationState :: IORef ConfirmationState
    , noOneLooksWidget :: QWidget.QWidget
    , mnemonicWidget :: QLabel.QLabel
    , retypeWidget :: QWidget.QWidget
    , checksWidget :: QWidget.QWidget
    , uMoneyOnDevice :: QCheckBox.QCheckBox
    , uMoved :: QCheckBox.QCheckBox
    }

initConfirmMnemonic :: [Text] -> IO ConfirmMnemonic
initConfirmMnemonic mnemonic = do
  confirmMnemonic <- QDialog.new
  layout <- createLayout confirmMnemonic

  let headerString = toString $ T.toUpper mnemonicHeaderMessage

  QWidget.setWindowTitle confirmMnemonic headerString

  header <- QLabel.newWithText headerString
  addHeader layout header

  instructionLabel <- QLabel.new
  QLabel.setWordWrap instructionLabel True
  QWidget.setMinimumSizeRaw instructionLabel 600 30
  QBoxLayout.addWidget layout instructionLabel

  (noOneLooksWidget, noOneLooksLayout) <- createSubWidget
  noOneLooks <- createCheckBox noOneLooksLayout CheckboxOnLeft mnemonicNoLooksMessage
  QBoxLayout.addWidget layout noOneLooksWidget

  mnemonicWidget <- QLabel.new
  setProperty mnemonicWidget ("styleRole" :: Text) ("mnemonicDisplay" :: Text)
  QBoxLayout.addWidget layout mnemonicWidget
  QWidget.hide mnemonicWidget

  (retypeWidget, retypeLayout) <- createSubWidget
  addSeparator retypeLayout

  retypeLabel <- QLabel.newWithText $ "<b>" <> headerString <> "</b>"
  retypeMnemonic <- QLineEdit.new
  addRow retypeLayout retypeLabel retypeMnemonic

  QBoxLayout.addWidget layout retypeWidget
  QWidget.hide retypeWidget

  (checksWidget, checksLayout) <- createSubWidget
  addSeparator checksLayout

  uMoneyOnDevice <- createCheckBox checksLayout CheckboxOnLeft mnemonicOnDeviceMessage
  uMoved <- createCheckBox checksLayout CheckboxOnLeft mnemonicAppMovedMessage
  QBoxLayout.addWidget layout checksWidget

  buttonsLayout <- QHBoxLayout.new
  clearButton <- QPushButton.newWithText ("CLEAR" :: String)
  actionButton <- QPushButton.newWithText ("CONTINUE" :: String)
  QBoxLayout.addWidget buttonsLayout clearButton
  QBoxLayout.addWidget buttonsLayout actionButton
  QBoxLayout.addLayout layout buttonsLayout

  setProperty clearButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  setProperty actionButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  setProperty clearButton ("styleRole" :: Text) ("secondaryButton" :: Text)

  QWidget.hide clearButton

  confirmationState <- newIORef Before

  let
    confirm = ConfirmMnemonic{..}

  connect_ clearButton QAbstractButton.clickedSignal $ \_ -> QLineEdit.clear retypeMnemonic
  connect_ actionButton QAbstractButton.clickedSignal $ doAction confirm
  connect_ noOneLooks QAbstractButton.toggledSignal $ \_ -> revalidate confirm
  connect_ retypeMnemonic QLineEdit.textChangedSignal $ \_ -> revalidate confirm
  connect_ uMoneyOnDevice QAbstractButton.toggledSignal $ \_ -> revalidate confirm
  connect_ uMoved QAbstractButton.toggledSignal $ \_ -> revalidate confirm

  revalidate confirm
  setState confirm Before

  return confirm

runConfirmMnemonic :: [Text] -> IO ConfirmMnemonicResult
runConfirmMnemonic mnemonic = do
  confirm@ConfirmMnemonic{confirmMnemonic = confirmMnemonic} <- initConfirmMnemonic mnemonic
  result <- toEnum <$> QDialog.exec confirmMnemonic
  valid <- isValid confirm

  return $ case result of
    QDialog.Accepted -> if valid then ConfirmMnemonicSuccess else ConfirmMnemonicFailure
    QDialog.Rejected -> ConfirmMnemonicFailure

stateToInstruction :: Int -> ConfirmationState -> Text
stateToInstruction mnemonicLength = \case
  Before -> mnemonicBeforeMkMessage mnemonicLength
  DisplayMnemonic -> mnemonicDisplayMessage
  RetypeMnemonic -> mnemonicRetypeMessage
  Done -> mnemonicDoneMessage -- This will never actually be displayed

stateToActionName :: ConfirmationState -> Text
stateToActionName = \case
  Before -> "CONTINUE"
  DisplayMnemonic -> "YES, I'VE WRITTEN IT DOWN"
  RetypeMnemonic -> "CONFIRM"
  Done -> "CONFIRM" -- Just in case

setState :: ConfirmMnemonic -> ConfirmationState -> IO ()
setState ConfirmMnemonic{..} newState = do
  let
    instruction = stateToInstruction (length mnemonic) newState
    actionName = stateToActionName newState

  QLabel.setText instructionLabel $ toString instruction
  QAbstractButton.setText actionButton $ toString actionName

doAction :: ConfirmMnemonic -> Bool -> IO ()
doAction confirm@ConfirmMnemonic{..} _checked = do
  currentState <- readIORef confirmationState

  newState <- case currentState of
      Before -> do
        QWidget.hide noOneLooksWidget
        QLabel.setText mnemonicWidget $ toString $ unwords mnemonic
        QWidget.show mnemonicWidget

        return DisplayMnemonic
      DisplayMnemonic -> do
        QWidget.hide mnemonicWidget
        QWidget.show retypeWidget
        QWidget.show clearButton

        return RetypeMnemonic
      RetypeMnemonic -> do
        QLineEdit.setReadOnly retypeMnemonic True
        QWidget.show checksWidget
        QDialog.accept confirmMnemonic
        -- This won't change anything after accept anyway
        return Done
      Done -> return Done

  writeIORef confirmationState newState
  setState confirm newState
  revalidate confirm

isRetypedMnemonicCorrect :: ConfirmMnemonic -> IO Bool
isRetypedMnemonicCorrect ConfirmMnemonic{..} = do
  retyped <- words . fromString <$> QLineEdit.text retypeMnemonic
  return $ retyped == mnemonic

isValid :: ConfirmMnemonic -> IO Bool
isValid confirm@ConfirmMnemonic{..} = do
  currentState <- readIORef confirmationState

  case currentState of
    Before -> do
      QAbstractButton.isChecked noOneLooks
    DisplayMnemonic -> do
      return True
    RetypeMnemonic -> do
      retypedCorrect <- isRetypedMnemonicCorrect confirm
      moneyOnDevice <- QAbstractButton.isChecked uMoneyOnDevice
      moved <- QAbstractButton.isChecked uMoved

      return $ retypedCorrect && moneyOnDevice && moved
    Done -> return True

revalidate :: ConfirmMnemonic -> IO ()
revalidate confirm@ConfirmMnemonic{..} = do
  isValid confirm >>= QWidget.setEnabled actionButton
  retypedCorrect <- isRetypedMnemonicCorrect confirm
  currentState <- readIORef confirmationState

  QWidget.setVisible checksWidget $
    currentState == RetypeMnemonic && retypedCorrect

  QWidget.adjustSize confirmMnemonic
