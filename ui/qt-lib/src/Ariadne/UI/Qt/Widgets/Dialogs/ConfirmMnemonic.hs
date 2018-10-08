module Ariadne.UI.Qt.Widgets.Dialogs.ConfirmMnemonic
       ( ConfirmMnemonicResult(..)
       , runConfirmMnemonic
       ) where

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

  let headerString :: String = "RECOVERY PHRASE"

  QWidget.setWindowTitle confirmMnemonic headerString

  header <- QLabel.newWithText headerString
  addHeader layout header

  instructionLabel <- QLabel.new
  QLabel.setWordWrap instructionLabel True
  QWidget.setMinimumSizeRaw instructionLabel 600 30
  QBoxLayout.addWidget layout instructionLabel

  (noOneLooksWidget, noOneLooksLayout) <- createSubWidget
  noOneLooks <- createCheckBox noOneLooksLayout CheckboxOnLeft
    "Make sure nobody looks into your screen unless you want them to have access to your funds."
  QBoxLayout.addWidget layout noOneLooksWidget

  mnemonicWidget <- QLabel.new
  setProperty mnemonicWidget ("styleRole" :: Text) ("mnemonicDisplay" :: Text)
  QBoxLayout.addWidget layout mnemonicWidget
  QWidget.hide mnemonicWidget

  (retypeWidget, retypeLayout) <- createSubWidget
  addSeparator retypeLayout

  retypeLabel <- QLabel.newWithText ("<b>RECOVERY PHRASE</b>" :: String)
  retypeMnemonic <- QLineEdit.new
  addRow retypeLayout retypeLabel retypeMnemonic

  QBoxLayout.addWidget layout retypeWidget
  QWidget.hide retypeWidget

  (checksWidget, checksLayout) <- createSubWidget
  addSeparator checksLayout

  uMoneyOnDevice <- createCheckBox checksLayout CheckboxOnLeft
    "I understand that my money are held securely on this device only, not on the company servers"
  uMoved <- createCheckBox checksLayout CheckboxOnLeft
    "I understand that if this application is moved to another device or deleted,\
    \ my money can be only recovered with the backup phrase which was written down in a secure place"
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
  Before -> sformat
    ("On the following screen, you will see a set of " % int %
     " random words. This is your wallet backup phrase. It can be entered in any version\
     \ of Ariadne application in order to restore your wallet’s funds and private key.")
    mnemonicLength
  DisplayMnemonic ->
    "Please make sure you have carefully writen down your recovery phrase somewhere safe.\
    \ You will need this phrase later for next use and recover. Phrase is case sensitive."
  RetypeMnemonic -> "Type each word in the correct order to verify your recovery phrase."
  Done -> "You are done!" -- This will never actually be displayed

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
