module Ariadne.UI.Qt.Widgets.Dialogs.Send
  ( SendInputs(..)
  , SendOptions(..)
  , SendResult(..)
  , runSend
  ) where

import Control.Lens (has)
import Data.Scientific (Scientific, normalize)
import qualified Data.Text as T
import Data.Validation (Validation(..), ensure, validate, _Success, bindValidation)

import Graphics.UI.Qtah.Signal (connect_)

import Graphics.UI.Qtah.Core.HPoint (HPoint(..))
import Graphics.UI.Qtah.Core.HSize (HSize(..))
import qualified Graphics.UI.Qtah.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Core.QLocale as QLocale
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import Graphics.UI.Qtah.Core.Types (QtWindowType(Popup))
import qualified Graphics.UI.Qtah.Event as Event
import qualified Graphics.UI.Qtah.Gui.QDoubleValidator as QDoubleValidator
import qualified Graphics.UI.Qtah.Gui.QMouseEvent as QMouseEvent
import qualified Graphics.UI.Qtah.Gui.QValidator as QValidator
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Util
import Ariadne.UI.Qt.Widgets.Dialogs.Util
import Ariadne.UI.Qt.Widgets.Dialogs.Validation

data SendInputs = SendInputsSingle Word | SendInputsMulti [UiAccountInfo]

data Error
  = Amount
  | AmountEmpty
  | Address Text
  | AddressEmpty
  deriving (Eq, Show)

type Errors = [Error]

data SendOptions =
  SendOptions
    { soAddress :: Text
    , soAmount :: Scientific
    , soAccounts :: [Word]
    }

data SendResult = SendSuccess SendOptions | SendCancel

data AccountCheckbox =
  AccountCheckbox
    { aIdx :: Word
    , checkbox :: QCheckBox.QCheckBox
    , accountName :: Text
    }

data AccountSelector =
  AccountSelector
    { dropdownWidget :: QWidget.QWidget
    , checkboxes :: [AccountCheckbox]
    }

data Send =
  Send
    { send :: QDialog.QDialog
    , amountEdit :: QLineEdit.QLineEdit
    , addressEdit :: QLineEdit.QLineEdit
    , sendButton :: QPushButton.QPushButton
    , fromDisplay :: QLabel.QLabel
    , accountSelector :: Either Word AccountSelector
    , uiWalletFace :: UiWalletFace
    , validations :: Validations
    }

initSend :: UiWalletFace -> SendInputs -> IO Send
initSend uiWalletFace@UiWalletFace{..} sendInputs = do
  send <- QDialog.new
  layout <- createLayout send

  QWidget.setWindowTitle send ("SEND" :: String)

  amountLabel <- QLabel.newWithText ("<b>AMOUNT</b>" :: String)
  amountLayout <- QHBoxLayout.new
  amountEdit <- QLineEdit.new
  unitLabel <- QLabel.newWithText $ toString $ unitToHtml ADA
  QObject.setObjectName amountEdit ("sendAmountEdit" :: String)
  QLineEdit.setPlaceholderText amountEdit ("0" :: String)

  cLocale <- QLocale.newWithName ("C" :: String)
  QLocale.setNumberOptions cLocale QLocale.RejectGroupSeparator
  validator <- QDoubleValidator.new
  QValidator.setLocale validator cLocale
  QDoubleValidator.setBottom validator 0
  QDoubleValidator.setDecimals validator uiCoinPrecision
  QLineEdit.setValidator amountEdit validator

  QBoxLayout.addWidget amountLayout amountEdit
  QBoxLayout.addWidget amountLayout unitLabel
  QBoxLayout.setSpacing amountLayout 0
  addRowLayout layout amountLabel amountLayout
  addSeparator layout

  fromAccountsLabel <- QLabel.newWithText ("<b>FROM</b>" :: String)
  fromDisplay <- QLabel.newWithText ("select account..." :: String)
  QObject.setObjectName fromDisplay ("accountsSelectorDisplay" :: String)

  case sendInputs of
    SendInputsSingle {} -> pass
    SendInputsMulti {} -> do
      addRow layout fromAccountsLabel fromDisplay
      addSeparator layout

  addressLabel <- QLabel.newWithText ("<b>ADDRESS</b>" :: String)
  addressEdit <- QLineEdit.new

  addRow layout addressLabel addressEdit
  addSeparator layout

  sendButton <- QPushButton.newWithText ("SEND" :: String)
  void $ setProperty sendButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)

  QBoxLayout.addWidget layout sendButton

  QBoxLayout.addStretch layout

  accountSelector <- case sendInputs of
    SendInputsSingle aIdx -> return $ Left aIdx
    SendInputsMulti uacis -> do
      selector@AccountSelector{..} <- createAccountSelector uacis
      QWidget.setParent dropdownWidget send
      QWidget.hide dropdownWidget
      -- This will magically make qt hide dropdown when user clicks outside it
      QWidget.setWindowFlags dropdownWidget Popup

      QWidget.setMinimumWidth fromDisplay =<< QWidget.width dropdownWidget

      return $ Right selector

  validations <- createValidations send $
    [ ("Wrong address", QWidget.cast addressEdit)
    , ("Wrong amount", QWidget.cast amountEdit)
    ]

  let s = Send{..}

  connect_ sendButton QAbstractButton.clickedSignal $ \_ -> QDialog.accept send
  connect_ amountEdit QLineEdit.textChangedSignal $ \_ -> revalidate s
  connect_ addressEdit QLineEdit.textChangedSignal $ \_ -> revalidate s

  whenRight accountSelector $ \as@AccountSelector{..} -> do
    void $ Event.onEvent fromDisplay $
      \(ev :: QMouseEvent.QMouseEvent) -> do
        eventType <- QEvent.eventType ev
        if eventType == QEvent.MouseButtonRelease
          then toggleDropDown dropdownWidget s $> True
          else return False

    void $ Event.onEvent send $
      \(ev :: QEvent.QEvent) -> do
        eventType <- QEvent.eventType ev
        dropdownVisible <- QWidget.isVisible dropdownWidget
        if eventType == QEvent.Resize && dropdownVisible
          then moveDropDown dropdownWidget s $> True
          else return False

    for_ checkboxes $
      \AccountCheckbox{..} -> connect_ checkbox QAbstractButton.toggledSignal $ \_ -> accountsSelected as s

  revalidate s

  return s

runSend :: UiWalletFace -> SendInputs -> IO SendResult
runSend uiWalletFace sendInputs = do
  s@Send{send = send} <- initSend uiWalletFace sendInputs
  result <- toEnum <$> QDialog.exec send

  options <- fillSendOptions s

  return $ case result of
    QDialog.Accepted -> case options of
      Success so -> SendSuccess so
      Failure _ -> SendCancel
    QDialog.Rejected -> SendCancel

fillSendOptions :: Send -> IO (Validation Errors SendOptions)
fillSendOptions Send{..} = do
  amount <- fromString <$> QLineEdit.text amountEdit
  address <- fromString <$> QLineEdit.text addressEdit

  accounts <- case accountSelector of
    Left aIdx -> return [aIdx]
    Right AccountSelector{..} -> map aIdx <$> filterM (QAbstractButton.isChecked . checkbox) checkboxes

  let UiWalletFace{..} = uiWalletFace

  let
    vAddress = validate [AddressEmpty] (not . null) address `bindValidation` \a ->
      case uiValidateAddress a of
        Just reason -> Failure [Address reason]
        Nothing -> Success address
    vAmount =
      validate [AmountEmpty] (not . null) amount `bindValidation` \a ->
      maybe (Failure [Amount]) (Success . normalize) (readMaybe a) &
      ensure [Amount] uiValidateCoin
    vOptions = SendOptions <$> vAddress <*> vAmount <*> pure accounts

  return vOptions

isSuccess :: Validation e b -> Bool
isSuccess = has _Success

isValid :: Send -> IO Bool
isValid = fmap isSuccess . fillSendOptions

revalidate :: Send -> IO ()
revalidate s@Send{..} = do
  validation <- fillSendOptions s
  let valid = isSuccess validation

  showErrorsV validations validation (errorToWidget s)

  QWidget.setEnabled sendButton valid

errorToWidget :: Send -> Error -> Maybe (Maybe Text, QWidget.QWidget)
errorToWidget Send{..} = \case
  Amount -> Just (Nothing, QWidget.cast amountEdit)
  Address reason -> Just (Just reason, QWidget.cast addressEdit)
  _ -> Nothing

createAccountSelector :: [UiAccountInfo] -> IO AccountSelector
createAccountSelector uacis = do
  dropdownWidget <- QWidget.new
  layout <- QVBoxLayout.new
  QWidget.setLayout dropdownWidget layout
  QObject.setObjectName dropdownWidget ("accountsSelector" :: String)

  checkboxes <- forM uacis $ createAccountCheckbox layout

  QWidget.adjustSize dropdownWidget

  return AccountSelector{..}

createAccountCheckbox :: QVBoxLayout.QVBoxLayout -> UiAccountInfo -> IO AccountCheckbox
createAccountCheckbox layout UiAccountInfo{..} = do
  rowLayout <- QHBoxLayout.new
  (checkbox, label) <- createCheckBoxWithLabel $ fromMaybe "" uaciLabel
  balance <- QLabel.newWithText $ toString $
    let (balance, unit) = uaciBalance in balance <> " " <> unitToHtmlSmall unit

  QBoxLayout.addWidget rowLayout checkbox
  QBoxLayout.addWidget rowLayout label
  QBoxLayout.addStretch rowLayout
  QBoxLayout.addWidget rowLayout balance

  QBoxLayout.addLayout layout rowLayout

  let
    aIdx = head uaciPath
    accountName = fromMaybe "" uaciLabel

  return AccountCheckbox{..}

toggleDropDown :: QWidget.QWidget -> Send -> IO ()
toggleDropDown dropdownWidget s@Send{..} = do
  visible <- QWidget.isVisible dropdownWidget
  QWidget.setVisible dropdownWidget $ not visible
  moveDropDown dropdownWidget s

moveDropDown :: QWidget.QWidget -> Send -> IO ()
moveDropDown dropdownWidget Send{..} = do
  QWidget.raise dropdownWidget
  -- dropdownWidget is a Popup window and thus uses global coordinates
  -- therefore we need to translate parent-relative label position to global screen coordinates
  HPoint{x = labelX, y = labelY} <- QWidget.mapToGlobal send =<< QWidget.pos fromDisplay
  HSize{width = labelWidth, height = labelHeight} <- QWidget.size fromDisplay
  QWidget.move dropdownWidget $ HPoint{x = labelX, y = labelY + labelHeight}
  QWidget.adjustSize dropdownWidget

  HSize{width = widgetWidth, height = widgetHeight} <- QWidget.size dropdownWidget
  -- Make account widget occupy as much space as is available
  when (widgetWidth < labelWidth) $ QWidget.resizeRaw dropdownWidget labelWidth widgetHeight

selectAccountText :: Text
selectAccountText = "select accounts..."

accountsSelected :: AccountSelector -> Send -> IO ()
accountsSelected AccountSelector{..} s@Send{..} = do
  revalidate s

  accountNames <- T.intercalate " + " . map accountName <$>
    filterM (QAbstractButton.isChecked . checkbox) checkboxes
  QLabel.setText fromDisplay $ toString $
    if not $ null accountNames then accountNames else selectAccountText
