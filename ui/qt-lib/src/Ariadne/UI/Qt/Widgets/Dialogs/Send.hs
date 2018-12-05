module Ariadne.UI.Qt.Widgets.Dialogs.Send
  ( Send(..)
  , SendInputs(..)
  , SendOptions(..)
  , SendResult(..)
  , initSend
  , runSend
  , changeTotalAmount
  , showInvalidArgumentsError
  ) where

import Control.Lens (has)
import Data.Scientific (Scientific, normalize)
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Validation
  (Validation(..), bindValidation, ensure, validate, _Success)

import Graphics.UI.Qtah.Core.Types (QtWindowType(Popup), alignRight)
import Graphics.UI.Qtah.Signal (connect_)

import Graphics.UI.Qtah.Core.HPoint (HPoint(..))
import Graphics.UI.Qtah.Core.HSize (HSize(..))
import qualified Graphics.UI.Qtah.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Core.QLocale as QLocale
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Gui.QDoubleValidator as QDoubleValidator
import qualified Graphics.UI.Qtah.Gui.QValidator as QValidator
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Widgets.QComboBox as QComboBox
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Util
import Ariadne.UI.Qt.Widgets.Dialogs.Util
import Ariadne.UI.Qt.Widgets.Dialogs.Validation

data SendInputs = SendInputsSingle Word | SendInputsMulti [UiAccountInfo]
data SendMode = SendBasic | SendAdvanced deriving (Show, Eq, Enum, Bounded)

data Error
  = Amount
  | AmountEmpty
  | Address Text
  | AddressEmpty
  deriving (Eq, Show)

type Errors = [(Receiver, Error)]

data SendOptions =
  SendOptions
    { soAddresses :: [Text]
    , soAmounts :: [Scientific]
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
    , modeSelector :: QComboBox.QComboBox
    , receivers :: IORef [Receiver]
    , singleReceiver :: Receiver
    , receiversLayout :: QVBoxLayout.QVBoxLayout
    , receiversWidget :: QWidget.QWidget
    , sendButton :: QPushButton.QPushButton
    , addReceiverButton :: QPushButton.QPushButton
    , fromDisplay :: QLabel.QLabel
    , totalAmount :: QLabel.QLabel
    , totalUnit :: QLabel.QLabel
    , errorTextLabel :: QLabel.QLabel
    , accountSelector :: Either Word AccountSelector
    , uiWalletFace :: UiWalletFace
    }

data Receiver =
  Receiver
    { receiverWidget :: QWidget.QWidget
    , amountEdit :: QLineEdit.QLineEdit
    , addressEdit :: QLineEdit.QLineEdit
    , removeButton :: QPushButton.QPushButton
    , validations :: Validations
    }

sendModeName :: SendMode -> Text
sendModeName SendBasic = "SEND"
sendModeName SendAdvanced = "SEND (ADVANCED)"

createModeSelector :: IO QComboBox.QComboBox
createModeSelector = do
  modeSelector <- QComboBox.new
  for_ [minBound..maxBound] $ \mode ->
    QComboBox.addItem modeSelector $ toString $ sendModeName mode
  return modeSelector

initSend :: UiLangFace -> UiWalletFace -> SendInputs -> IO Send
initSend uiLangFace@UiLangFace{..} uiWalletFace@UiWalletFace{..} sendInputs = do
  send <- QDialog.new
  QObject.setObjectName send ("sendDialog" :: String)
  layout <- createLayout send

  QWidget.setWindowTitle send ("SEND" :: String)
  modeSelector <- createModeSelector
  addHeader layout modeSelector

  -- Create widget for SendBasic which contains single receiver
  singleReceiver <- createSingleReceiver uiWalletFace
  QBoxLayout.addWidget layout $ receiverWidget singleReceiver

  -- Create widget for SendAdvanced which contains multiple recevivers and top bar with labels
  (receiversWidget, receiversLayout) <- createMulptipleReceivers
  QBoxLayout.addWidget layout receiversWidget
  QWidget.setVisible receiversWidget False

  receivers <- newIORef []

  fromAccountsLabel <- QLabel.newWithText ("<b>FROM</b>" :: String)
  fromDisplay <- QLabel.newWithText ("select account..." :: String)
  QObject.setObjectName fromDisplay ("accountsSelectorDisplay" :: String)

  case sendInputs of
    SendInputsSingle {} -> pass
    SendInputsMulti {} -> do
      addRow layout fromAccountsLabel fromDisplay
      addSeparator layout

  errorTextLabel <- QLabel.new
  QLayout.addWidget layout errorTextLabel
  QWidget.setVisible errorTextLabel False

  totalLabel <- QLabel.newWithText ("<b>TOTAL</b>" :: String)
  totalAmount <- QLabel.new
  totalUnit <- QLabel.new
  total <- QWidget.new
  totalLayout <- QHBoxLayout.new
  QWidget.setLayout total totalLayout
  QLayout.addWidget totalLayout totalAmount
  QLayout.addWidget totalLayout totalUnit
  QLayout.setAlignment totalLayout alignRight
  QObject.setObjectName totalAmount ("amountLabel" :: String)
  QObject.setObjectName totalUnit ("amountLabel" :: String)
  addRow layout totalLabel total

  addReceiverButton <- QPushButton.new
  QWidget.resizeRaw addReceiverButton 36 36
  void $ setProperty addReceiverButton ("styleRole" :: Text) ("addButton" :: Text)
  QWidget.setParent addReceiverButton send
  QWidget.setVisible addReceiverButton False

  sendButton <- QPushButton.newWithText ("NEXT" :: String)
  void $ setProperty sendButton ("dialogButtonRole" :: Text) ("dialogAction" :: Text)
  buttonLayout <- QHBoxLayout.new
  QBoxLayout.addStretch buttonLayout
  QBoxLayout.addWidget buttonLayout sendButton
  QBoxLayout.addStretch buttonLayout
  QLayout.setContentsMarginsRaw buttonLayout 268 0 268 0

  QBoxLayout.addLayout layout buttonLayout
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

  let s = Send{..}

  createNewReceiver uiLangFace s
  changeTotalAmount s "0" "ADA"

  -- Connect single receiver line edits
  connect_ (amountEdit singleReceiver) QLineEdit.textChangedSignal $ \_ -> do
    revalidate s
    recalcTotal uiLangFace s
  connect_ (addressEdit singleReceiver) QLineEdit.textChangedSignal $ \_ -> revalidate s

  connect_ sendButton QAbstractButton.clickedSignal $ \_ -> QDialog.accept send
  connect_ addReceiverButton QAbstractButton.clickedSignal $ \_ -> do
    -- To prohibit adding one more receiver editor if not all receivers are filled
    whenM (QWidget.isEnabled sendButton) $ createNewReceiver uiLangFace s
  connect_ modeSelector QComboBox.activatedSignal $ \ix -> modeChanged uiLangFace s $ toEnum ix

  whenRight accountSelector $ \as@AccountSelector{..} -> do
    onEventType fromDisplay $ \case
      QEvent.MouseButtonRelease -> toggleDropDown dropdownWidget s
      _ -> pass

    onEventType send $ \case
      QEvent.Resize -> whenM (QWidget.isVisible dropdownWidget) $ moveDropDown dropdownWidget s
      _ -> pass

    for_ checkboxes $
      \AccountCheckbox{..} -> connect_ checkbox QAbstractButton.toggledSignal $ \_ -> accountsSelected as s

  onEventType send $ \case
    QEvent.Resize -> moveAddReceiverButton s
    _ -> pass

  QWidget.adjustSize send

  revalidate s

  return s

modeChanged :: UiLangFace -> Send -> SendMode -> IO ()
modeChanged uiLangFace s@Send{..} mode = do
  let isBasic = mode == SendBasic
  QWidget.setVisible receiversWidget $ not isBasic
  QWidget.setVisible addReceiverButton $ not isBasic
  QWidget.setVisible (receiverWidget singleReceiver) isBasic
  QWidget.adjustSize send
  recalcTotal uiLangFace s
  revalidate s

runSend :: Send -> IO SendResult
runSend s@Send{..} = do
  result <- toEnum <$> QDialog.exec send

  options <- fillSendOptions s

  return $ case result of
    QDialog.Accepted -> case options of
      Success so -> SendSuccess so
      Failure _ -> SendCancel
    QDialog.Rejected -> SendCancel

fillSendOptions :: Send -> IO (Validation Errors SendOptions)
fillSendOptions s@Send{..} = do
  enabledReceivers <- getEnabledReceivers s
  amounts <- mapM (\r'@Receiver{..} -> (r', ) <$> (fromString <$> QLineEdit.text amountEdit)) enabledReceivers
  addresses <- mapM (\r'@Receiver{..} -> (r', ) <$> (fromString <$> QLineEdit.text addressEdit)) enabledReceivers
  accounts <- case accountSelector of
    Left aIdx -> return [aIdx]
    Right AccountSelector{..} -> map aIdx <$> filterM (QAbstractButton.isChecked . checkbox) checkboxes

  let UiWalletFace{..} = uiWalletFace
  let
    vAddresses = for addresses (\(r', address) -> validate [(r', AddressEmpty)] (not . null) address `bindValidation` \a ->
      case uiValidateAddress a of
        Just reason -> Failure [(r', Address reason)]
        Nothing -> Success address)
    vAmounts = for amounts (\(r', amount) ->
      validate [(r', AmountEmpty)] (not . null) amount `bindValidation` \a ->
      maybe (Failure [(r', Amount)]) (Success . normalize) (readMaybe a) &
      ensure [(r', Amount)] uiValidateCoin)
    vAccounts = Success accounts
    vOptions = SendOptions <$> vAddresses <*> vAmounts <*> vAccounts

  return vOptions

isSuccess :: Validation e b -> Bool
isSuccess = has _Success

isValid :: Send -> IO Bool
isValid = fmap isSuccess . fillSendOptions

revalidate :: Send -> IO ()
revalidate s@Send{..} = do
  enabledReceivers <- getEnabledReceivers s

  validation <- fillSendOptions s
  let valid = isSuccess validation

  mapM_ (\r'@Receiver{..} ->
    showErrorsV validations (filteredValidation validation r') errorToWidget) enabledReceivers

  QWidget.setEnabled addReceiverButton valid
  QWidget.setEnabled sendButton valid

filteredValidation :: Validation Errors a -> Receiver -> Validation Errors a
filteredValidation (Success x)  _ = Success x
filteredValidation (Failure l) r =
  Failure $ filter (\(r', _) -> receiverWidget r == receiverWidget r') l

errorToWidget :: (Receiver, Error) -> Maybe (Maybe Text, QWidget.QWidget)
errorToWidget (Receiver{..}, e) =
  case e of
    Amount -> Just (Nothing, QWidget.cast amountEdit)
    Address reason -> Just (Just reason, QWidget.cast addressEdit)
    _ -> Nothing

createValidator :: UiWalletFace -> IO QDoubleValidator.QDoubleValidator
createValidator UiWalletFace{..} = do
  cLocale <- QLocale.newWithName ("C" :: String)
  QLocale.setNumberOptions cLocale QLocale.RejectGroupSeparator
  validator <- QDoubleValidator.new
  QValidator.setLocale validator cLocale
  QDoubleValidator.setBottom validator 0
  QDoubleValidator.setDecimals validator uiCoinPrecision
  return validator

createAmountEdit :: IO QLineEdit.QLineEdit
createAmountEdit = do
  amountEdit <- QLineEdit.new
  QObject.setObjectName amountEdit ("sendAmountEdit" :: String)
  QLineEdit.setPlaceholderText amountEdit ("0" :: String)
  return amountEdit

createUnitLabel :: IO QLabel.QLabel
createUnitLabel = do
  unitLabel <- QLabel.newWithText ("ADA" :: String)
  QObject.setObjectName unitLabel ("amountLabel" :: String)
  return unitLabel

createAccountSelector :: [UiAccountInfo] -> IO AccountSelector
createAccountSelector uacis = do
  dropdownWidget <- QWidget.new
  layout <- QVBoxLayout.new
  QWidget.setLayout dropdownWidget layout
  QObject.setObjectName dropdownWidget ("accountsSelector" :: String)

  checkboxes <- forM uacis $ createAccountCheckbox layout

  QWidget.adjustSize dropdownWidget

  return AccountSelector{..}

createSingleReceiver :: UiWalletFace -> IO Receiver
createSingleReceiver uiWalletFace = do
  receiverWidget <- QWidget.new
  receiverLayout <- QVBoxLayout.new

  QLayout.setContentsMarginsRaw receiverLayout 0 0 0 0
  QBoxLayout.setSpacing receiverLayout 18
  basicAddressLabel <- QLabel.newWithText ("<b>RECEIVER</b>" :: String)
  basicAmountLabel <- QLabel.newWithText ("<b>AMOUNT</b>" :: String)
  amountLayout <- QHBoxLayout.new

  amountEdit <- createAmountEdit
  unitLabel <- createUnitLabel

  validator <- createValidator uiWalletFace
  QLineEdit.setValidator amountEdit validator

  QBoxLayout.addWidget amountLayout amountEdit
  QBoxLayout.addWidget amountLayout unitLabel
  QBoxLayout.setSpacing amountLayout 0
  addRowLayout receiverLayout basicAmountLabel amountLayout
  addSeparator receiverLayout
  addressEdit <- QLineEdit.new
  QLineEdit.setPlaceholderText addressEdit ("Type or paste receiver address here" :: String)
  addRow receiverLayout basicAddressLabel addressEdit
  addSeparator receiverLayout
  -- stub
  removeButton <- QPushButton.new
  QWidget.setLayout receiverWidget receiverLayout

  validations <- createValidations receiverWidget $
    [ ("Wrong address", QWidget.cast addressEdit)
    , ("Wrong amount", QWidget.cast amountEdit)
    ]

  let singleReceiver = Receiver{..}

  return singleReceiver

createMulptipleReceivers :: IO (QWidget.QWidget, QVBoxLayout.QVBoxLayout)
createMulptipleReceivers = do
  receiversWidget <- QWidget.new
  receiversLayout <- QVBoxLayout.new

  topBarWidget <- QWidget.new
  addressLabel <- QLabel.newWithText ("<b>RECEIVER</b>" :: String)
  amountLabel <- QLabel.newWithText ("<b>AMOUNT</b>" :: String)
  QLabel.setAlignment amountLabel alignRight
  rowLayout <- QHBoxLayout.new
  QLayout.setContentsMarginsRaw rowLayout 18 0 64 0
  QBoxLayout.addWidget rowLayout addressLabel
  QBoxLayout.addWidget rowLayout amountLabel
  QWidget.setLayout topBarWidget rowLayout
  QBoxLayout.addWidget receiversLayout topBarWidget

  QLayout.setContentsMarginsRaw receiversLayout 0 0 0 0
  QWidget.setLayout receiversWidget receiversLayout
  return (receiversWidget, receiversLayout)

createNewReceiver :: UiLangFace -> Send -> IO ()
createNewReceiver uiLangFace@UiLangFace{..} s@Send{..} = do
  receiverWidget <- QWidget.new
  layout <- createLayout receiverWidget
  QLayout.setContentsMarginsRaw layout 0 18 0 18

  amountEdit <- createAmountEdit
  unitLabel <- createUnitLabel

  let UiWalletFace{..} = uiWalletFace

  validator <- createValidator uiWalletFace
  QLineEdit.setValidator amountEdit validator

  amountLayout <- QHBoxLayout.new
  QBoxLayout.addWidget amountLayout amountEdit
  QBoxLayout.addWidget amountLayout unitLabel
  removeButton <- QPushButton.new
  void $ setProperty removeButton ("styleRole" :: Text) ("removeButton" :: Text)
  QBoxLayout.addWidget amountLayout removeButton
  QBoxLayout.setSpacing amountLayout 2
  addressEdit <- QLineEdit.new
  QLineEdit.setPlaceholderText addressEdit ("Type or paste receiver address here" :: String)

  validations <- createValidations receiverWidget $
    [ ("Wrong address", QWidget.cast addressEdit)
    , ("Wrong amount", QWidget.cast amountEdit)
    ]

  addRowLayout layout addressEdit amountLayout
  addSeparator layout

  QBoxLayout.addWidget receiversLayout receiverWidget

  void $ setProperty totalAmount ("styleRole" :: Text) ("incorrectTotal" :: Text)
  connect_ amountEdit QLineEdit.textChangedSignal $ \_ -> do
    revalidate s
    recalcTotal uiLangFace s
  connect_ addressEdit QLineEdit.textChangedSignal $ \_ -> revalidate s

  let r = Receiver{..}

  connect_ removeButton QAbstractButton.clickedSignal $ \_ -> do
    removeReceiver uiLangFace r s

  modifyIORef' receivers (r :)
  QWidget.adjustSize receiversWidget
  QWidget.adjustSize send
  revalidate s

removeReceiver :: UiLangFace -> Receiver -> Send -> IO ()
removeReceiver uiLangFace@UiLangFace{..} Receiver{..} s@Send{..} = do
  enabledReceivers <- getEnabledReceivers s
  QWidget.setEnabled receiverWidget False
  QWidget.setVisible receiverWidget False
  revalidate s
  recalcTotal uiLangFace s
  -- To prevent removing last receiver
  if length enabledReceivers == 1
    then do
      createNewReceiver uiLangFace s
    else do
      QWidget.adjustSize receiversWidget
      QWidget.adjustSize send

getEnabledReceivers :: Send -> IO [Receiver]
getEnabledReceivers Send{..} = do
  mode <- toEnum <$> QComboBox.currentIndex modeSelector
  case mode of
    SendBasic ->
      return [singleReceiver]
    SendAdvanced -> do
      r <- readIORef receivers
      filterM (\Receiver{..} -> QWidget.isEnabled receiverWidget) r

blackColorQLabelStyleSheet :: String
blackColorQLabelStyleSheet = "QLabel { color : rgba(52, 52, 62, 255) }"

grayColorQLabelStyleSheet :: String
grayColorQLabelStyleSheet = "QLabel { color : rgba(52, 52, 62, 127) }"

recalcTotal :: UiLangFace -> Send -> IO ()
recalcTotal UiLangFace{..} s@Send{..} = do
  enabledReceivers <- getEnabledReceivers s
  textAmounts <- mapM (fmap fromString . QLineEdit.text . amountEdit) enabledReceivers
  QWidget.setVisible errorTextLabel False
  QWidget.adjustSize send
  let amounts = mapMaybe (fmap normalize . readMaybe) textAmounts
  -- Calculate total sum only when all amount inputs are correct
  if length enabledReceivers == length amounts
    then do
      QWidget.setStyleSheet totalAmount blackColorQLabelStyleSheet
      langPutUiCommand (UiCalcTotal amounts) >>= \case
        Left err -> void $ QMessageBox.critical send ("Error" :: String) $ toString err
        Right _ -> pass
    -- Color amount gray, when something is typed improperly. Last correct total amount will be shown
    else QWidget.setStyleSheet totalAmount grayColorQLabelStyleSheet

changeTotalAmount :: Send -> Text -> Text -> IO ()
changeTotalAmount Send{..} amount unit = do
  QLabel.setText totalAmount $ toString amount
  QLabel.setText totalUnit $ toString unit

showInvalidArgumentsError :: Send -> Text -> IO ()
showInvalidArgumentsError Send{..} err = do
  QLabel.setText errorTextLabel $
    ("An error occurred while calculating total: " :: String) <> toString err
  QWidget.setStyleSheet totalAmount grayColorQLabelStyleSheet
  QWidget.setVisible errorTextLabel True

moveAddReceiverButton :: Send -> IO ()
moveAddReceiverButton Send{..} = do
  HPoint{y = widgetY} <- QWidget.pos receiversWidget
  HSize{height = widgetHeight} <- QWidget.size receiversWidget
  HSize{width = sendWidth} <- QWidget.size send
  QWidget.move addReceiverButton $ HPoint {x = sendWidth - 40, y = widgetY + widgetHeight - 73}

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
  QWidget.setVisible dropdownWidget True

selectAccountText :: Text
selectAccountText = "select accounts..."

accountsSelected :: AccountSelector -> Send -> IO ()
accountsSelected AccountSelector{..} s@Send{..} = do
  revalidate s

  accountNames <- T.intercalate " + " . map accountName <$>
    filterM (QAbstractButton.isChecked . checkbox) checkboxes
  QLabel.setText fromDisplay $ toString $
    if not $ null accountNames then accountNames else selectAccountText
