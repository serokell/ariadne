module Ariadne.UI.Qt.Widgets.Dialogs.Send
  ( Send(..)
  , SendInputs(..)
  , SendOptions(..)
  , SendResult(..)
  , initSend
  , runSend
  , changeTotalAmount
  ) where

import Control.Lens (has)
import Data.Scientific (Scientific, normalize)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Validation
  (Validation(..), bindValidation, ensure, validate, _Success)

import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import Graphics.UI.Qtah.Core.HPoint (HPoint(..))
import Graphics.UI.Qtah.Core.HSize (HSize(..))
import Graphics.UI.Qtah.Core.Types (alignRight)
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

data Error
  = Amount
  | AmountEmpty
  | Address Text
  | AddressEmpty
  deriving (Eq, Show)

type Errors = [(Reciever, Error)]

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
    , recievers :: IORef [Reciever]
    , recieversLayout :: QVBoxLayout.QVBoxLayout
    , recieversWidget :: QWidget.QWidget
    , sendButton :: QPushButton.QPushButton
    , addRecieverButton :: QPushButton.QPushButton
    , fromDisplay :: QLabel.QLabel
    , totalAmount :: QLabel.QLabel
    , accountSelector :: Either Word AccountSelector
    , uiWalletFace :: UiWalletFace
    }

data Reciever = 
  Reciever
    { recieverWidget :: QWidget.QWidget
    , amountEdit :: QLineEdit.QLineEdit
    , addressEdit :: QLineEdit.QLineEdit
    , removeButton :: QPushButton.QPushButton
    , validations :: Validations
    }

initSend :: UiLangFace -> UiWalletFace -> SendInputs -> IO Send
initSend uiLangFace@UiLangFace{..} uiWalletFace@UiWalletFace{..} sendInputs = do
  send <- QDialog.new
  QObject.setObjectName send ("settingsDialog" :: String)
  layout <- createLayout send

  QWidget.setWindowTitle send ("SEND" :: String)
  headerLabel <- QLabel.newWithText ("SEND" :: String)
  addHeader layout headerLabel

  addressLabel <- QLabel.newWithText ("<b>RECIEVER</b>" :: String)
  amountLabel <- QLabel.newWithText ("<b>AMOUNT</b>" :: String)
  QLabel.setAlignment amountLabel alignRight
  addRow layout addressLabel amountLabel

  recieversWidget <- QWidget.new
  recieversLayout <- QVBoxLayout.new
  QWidget.setLayout recieversWidget recieversLayout
  QBoxLayout.addWidget layout recieversWidget

  fromAccountsLabel <- QLabel.newWithText ("<b>FROM</b>" :: String)
  fromDisplay <- QLabel.newWithText ("select account..." :: String)
  QObject.setObjectName fromDisplay ("accountsSelectorDisplay" :: String)

  case sendInputs of
    SendInputsSingle {} -> pass
    SendInputsMulti {} -> do
      addRow layout fromAccountsLabel fromDisplay
      addSeparator layout

  totalLabel <- QLabel.newWithText ("<b>TOTAL</b>" :: String)
  totalAmount <- QLabel.new
  QObject.setObjectName totalAmount ("amountLabel" :: String)
  QWidget.setSizePolicyRaw totalAmount Maximum Fixed
  addRow layout totalLabel totalAmount

  addRecieverButton <- QPushButton.new
  QWidget.resizeRaw addRecieverButton 36 36
  void $ setProperty addRecieverButton ("styleRole" :: Text) ("addButton" :: Text)
  QWidget.setParent addRecieverButton send

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
  enabledRecievers <- getEnabledRecievers s
  amounts <- mapM (\r'@Reciever{..} -> liftM2 (,) (return $ r') (fromString <$> QLineEdit.text amountEdit)) enabledRecievers
  addresses <- mapM (\r'@Reciever{..} -> liftM2 (,) (return $ r') (fromString <$> QLineEdit.text addressEdit)) enabledRecievers
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
  r <- readIORef recievers
  enabledRecievers <- filterM (\Reciever{..} -> QWidget.isEnabled recieverWidget) r

  validation <- fillSendOptions s
  let valid = isSuccess validation

  mapM_ (\r'@Reciever{..} -> 
    showErrorsV validations (filteredValidation validation r') errorToWidget) enabledRecievers

  QWidget.setEnabled sendButton valid

filteredValidation :: Validation Errors a -> Reciever -> Validation Errors a
filteredValidation (Success x)  _ = Success x
filteredValidation (Failure l) r = 
  Failure $ filter (\(r', _) -> recieverWidget r == recieverWidget r') l

errorToWidget :: (Reciever, Error) -> Maybe (Maybe Text, QWidget.QWidget)
errorToWidget (Reciever{..}, e) =
  case e of
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

createNewReciever :: UiLangFace -> Send -> IO ()
createNewReciever uiLangFace@UiLangFace{..} s@Send{..} = do
  recieverWidget <- QWidget.new
  layout <- createLayout recieverWidget
  QLayout.setContentsMarginsRaw layout 0 18 0 18

  amountEdit <- QLineEdit.new
  unitLabel <- QLabel.newWithText ("ADA" :: String)
  QObject.setObjectName unitLabel ("amountLabel" :: String)
  QObject.setObjectName amountEdit ("sendAmountEdit" :: String)
  QLineEdit.setPlaceholderText amountEdit ("0" :: String)

  let UiWalletFace{..} = uiWalletFace

  cLocale <- QLocale.newWithName ("C" :: String)
  QLocale.setNumberOptions cLocale QLocale.RejectGroupSeparator
  validator <- QDoubleValidator.new
  QValidator.setLocale validator cLocale
  QDoubleValidator.setBottom validator 0
  QDoubleValidator.setDecimals validator uiCoinPrecision
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

  validations <- createValidations recieverWidget $
    [ ("Wrong address", QWidget.cast addressEdit)
    , ("Wrong amount", QWidget.cast amountEdit)
    ]

  addRowLayout layout addressEdit amountLayout
  addSeparator layout

  QBoxLayout.addWidget recieversLayout recieverWidget

  connect_ amountEdit QLineEdit.textChangedSignal $ \_ -> do
    revalidate s
    recalcTotal uiLangFace s
  connect_ addressEdit QLineEdit.textChangedSignal $ \_ -> revalidate s

  let r = Reciever{..}

  connect_ removeButton QAbstractButton.clickedSignal $ \_ -> do
    removeReciever uiLangFace r s
    

  modifyIORef' recievers (r :)
  QWidget.adjustSize recieversWidget
  QWidget.adjustSize send
  revalidate s

removeReciever :: UiLangFace -> Reciever -> Send -> IO ()
removeReciever uiLangFace@UiLangFace{..} Reciever{..} s@Send{..} = do
  enabledRecievers <- getEnabledRecievers s
  QWidget.setEnabled recieverWidget False
  QWidget.setVisible recieverWidget False
  revalidate s
  recalcTotal uiLangFace s
  -- To prevent removing last reciever
  if length enabledRecievers == 1
    then do
      createNewReciever uiLangFace s
    else do
      QWidget.adjustSize recieversWidget
      QWidget.adjustSize send

getEnabledRecievers :: Send -> IO [Reciever]
getEnabledRecievers Send{..} = do
  r <- readIORef recievers
  filterM (\Reciever{..} -> QWidget.isEnabled recieverWidget) r

recalcTotal :: UiLangFace -> Send -> IO ()
recalcTotal UiLangFace{..} s@Send{..} = do
  enabledRecievers <- getEnabledRecievers s
  textAmounts <- mapM (\Reciever{..} -> fromString <$> QLineEdit.text amountEdit) enabledRecievers
  let amounts = map Unsafe.fromJust (filter isJust (map (\a -> normalize <$> readMaybe a) textAmounts))
  langPutUiCommand (UiCalcTotal amounts) >>= \case
    Left err -> void $ QMessageBox.critical send ("Error" :: String) $ toString err
    Right _ -> pass

changeTotalAmount :: Send -> Text -> IO ()
changeTotalAmount Send{..} amount = do
  QLabel.setText totalAmount $ toString amount

moveAddRecieverButton :: Send -> IO ()
moveAddRecieverButton Send{..} = do
  HPoint{x = _, y = widgetY} <- QWidget.pos recieversWidget
  HSize{width = _, height = widgetHeight} <- QWidget.size recieversWidget
  HSize{width = sendWidth, height = _} <- QWidget.size send
  QWidget.move addRecieverButton $ HPoint {x = sendWidth - 40, y = widgetY + widgetHeight - 78}  

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
  -- print (widgetWidth, widgetHeight)
  -- Make account widget occupy as much space as is available
  when (widgetWidth < labelWidth) $ QWidget.resizeRaw dropdownWidget labelWidth widgetHeight
  -- print (labelWidth, widgetHeight)
  QWidget.updateGeometry dropdownWidget

selectAccountText :: Text
selectAccountText = "select accounts..."

accountsSelected :: AccountSelector -> Send -> IO ()
accountsSelected AccountSelector{..} s@Send{..} = do
  revalidate s

  accountNames <- T.intercalate " + " . map accountName <$>
    filterM (QAbstractButton.isChecked . checkbox) checkboxes
  QLabel.setText fromDisplay $ toString $
    if not $ null accountNames then accountNames else selectAccountText
