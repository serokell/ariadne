module Ariadne.UI.Qt.Widgets.Dialogs.Send
  ( SendInputs(..)
  , SendOptions(..)
  , SendResult(..)
  , runSend
  ) where

import Data.Scientific (Scientific, normalize, isInteger)
import qualified Data.Text as T

import Graphics.UI.Qtah.Signal (connect_)

import Graphics.UI.Qtah.Core.HPoint (HPoint(..))
import Graphics.UI.Qtah.Core.HSize (HSize(..))
import qualified Graphics.UI.Qtah.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Core.QLocale as QLocale
import Graphics.UI.Qtah.Core.Types (QtWindowType(Popup))
import qualified Graphics.UI.Qtah.Event as Event
import qualified Graphics.UI.Qtah.Gui.QValidator as QValidator
import qualified Graphics.UI.Qtah.Gui.QDoubleValidator as QDoubleValidator
import qualified Graphics.UI.Qtah.Gui.QMouseEvent as QMouseEvent
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

data SendInputs = SendInputsSingle Word | SendInputsMulti [UiAccountInfo]

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
      Just so -> SendSuccess so
      Nothing -> SendCancel
    QDialog.Rejected -> SendCancel

fillSendOptions :: Send -> IO (Maybe SendOptions)
fillSendOptions Send{..} = do
  amount <- fromString <$> QLineEdit.text amountEdit
  address <- fromString <$> QLineEdit.text addressEdit

  accounts <- case accountSelector of
    Left aIdx -> return [aIdx]
    Right AccountSelector{..} -> map aIdx <$> filterM (QAbstractButton.isChecked . checkbox) checkboxes

  let UiWalletFace{..} = uiWalletFace

  return $ do
    guard $ not (null address) && uiValidateAddress address
    amount' :: Scientific <- normalize <$> readMaybe amount
    -- Check that amount' has no more than uiCoinPrecision decimal digits
    -- This is checked by QDoubleValidator, but just do be sure check again
    guard $ amount' > 0 && isInteger (amount' * 10 ^^ uiCoinPrecision)

    return $ SendOptions{soAmount = amount', soAddress = address, soAccounts = accounts}

isValid :: Send -> IO Bool
isValid = fmap isJust . fillSendOptions

revalidate :: Send -> IO ()
revalidate s@Send{..} = do
  valid <- isValid s
  QWidget.setEnabled sendButton valid

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
