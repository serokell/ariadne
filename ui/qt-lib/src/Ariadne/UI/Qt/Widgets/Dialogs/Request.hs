module Ariadne.UI.Qt.Widgets.Dialogs.Request
  ( RequestAccounts(..)
  , Request
  , startRequest
  , addNewAddress
  ) where

import Data.Bits
import qualified Data.Text as T

import Graphics.UI.Qtah.Core.Types
  (QtLayoutDirection(..), alignHCenter, alignRight, alignVCenter)
import Graphics.UI.Qtah.Signal (connect_)
import Graphics.UI.Qtah.Widgets.QSizePolicy (QSizePolicyPolicy(..))

import qualified Graphics.UI.Qtah.Gui.QClipboard as QClipboard
import qualified Graphics.UI.Qtah.Gui.QIcon as QIcon
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Ariadne.UI.Qt.Face
import Ariadne.UI.Qt.UI
import Ariadne.UI.Qt.Util
import Ariadne.UI.Qt.Widgets.Dialogs.Util

data RequestAccounts
  = RequestAccountsSingle UiAccountInfo
  | RequestAccountsMulti [UiAccountInfo]

data RequestAccountWidget =
  RequestAccountWidget
    { rawWidget :: QWidget.QWidget
    , rawLayout :: QVBoxLayout.QVBoxLayout
    , rawHeader :: Maybe QPushButton.QPushButton
    , rawWIdx :: Word
    , rawAIdx :: Word
    }

data Request =
  Request
    { request :: QDialog.QDialog
    , currentAccount :: IORef (Maybe (Word, Word))
    , accountWidgets :: [RequestAccountWidget]
    }

initRequest :: UiLangFace -> RequestAccounts -> IO Request
initRequest langFace requestAccounts = do
  request <- QDialog.new
  layout <- createLayout request

  let headerString = "REQUEST" :: String

  QWidget.setWindowTitle request headerString

  header <- QLabel.newWithText headerString
  addHeader layout header

  currentAccount <- newIORef $ case requestAccounts of
    RequestAccountsSingle uaci -> Just $ uaciToIndices uaci
    RequestAccountsMulti (uaci:_) -> Just $ uaciToIndices uaci
    _ -> Nothing

  accountWidgets <- createAccountWidgets layout requestAccounts

  generateButton <- QPushButton.newWithText ("GENERATE NEW ADDRESS" :: String)
  QWidget.setSizePolicyRaw generateButton Maximum Preferred
  QBoxLayout.addWidget layout generateButton
  void $ QLayout.setWidgetAlignment layout generateButton $ alignHCenter .|. alignVCenter

  let req = Request{..}

  connect_ generateButton QAbstractButton.clickedSignal $ \_ -> generateClicked langFace req
  connectHeaders req

  whenJust (safeHead accountWidgets) $ switchAccount req

  return req

startRequest :: UiLangFace -> IO () -> RequestAccounts -> IO Request
startRequest langFace onClosed requestAccounts = do
  req@Request{..} <- initRequest langFace requestAccounts
  QWidget.show request

  connect_ request QDialog.finishedSignal $ const onClosed

  return req

uaciToIndices :: UiAccountInfo -> (Word, Word)
uaciToIndices UiAccountInfo{..} = (uaciWalletIdx, head uaciPath)

createAccountWidgets :: QVBoxLayout.QVBoxLayout -> RequestAccounts -> IO [RequestAccountWidget]
createAccountWidgets layout (RequestAccountsSingle uaci) = do
  (widget, layout') <- createAccountWidget uaci
  QBoxLayout.addWidget layout widget

  let (wIdx, aIdx) = uaciToIndices uaci
  return [
    RequestAccountWidget
      { rawWidget = widget
      , rawLayout = layout'
      , rawHeader = Nothing
      , rawWIdx = wIdx
      , rawAIdx = aIdx
      }]
createAccountWidgets layout (RequestAccountsMulti uacis) = forM uacis $ \uaci@UiAccountInfo{..} -> do
  header <- QPushButton.newWithText $ toString $ T.toUpper $ fromMaybe "" uaciLabel
  (widget, layout') <- createAccountWidget uaci
  QBoxLayout.addWidget layout header
  QBoxLayout.addWidget layout widget
  QWidget.hide widget

  void $ setProperty header ("styleRole" :: String) ("receiveAccordionHeader" :: String)
  -- This is needed to put the icon right of the text
  QWidget.setLayoutDirection header RightToLeft
  -- Right is actually Left due to layoutDirection = RightToLeft
  void $ QLayout.setWidgetAlignment layout header $ alignRight .|. alignVCenter
  QWidget.setSizePolicyRaw header Maximum Minimum

  let (wIdx, aIdx) = uaciToIndices uaci
  return
    RequestAccountWidget
      { rawWidget = widget
      , rawLayout = layout'
      , rawHeader = Just header
      , rawWIdx = wIdx
      , rawAIdx = aIdx
      }

createAddressRow :: QVBoxLayout.QVBoxLayout -> Text -> (Text, UiCurrency) -> IO ()
createAddressRow parentLayout address (balance, unit) = do
  layout <- createRowLayout
  QLayout.setSpacing layout 18
  QLayout.setSizeConstraint layout QLayout.SetMinimumSize

  addressLabel <- QLabel.newWithText $ toString address
  QBoxLayout.addWidget layout addressLabel

  rightLayout <- QVBoxLayout.new
  QBoxLayout.setSpacing rightLayout 0

  balanceLabel <- QLabel.newWithText $ toString $ balance <> " " <> unitToHtmlSmall unit
  copyButton <- QPushButton.newWithText ("Copy address" :: String)
  QBoxLayout.addWidget rightLayout balanceLabel
  QBoxLayout.addWidget rightLayout copyButton

  void $ QLayout.setWidgetAlignment rightLayout balanceLabel $ alignRight .|. alignVCenter
  void $ setProperty copyButton ("styleRole" :: String) ("copyAddressButton" :: String)
  QAbstractButton.setIcon copyButton =<< QIcon.newWithFile (":/images/clipboard-small-ic.png" :: String)
  QWidget.setSizePolicyRaw copyButton Maximum Preferred

  QBoxLayout.addLayout layout rightLayout
  QBoxLayout.setStretch layout 0 1
  QBoxLayout.setStretch layout 1 0

  QBoxLayout.addLayout parentLayout layout

  clipboard <- QApplication.clipboard
  connect_ copyButton QAbstractButton.clickedSignal $ \_ -> QClipboard.setText clipboard $ toString address

createAccountWidget :: UiAccountInfo -> IO (QWidget.QWidget, QVBoxLayout.QVBoxLayout)
createAccountWidget UiAccountInfo{..} = do
  widget <- QWidget.new
  layout <- QVBoxLayout.new
  QWidget.setLayout widget layout

  sequence_ $ intersperse (addSeparator layout) $
    map (\UiAddressInfo{..} -> createAddressRow layout uadiAddress uadiBalance) uaciAddresses

  return (widget, layout)

connectHeaders :: Request -> IO ()
connectHeaders req@Request{..} = for_ accountWidgets $
  \raw@RequestAccountWidget{..} -> whenJust rawHeader $ \header ->
      connect_ header QAbstractButton.clickedSignal $ \_ -> switchAccount req raw

switchAccount :: Request -> RequestAccountWidget -> IO ()
switchAccount Request{..} RequestAccountWidget{..} = do
  closedIcon <- QIcon.newWithFile (":/images/expand-arrow.png" :: String)
  openIcon <- QIcon.newWithFile (":/images/expand-arrow-open.png" :: String)

  writeIORef currentAccount $ Just (rawWIdx, rawAIdx)
  for_ accountWidgets $ \RequestAccountWidget{rawWidget = aWidget, rawHeader = aHeader} -> do
    QWidget.hide aWidget
    whenJust aHeader $ \header -> QAbstractButton.setIcon header closedIcon
  QWidget.show rawWidget
  whenJust rawHeader $ \header -> QAbstractButton.setIcon header openIcon
  QWidget.adjustSize request

generateClicked :: UiLangFace -> Request -> IO ()
generateClicked UiLangFace{..} req@Request{..} = whenJustM (readIORef currentAccount) $ \(wIdx, aIdx) ->
  langPutUiCommand (UiNewAddress wIdx aIdx) >>= handleResult req

handleResult :: Request -> Either Text UiCommandId -> IO ()
handleResult Request{..} (Left msg) = void $ QMessageBox.critical request ("Error" :: String) $ toString msg
handleResult _ _ = pass

addNewAddress :: Request -> Word -> Word -> Text -> IO ()
addNewAddress Request{..} wIdx aIdx address = for_ accountWidgets updateAccountWidget
  where
    updateAccountWidget :: RequestAccountWidget -> IO ()
    updateAccountWidget RequestAccountWidget{..}
      | (rawWIdx, rawAIdx) == (wIdx, aIdx) = doUpdate rawLayout
      | otherwise = pass

    doUpdate :: QVBoxLayout.QVBoxLayout -> IO ()
    doUpdate layout = createAddressRow layout address ("0", ADA)
