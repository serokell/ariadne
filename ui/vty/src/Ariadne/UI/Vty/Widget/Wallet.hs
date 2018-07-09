module Ariadne.UI.Vty.Widget.Wallet
       ( initWalletWidget
       ) where

import Universum

import Control.Lens (assign, makeLensesWith, (%=), (.=))
import IiExtras

import qualified Brick as B
import qualified Data.Text as T

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Form.Button
import Ariadne.UI.Vty.Widget.Form.Edit

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

data WalletWidgetState =
  WalletWidgetState
    { walletLangFace :: !UiLangFace

    , walletName :: !Text
    , walletBalance :: !BalanceResult

    , walletSendAddress :: !Text
    , walletSendAmount :: !Text
    , walletSendPass :: !Text
    , walletSendResult :: !SendResult
    }

data BalanceResult
  = BalanceResultNone
  | BalanceResultWaiting !UiCommandId
  | BalanceResultError !Text
  | BalanceResultSuccess !Text  -- ^ Balance

data SendResult
  = SendResultNone
  | SendResultWaiting !UiCommandId
  | SendResultError !Text
  | SendResultSuccess !Text  -- ^ Transaction ID

makeLensesWith postfixLFields ''WalletWidgetState

initWalletWidget :: UiLangFace -> Widget p
initWalletWidget langFace =
  initWidget $ do
    setWidgetDrawWithFocus drawWalletWidget
    setWidgetScrollable
    setWidgetHandleEvent handleWalletWidgetEvent
    setWidgetState WalletWidgetState
      { walletLangFace = langFace

      , walletName = ""
      , walletBalance = BalanceResultNone

      , walletSendAddress = ""
      , walletSendAmount = ""
      , walletSendPass = ""
      , walletSendResult = SendResultNone
      }

    addWidgetChild WidgetNameWalletSendAddress $
      initEditWidget $ widgetParentLens walletSendAddressL
    addWidgetChild WidgetNameWalletSendAmount $
      initEditWidget $ widgetParentLens walletSendAmountL
    addWidgetChild WidgetNameWalletSendPass $
      initPasswordWidget $ widgetParentLens walletSendPassL
    addWidgetChild WidgetNameWalletSendButton $
      initButtonWidget "Send"

    addWidgetEventHandler WidgetNameWalletSendButton $ \case
      WidgetEventButtonPressed -> performSendTransaction
      _ -> return ()

    setWidgetFocusList
      [ WidgetNameWalletSendAddress
      , WidgetNameWalletSendAmount
      , WidgetNameWalletSendPass
      , WidgetNameWalletSendButton
      ]

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

drawWalletWidget :: WidgetName -> WalletWidgetState -> WidgetDrawM WalletWidgetState p (B.Widget WidgetName)
drawWalletWidget focus WalletWidgetState{..} = do
  widget <- ask
  widgetName <- getWidgetName

  let
    visible namePart = if focus == widgetName ++ [namePart] then B.visible else identity
    drawChild namePart = visible namePart $ drawWidgetChild focus widget namePart
    label = B.padRight (B.Pad 1) . B.txt . T.takeEnd 14 . (T.append $ T.replicate 14 " ")
    padBottom = B.padBottom (B.Pad 1)

  return $
    B.viewport widgetName B.Vertical $
    B.padAll 1 $
    B.vBox $
    padBottom <$>
      [ label "Wallet name:" B.<+> B.txt walletName
      , padBottom $ label "Balance:" B.<+> case walletBalance of
          BalanceResultNone -> B.emptyWidget
          BalanceResultWaiting _ -> B.txt "calculating..."
          BalanceResultError err -> B.txt err
          BalanceResultSuccess balance -> B.txt balance

      , visible WidgetNameWalletSendAddress $ B.txt "Send transaction"
      , label     "Address:" B.<+> drawChild WidgetNameWalletSendAddress
      , label "Amount, ADA:" B.<+> drawChild WidgetNameWalletSendAmount
      , label  "Passphrase:" B.<+> drawChild WidgetNameWalletSendPass
      , label             "" B.<+> drawChild WidgetNameWalletSendButton
      , case walletSendResult of
          SendResultNone -> B.emptyWidget
          SendResultWaiting _ -> B.txt "Sending..."
          SendResultError err -> B.txt $ "Couldn't send a transaction: " <> err
          SendResultSuccess tr -> B.txt $ "Transaction sent: " <> tr
      ]

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

handleWalletWidgetEvent
  :: UiEvent
  -> WidgetEventM WalletWidgetState p ()
handleWalletWidgetEvent = \case
  UiWalletEvent UiWalletUpdate{..} -> do
    whenJust wuPaneInfoUpdate $ \UiWalletInfo{..} -> case wpiType of
      Just UiWalletInfoWallet -> do
        UiLangFace{..} <- use walletLangFaceL
        walletNameL .= fromMaybe "" wpiLabel
        use walletBalanceL >>= \case
          BalanceResultWaiting commandId
            | Just taskId <- cmdTaskId commandId ->
                void . liftIO . langPutUiCommand $ UiKill taskId
          _ -> return ()
        liftIO (langPutUiCommand UiBalance) >>=
          assign walletBalanceL . either BalanceResultError BalanceResultWaiting
      _ -> return ()
  UiCommandResult commandId (UiBalanceCommandResult result) -> do
    walletBalanceL %= \case
      BalanceResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiBalanceCommandSuccess balance -> BalanceResultSuccess balance
          UiBalanceCommandFailure err -> BalanceResultError err
      other -> other
  UiCommandResult commandId (UiSendCommandResult result) -> do
    walletSendResultL %= \case
      SendResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiSendCommandSuccess tr -> SendResultSuccess tr
          UiSendCommandFailure err -> SendResultError err
      other -> other
  _ ->
    return ()

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

performSendTransaction :: WidgetEventM WalletWidgetState p ()
performSendTransaction = do
  UiLangFace{..} <- use walletLangFaceL
  address <- use walletSendAddressL
  amount <- use walletSendAmountL
  passphrase <- use walletSendPassL
  use walletSendResultL >>= \case
    SendResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiSend address amount passphrase) >>=
      assign walletSendResultL . either SendResultError SendResultWaiting
