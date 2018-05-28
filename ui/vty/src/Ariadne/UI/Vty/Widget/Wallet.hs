module Ariadne.UI.Vty.Widget.Wallet
       ( WalletWidgetState
       , initWalletWidget
       , drawWalletWidget

       , WalletWidgetEvent(..)
       , handleWalletFocus
       , handleWalletFocusIn
       , handleWalletWidgetEvent
       ) where

import Universum

import Control.Lens (assign, makeLensesWith, uses, (<%=), (%=), (.=))
import IiExtras

import qualified Brick as B
import qualified Brick.Focus as B
import qualified Brick.Forms as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.UI

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

data WalletWidgetState =
  WalletWidgetState
    { walletName :: !Text
    , walletBalance :: !BalanceResult
    , walletSendAddress :: !Text
    , walletSendAmount :: !Text
    , walletSendPass :: !Text
    , walletSendResult :: !SendResult

    , walletFocusRing :: !(B.FocusRing BrickName)
    , walletFieldSendAddress :: B.FormFieldState WalletWidgetState UiEvent BrickName
    , walletFieldSendAmount :: B.FormFieldState WalletWidgetState UiEvent BrickName
    , walletFieldSendPass :: B.FormFieldState WalletWidgetState UiEvent BrickName
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

initWalletWidget :: WalletWidgetState
initWalletWidget =
  fix $ \this -> WalletWidgetState
    { walletName = ""
    , walletBalance = BalanceResultNone
    , walletSendAddress = ""
    , walletSendAmount = ""
    , walletSendPass = ""
    , walletSendResult = SendResultNone

    , walletFocusRing = B.focusRing
        [ BrickNone
        , BrickWalletSendAddress, BrickWalletSendAmount
        , BrickWalletSendPass, BrickWalletSendButton
        ]
    , walletFieldSendAddress = B.editTextField walletSendAddressL BrickWalletSendAddress (Just 1) this
    , walletFieldSendAmount = B.editTextField walletSendAmountL BrickWalletSendAmount (Just 1) this
    , walletFieldSendPass = B.editPasswordField walletSendPassL BrickWalletSendPass this
    }

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

drawWalletWidget :: Bool -> WalletWidgetState -> B.Widget BrickName
drawWalletWidget _hasFocus WalletWidgetState{..} =
  B.vBox
    [       pad . B.txt $ " Wallet name: " <> walletName
    , pad . pad . B.txt $ "     Balance: " <> drawBalance

    , pad $ B.txt "Send transaction"
    , renderField "     Address: " $ walletFieldSendAddress
    , renderField " Amount, ADA: " $ walletFieldSendAmount
    , renderField "  Passphrase: " $ walletFieldSendPass
    , button "[ Send ]" BrickWalletSendButton
    , drawSendResult
    ]
  where
    pad = B.padBottom (B.Pad 1)
    padLeft = B.padLeft (B.Pad 14)
    renderField label field =
      pad $
        B.txt label B.<+>
        B.renderFormFieldState walletFocusRing field
    withFocus name =
      if B.focusGetCurrent walletFocusRing == Just name
      then B.withAttr "selected"
      else identity
    button label name =
      pad . padLeft . B.clickable name . withFocus name $ B.txt label
    drawBalance = case walletBalance of
      BalanceResultNone -> ""
      BalanceResultWaiting _ -> "calculating..."
      BalanceResultError err -> err
      BalanceResultSuccess balance -> balance
    drawSendResult = padLeft $ case walletSendResult of
      SendResultNone -> B.emptyWidget
      SendResultWaiting _ -> B.txt "Sending..."
      SendResultError err -> B.txt $ "Couldn't send a transaction: " <> err
      SendResultSuccess tr -> B.txt $ "Transaction sent: " <> tr

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

data WalletWidgetEvent
  = WalletUpdateEvent (Maybe UiWalletInfo)
  | WalletMouseDownEvent BrickName B.Location
  | WalletKeyEvent KeyboardEvent V.Event
  | WalletBalanceCommandResult UiCommandId UiBalanceCommandResult
  | WalletSendCommandResult UiCommandId UiSendCommandResult

handleWalletFocus
  :: Bool
  -> StateT WalletWidgetState (B.EventM BrickName) Bool
handleWalletFocus back = do
  newFocus <- walletFocusRingL <%= if back then B.focusPrev else B.focusNext
  return $ B.focusGetCurrent newFocus /= Just BrickNone

handleWalletFocusIn
  :: Bool
  -> StateT WalletWidgetState (B.EventM BrickName) ()
handleWalletFocusIn back = do
  walletFocusRingL %= (if back then B.focusPrev else B.focusNext) . B.focusSetCurrent BrickNone

handleWalletWidgetEvent
  :: UiLangFace
  -> WalletWidgetEvent
  -> StateT WalletWidgetState (B.EventM BrickName) ()
handleWalletWidgetEvent langFace@UiLangFace{..} = \case
  WalletUpdateEvent itemInfo -> do
    whenJust itemInfo $ \UiWalletInfo{..} -> case wpiType of
      Just UiWalletInfoWallet -> do
        walletNameL .= fromMaybe "" wpiLabel
        use walletBalanceL >>= \case
          BalanceResultWaiting commandId
            | Just taskId <- cmdTaskId commandId ->
                void . liftIO . langPutUiCommand $ UiKill taskId
          _ -> return ()
        liftIO (langPutUiCommand UiBalance) >>=
          assign walletBalanceL . either BalanceResultError BalanceResultWaiting
      _ -> return ()
  WalletMouseDownEvent name _coords -> do
    walletFocusRingL %= B.focusSetCurrent name
    case name of
      BrickWalletSendButton ->
        performSendTransaction langFace
      _ ->
        return ()
  WalletKeyEvent key vtyEv -> do
    name <- uses walletFocusRingL $ fromMaybe BrickNone . B.focusGetCurrent
    case name of
      BrickWalletSendButton
        | key `elem` [KeyEnter, KeyChar ' '] ->
            performSendTransaction langFace
        | otherwise ->
            return ()
      BrickWalletSendAddress -> do
        field <- use walletFieldSendAddressL
        get >>= lift . handleFormFieldEvent BrickWalletSendAddress (B.VtyEvent vtyEv) walletFieldSendAddressL field >>= put
      BrickWalletSendAmount -> do
        field <- use walletFieldSendAmountL
        get >>= lift . handleFormFieldEvent BrickWalletSendAmount (B.VtyEvent vtyEv) walletFieldSendAmountL field >>= put
      BrickWalletSendPass -> do
        field <- use walletFieldSendPassL
        get >>= lift . handleFormFieldEvent BrickWalletSendPass (B.VtyEvent vtyEv) walletFieldSendPassL field >>= put
      _ ->
        return ()
  WalletBalanceCommandResult commandId result -> do
    walletBalanceL %= \case
      BalanceResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiBalanceCommandSuccess balance -> BalanceResultSuccess balance
          UiBalanceCommandFailure err -> BalanceResultError err
      other -> other
  WalletSendCommandResult commandId result -> do
    walletSendResultL %= \case
      SendResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiSendCommandSuccess tr -> SendResultSuccess tr
          UiSendCommandFailure err -> SendResultError err
      other -> other

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

performSendTransaction
  :: UiLangFace
  -> StateT WalletWidgetState (B.EventM BrickName) ()
performSendTransaction UiLangFace{..} = do
  address <- use walletSendAddressL
  amount <- use walletSendAmountL
  passphrase <- use walletSendPassL
  use walletSendResultL >>= \case
    SendResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiSend address amount passphrase) >>=
      assign walletSendResultL . either SendResultError SendResultWaiting
