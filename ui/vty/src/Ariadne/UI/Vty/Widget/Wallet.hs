module Ariadne.UI.Vty.Widget.Wallet
       ( initWalletWidget
       ) where

import Universum

import Control.Lens (assign, at, ix, lens, makeLensesWith, uses, (%=), (.=), (<<+=))
import Data.Map (Map)
import Data.Maybe (fromJust)
import IiExtras

import qualified Brick as B
import qualified Data.Map as Map
import qualified Data.Text as T

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Form.Button
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.UI.Vty.Widget.Form.List

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

data WalletAccount =
  WalletAccount
    { walletAccountIdx :: !Word32
    , walletAccountName :: !Text
    , walletAccountSelected :: Bool
    }

data WalletSendOutput = 
  WalletSendOutput
    { walletSendAddress :: !Text
    , walletSendAmount :: !Text
    }

data WalletWidgetState =
  WalletWidgetState
    { walletLangFace :: !UiLangFace

    , walletName :: !Text
    , walletBalance :: !BalanceResult

    , walletAccounts :: ![WalletAccount]

    , walletSendOutputs :: !(Map Int WalletSendOutput)
    , walletSendNextOutput :: !Int
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

makeLensesWith postfixLFields ''WalletAccount
makeLensesWith postfixLFields ''WalletSendOutput
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

      , walletAccounts = []

      , walletSendOutputs = Map.empty
      , walletSendNextOutput = 0
      , walletSendPass = ""
      , walletSendResult = SendResultNone
      }

    addWidgetChild WidgetNameWalletAccountList $
      initListWidget (widgetParentGetter walletAccounts) drawAccountRow
    addWidgetEventHandler WidgetNameWalletAccountList $ \case
      WidgetEventListSelected idx -> toggleAccount idx
      _ -> return ()

    withWidgetState addOutput
    addWidgetChild WidgetNameWalletSendAdd $
      initButtonWidget "+"
    addWidgetChild WidgetNameWalletSendPass $
      initPasswordWidget $ widgetParentLens walletSendPassL
    addWidgetChild WidgetNameWalletSendButton $
      initButtonWidget "Send"

    addWidgetEventHandler WidgetNameWalletSendAdd $ \case
      WidgetEventButtonPressed -> addOutput
      _ -> return ()
    addWidgetEventHandler WidgetNameWalletSendButton $ \case
      WidgetEventButtonPressed -> performSendTransaction
      _ -> return ()

    withWidgetState updateFocusList

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

drawAccountRow :: Bool -> WalletAccount -> B.Widget WidgetName
drawAccountRow focused WalletAccount{..} =
  (if focused then B.withAttr "selected" else id) $
  B.txt $
  (if walletAccountSelected then "[X] " else "[ ] ") <> walletAccountName

drawWalletWidget :: WidgetName -> WalletWidgetState -> WidgetDrawM WalletWidgetState p (B.Widget WidgetName)
drawWalletWidget focus WalletWidgetState{..} = do
  widget <- ask
  widgetName <- getWidgetName

  let
    padBottom = B.padBottom (B.Pad 1)
    padLeft = B.padLeft (B.Pad 1)
    fillLeft w = T.takeEnd w . (T.append $ T.replicate w " ")
    fillRight w = T.take w . (flip T.append $ T.replicate w " ")

    labelWidth = 14
    amountWidth = 15

    visible namePart = if focus == widgetName ++ [namePart] then B.visible else identity
    drawChild namePart = visible namePart $ drawWidgetChild focus widget namePart
    label = B.padRight (B.Pad 1) . B.txt . fillLeft labelWidth

    drawOutputsHeader = B.hBox
      [ label ""
      , B.padRight B.Max $ B.txt "Address"
      , padLeft $ B.txt $ fillRight amountWidth $ "Amount, ADA"
      , padLeft $ B.txt "     "
      ]
    drawOutput idx = B.hBox
      [ label ""
      , drawChild $ WidgetNameWalletSendAddress idx
      , padLeft $ B.hLimit amountWidth $ drawChild $ WidgetNameWalletSendAmount idx
      , padLeft $ drawChild $ WidgetNameWalletSendRemove idx
      ]
    drawOutputsFooter = B.hBox
      [ B.padLeft B.Max $ drawChild WidgetNameWalletSendAdd
      ]

  return $
    B.viewport widgetName B.Vertical $
    B.padAll 1 $
    B.vBox $
    padBottom <$>
      [ label "Wallet name:" B.<+> B.txt walletName
      , label "Balance:" B.<+> case walletBalance of
          BalanceResultNone -> B.emptyWidget
          BalanceResultWaiting _ -> B.txt "calculating..."
          BalanceResultError err -> B.txt err
          BalanceResultSuccess balance -> B.txt balance
      , label "Accounts:" B.<+> drawChild WidgetNameWalletAccountList

      , B.txt "Send transaction"
      , B.vBox $
          [drawOutputsHeader] ++
          (drawOutput <$> Map.keys walletSendOutputs) ++
          [drawOutputsFooter]
      , label "Passphrase:" B.<+> drawChild WidgetNameWalletSendPass
      , label "" B.<+> drawChild WidgetNameWalletSendButton
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
        walletAccountsL .= map (\(idx, (_, name)) -> WalletAccount idx name False) (zip [0..] wpiAccounts)
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
    use walletSendResultL >>= \case
      SendResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiSendCommandSuccess tr -> do
            walletSendResultL .= SendResultSuccess tr
            walletSendPassL .= ""
            walletSendOutputsL .= Map.empty
            addOutput
          UiSendCommandFailure err -> do
            walletSendResultL .= SendResultError err
      _ ->
        return ()
  _ ->
    return ()

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

updateFocusList :: Monad m => StateT WalletWidgetState (StateT (WidgetInfo WalletWidgetState p) m) ()
updateFocusList = do
    outputs <- uses walletSendOutputsL Map.keys
    lift $ setWidgetFocusList $
      [ WidgetNameWalletAccountList
      ] ++
      concat (outputFocuses <$> outputs) ++
      [ WidgetNameWalletSendAdd
      , WidgetNameWalletSendPass
      , WidgetNameWalletSendButton
      ]
  where
    outputFocuses idx =
      [ WidgetNameWalletSendAddress idx
      , WidgetNameWalletSendAmount idx
      , WidgetNameWalletSendRemove idx
      ]

toggleAccount :: Int -> WidgetEventM WalletWidgetState p ()
toggleAccount idx = do
  walletAccountsL . ix idx . walletAccountSelectedL %= not

addOutput :: Monad m => StateT WalletWidgetState (StateT (WidgetInfo WalletWidgetState p) m) ()
addOutput = do
  idx <- walletSendNextOutputL <<+= 1
  walletSendOutputsL %= Map.insert idx (WalletSendOutput "" "")
  updateFocusList

  lift $ do
    addWidgetChild (WidgetNameWalletSendAddress idx) $
      initEditWidget $ widgetParentLens $ walletSendOutputsL . at idx . unsafeFromJust . walletSendAddressL
    addWidgetChild (WidgetNameWalletSendAmount idx) $
      initEditWidget $ widgetParentLens $ walletSendOutputsL . at idx . unsafeFromJust . walletSendAmountL
    addWidgetChild (WidgetNameWalletSendRemove idx) $
      initButtonWidget "-"
    addWidgetEventHandler (WidgetNameWalletSendRemove idx) $ \case
      WidgetEventButtonPressed -> removeOutput idx
      _ -> return ()

removeOutput :: Int -> WidgetEventM WalletWidgetState p ()
removeOutput idx = do
  remaining <- uses walletSendOutputsL Map.size
  when (remaining > 1) $ do
    walletSendOutputsL %= Map.delete idx
    updateFocusList

performSendTransaction :: WidgetEventM WalletWidgetState p ()
performSendTransaction = do
  UiLangFace{..} <- use walletLangFaceL
  accounts <- map walletAccountIdx <$> filter walletAccountSelected <$> use walletAccountsL
  outputs <- fmap (\WalletSendOutput{..} -> (walletSendAddress, walletSendAmount)) <$> uses walletSendOutputsL Map.elems
  passphrase <- use walletSendPassL
  use walletSendResultL >>= \case
    SendResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiSend accounts outputs passphrase) >>=
      assign walletSendResultL . either SendResultError SendResultWaiting

unsafeFromJust :: Lens' (Maybe a) a
unsafeFromJust = lens fromJust setJust
  where
    setJust (Just _) b = Just b
    setJust Nothing  _ = error "setJust: Nothing"
