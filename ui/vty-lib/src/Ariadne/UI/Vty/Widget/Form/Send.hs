module Ariadne.UI.Vty.Widget.Form.Send
       ( initSendWidget
       ) where

import qualified Universum.Unsafe as Unsafe (fromJust)

import Control.Lens (assign, at, lens, makeLensesWith, uses, (%=), (.=), (<<+=))
import Data.Map (Map)

import qualified Brick as B
import qualified Data.Map as Map
import qualified Data.Text as T

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Form.Button
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.Util

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

data SendOutput =
  SendOutput
    { sendAddress :: !Text
    , sendAmount :: !Text
    }

data SendWidgetState p =
  SendWidgetState
    { sendLangFace :: !UiLangFace
    , sendWalletIdxGetter :: !(Maybe (p -> Maybe Word))
    , sendAccountsGetter :: !(Maybe (p -> [Word32]))

    , sendOutputs :: !(Map Int SendOutput)
    , sendNextOutput :: !Int
    , sendPass :: !Text
    , sendFeeResult :: !FeeResult
    , sendResult :: !SendResult
    }

data FeeResult
  = FeeResultNone
  | FeeResultWaiting !UiCommandId
  | FeeResultError !Text
  | FeeResultSuccess !Text  -- ^ Estimated fee

data SendResult
  = SendResultNone
  | SendResultWaiting !UiCommandId
  | SendResultError !Text
  | SendResultSuccess !Text  -- ^ Transaction ID

makeLensesWith postfixLFields ''SendOutput
makeLensesWith postfixLFields ''SendWidgetState

initSendWidget
  :: UiLangFace
  -> Maybe (p -> Maybe Word)
  -> Maybe (p -> [Word32])
  -> Widget p
initSendWidget langFace walletIdxGetter accountsGetter =
  initWidget $ do
    setWidgetDrawWithFocus drawSendWidget
    setWidgetHandleEvent handleSendWidgetEvent
    setWidgetState SendWidgetState
      { sendLangFace = langFace
      , sendWalletIdxGetter = walletIdxGetter
      , sendAccountsGetter = accountsGetter

      , sendOutputs = Map.empty
      , sendNextOutput = 0
      , sendPass = ""
      , sendFeeResult = FeeResultNone
      , sendResult = SendResultNone
      }

    addOutput
    addWidgetChild WidgetNameSendAdd $
      initButtonWidget "+"
    addWidgetChild WidgetNameSendPass $
      initPasswordWidget $ widgetParentLens sendPassL
    addWidgetChild WidgetNameSendButton $
      initButtonWidget "Send"

    addWidgetEventHandler WidgetNameSendAdd $ \case
      WidgetEventButtonPressed -> addOutput
      _ -> pass
    addWidgetEventHandler WidgetNameSendButton $ \case
      WidgetEventButtonPressed -> performSendTransaction
      _ -> pass

    updateFocusList

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

drawSendWidget
  :: WidgetName
  -> SendWidgetState p
  -> WidgetDrawM (SendWidgetState p) p WidgetDrawing
drawSendWidget focus SendWidgetState{..} = do
  widget <- ask

  let
    padBottom = B.padBottom (B.Pad 1)
    padLeft = B.padLeft (B.Pad 1)
    fillLeft w = T.takeEnd w . (T.append $ T.replicate w " ")
    fillRight w = T.take w . (flip T.append $ T.replicate w " ")

    labelWidth = 16
    amountWidth = 15

    drawChild = last . drawWidgetChild focus widget
    label = B.padRight (B.Pad 1) . B.txt . fillLeft labelWidth

    drawOutputsHeader = B.hBox
      [ label ""
      , B.padRight B.Max $ B.txt "Address"
      , padLeft $ B.txt $ fillRight amountWidth $ "Amount"
      , padLeft $ B.txt "     "
      ]
    drawOutput idx = B.hBox
      [ label ""
      , drawChild $ WidgetNameSendAddress idx
      , padLeft $ B.hLimit amountWidth $ drawChild $ WidgetNameSendAmount idx
      , padLeft $ drawChild $ WidgetNameSendRemove idx
      ]
    drawOutputsFooter = B.hBox
      [ B.padLeft B.Max $ drawChild WidgetNameSendAdd
      ]

  return . singleDrawing $
    B.vBox $
    padBottom <$>
      [ B.txt "Send transaction"
      , B.vBox $
          [drawOutputsHeader] ++
          (drawOutput <$> Map.keys sendOutputs) ++
          [drawOutputsFooter]
      , label "Estimated fee:" B.<+> case sendFeeResult of
          FeeResultNone -> B.emptyWidget
          FeeResultWaiting _ -> B.txt "Calculating fee..."
          FeeResultError err -> B.txt err
          FeeResultSuccess fee -> B.txt fee
      , label "Passphrase:" B.<+> drawChild WidgetNameSendPass
      , label "" B.<+> drawChild WidgetNameSendButton
      , case sendResult of
          SendResultNone -> B.emptyWidget
          SendResultWaiting _ -> B.txt "Sending..."
          SendResultError err -> B.txt $ "Couldn't send a transaction: " <> err
          SendResultSuccess tr -> B.txt $ "Transaction sent: " <> tr
      ]

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

handleSendWidgetEvent
  :: UiEvent
  -> WidgetEventM (SendWidgetState p) p ()
handleSendWidgetEvent = \case
  UiCommandResult commandId (UiSendCommandResult result) -> do
    use (widgetStateL . sendResultL) >>= \case
      SendResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiSendCommandSuccess tr -> do
            zoomWidgetState $ do
              sendResultL .= SendResultSuccess tr
              sendPassL .= ""
              sendOutputsL .= Map.empty
            addOutput
            updateFee
          UiSendCommandFailure err -> do
            widgetStateL . sendResultL .= SendResultError err
      _ ->
        pass
  UiCommandResult commandId (UiFeeCommandResult result) -> zoomWidgetState $ do
    sendFeeResultL %= \case
      FeeResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiFeeCommandSuccess fee -> FeeResultSuccess fee
          UiFeeCommandFailure err -> FeeResultError err
      other -> other
  _ ->
    pass

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

updateFocusList :: Monad m => StateT (WidgetInfo (SendWidgetState p) p) m ()
updateFocusList = do
    outputs <- uses (widgetStateL . sendOutputsL) Map.keys
    setWidgetFocusList $
      concat (outputFocuses <$> outputs) ++
      [ WidgetNameSendAdd
      , WidgetNameSendPass
      , WidgetNameSendButton
      ]
  where
    outputFocuses idx =
      [ WidgetNameSendAddress idx
      , WidgetNameSendAmount idx
      , WidgetNameSendRemove idx
      ]

collectFormData ::
  StateT (SendWidgetState p) (StateT p (B.EventM WidgetName)) (Maybe Word, [Word32], [UiSendOutput])
collectFormData = do
  parentState <- lift get
  walletIdx <- uses sendWalletIdxGetterL $ maybe Nothing (\getter -> getter parentState)
  accounts <- uses sendAccountsGetterL $ maybe [] (\getter -> getter parentState)
  outputs <- fmap (\SendOutput{..} -> UiSendOutput sendAddress sendAmount) <$> uses sendOutputsL Map.elems
  return (walletIdx, accounts, outputs)

updateFee :: WidgetEventM (SendWidgetState p) p ()
updateFee = zoomWidgetState $ do
  UiLangFace{..} <- use sendLangFaceL
  (walletIdx, accounts, outputs) <- collectFormData
  use sendFeeResultL >>= \case
    FeeResultWaiting commandId
      | Just taskId <- cmdTaskId commandId ->
          void . liftIO . langPutUISilentCommand $ UiKill taskId
    _ -> pass
  liftIO (langPutUISilentCommand $ UiFee $ UiFeeArgs walletIdx accounts outputs) >>=
    assign sendFeeResultL . either FeeResultError FeeResultWaiting

addOutput :: Monad m => StateT (WidgetInfo (SendWidgetState p) p) m ()
addOutput = do
  idx <- zoomWidgetState $ do
    idx <- sendNextOutputL <<+= 1
    sendOutputsL %= Map.insert idx (SendOutput "" "")
    return idx
  updateFocusList

  addWidgetChild (WidgetNameSendAddress idx) $
    initEditWidget $ widgetParentLens sendOutputsL . at idx . unsafeFromJust . sendAddressL
  addWidgetChild (WidgetNameSendAmount idx) $
    initEditWidget $ widgetParentLens sendOutputsL . at idx . unsafeFromJust . sendAmountL
  addWidgetChild (WidgetNameSendRemove idx) $
    initButtonWidget "-"

  addWidgetEventHandler (WidgetNameSendAddress idx) $ \case
    WidgetEventEditChanged -> updateFee
    _ -> pass
  addWidgetEventHandler (WidgetNameSendAmount idx) $ \case
    WidgetEventEditChanged -> updateFee
    _ -> pass
  addWidgetEventHandler (WidgetNameSendRemove idx) $ \case
    WidgetEventButtonPressed -> removeOutput idx
    _ -> pass

removeOutput :: Int -> WidgetEventM (SendWidgetState p) p ()
removeOutput idx = do
  remaining <- uses (widgetStateL . sendOutputsL) Map.size
  when (remaining > 1) $ do
    widgetStateL . sendOutputsL %= Map.delete idx
    updateFocusList
    updateFee

performSendTransaction :: WidgetEventM (SendWidgetState p) p ()
performSendTransaction = zoomWidgetState $ do
  UiLangFace{..} <- use sendLangFaceL
  (walletIdx, accounts, outputs) <- collectFormData
  passphrase <- use sendPassL
  use sendResultL >>= \case
    SendResultWaiting _ -> pass
    _ -> liftIO (langPutUiCommand $ UiSend $ UiSendArgs walletIdx accounts outputs passphrase) >>=
      assign sendResultL . either SendResultError SendResultWaiting

unsafeFromJust :: Lens' (Maybe a) a
unsafeFromJust = lens Unsafe.fromJust setJust
  where
    setJust (Just _) b = Just b
    setJust Nothing  _ = error "setJust: Nothing"
