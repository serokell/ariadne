module Ariadne.UI.Vty.Widget.Form.Send
       ( initSendWidget
       ) where

import Universum

import Control.Lens (assign, at, lens, makeLensesWith, uses, (%=), (.=), (<<+=))
import Data.Map (Map)
import Data.Maybe (fromJust)

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
    , sendAccountsGetter :: !(Maybe (p -> [Word32]))

    , sendOutputs :: !(Map Int SendOutput)
    , sendNextOutput :: !Int
    , sendPass :: !Text
    , sendResult :: !SendResult
    }

data SendResult
  = SendResultNone
  | SendResultWaiting !UiCommandId
  | SendResultError !Text
  | SendResultSuccess !Text  -- ^ Transaction ID

makeLensesWith postfixLFields ''SendOutput
makeLensesWith postfixLFields ''SendWidgetState

initSendWidget
  :: UiLangFace
  -> Maybe (p -> [Word32])
  -> Widget p
initSendWidget langFace accountsGetter =
  initWidget $ do
    setWidgetDrawWithFocus drawSendWidget
    setWidgetHandleEvent handleSendWidgetEvent
    setWidgetState SendWidgetState
      { sendLangFace = langFace
      , sendAccountsGetter = accountsGetter

      , sendOutputs = Map.empty
      , sendNextOutput = 0
      , sendPass = ""
      , sendResult = SendResultNone
      }

    withWidgetState addOutput
    addWidgetChild WidgetNameSendAdd $
      initButtonWidget "+"
    addWidgetChild WidgetNameSendPass $
      initPasswordWidget $ widgetParentLens sendPassL
    addWidgetChild WidgetNameSendButton $
      initButtonWidget "Send"

    addWidgetEventHandler WidgetNameSendAdd $ \case
      WidgetEventButtonPressed -> addOutput
      _ -> return ()
    addWidgetEventHandler WidgetNameSendButton $ \case
      WidgetEventButtonPressed -> performSendTransaction
      _ -> return ()

    withWidgetState updateFocusList

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

drawSendWidget :: WidgetName -> (SendWidgetState p) -> WidgetDrawM (SendWidgetState p) p (B.Widget WidgetName)
drawSendWidget focus SendWidgetState{..} = do
  widget <- ask

  let
    padBottom = B.padBottom (B.Pad 1)
    padLeft = B.padLeft (B.Pad 1)
    fillLeft w = T.takeEnd w . (T.append $ T.replicate w " ")
    fillRight w = T.take w . (flip T.append $ T.replicate w " ")

    labelWidth = 16
    amountWidth = 15

    drawChild = drawWidgetChild focus widget
    label = B.padRight (B.Pad 1) . B.txt . fillLeft labelWidth

    drawOutputsHeader = B.hBox
      [ label ""
      , B.padRight B.Max $ B.txt "Address"
      , padLeft $ B.txt $ fillRight amountWidth $ "Amount, ADA"
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

  return $
    B.vBox $
    padBottom <$>
      [ B.txt "Send transaction"
      , B.vBox $
          [drawOutputsHeader] ++
          (drawOutput <$> Map.keys sendOutputs) ++
          [drawOutputsFooter]
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
    use sendResultL >>= \case
      SendResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiSendCommandSuccess tr -> do
            sendResultL .= SendResultSuccess tr
            sendPassL .= ""
            sendOutputsL .= Map.empty
            addOutput
          UiSendCommandFailure err -> do
            sendResultL .= SendResultError err
      _ ->
        return ()
  _ ->
    return ()

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

updateFocusList :: Monad m => StateT (SendWidgetState p) (StateT (WidgetInfo (SendWidgetState p) p) m) ()
updateFocusList = do
    outputs <- uses sendOutputsL Map.keys
    lift $ setWidgetFocusList $
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

addOutput :: Monad m => StateT (SendWidgetState p) (StateT (WidgetInfo (SendWidgetState p) p) m) ()
addOutput = do
  idx <- sendNextOutputL <<+= 1
  sendOutputsL %= Map.insert idx (SendOutput "" "")
  updateFocusList

  lift $ do
    addWidgetChild (WidgetNameSendAddress idx) $
      initEditWidget $ widgetParentLens $ sendOutputsL . at idx . unsafeFromJust . sendAddressL
    addWidgetChild (WidgetNameSendAmount idx) $
      initEditWidget $ widgetParentLens $ sendOutputsL . at idx . unsafeFromJust . sendAmountL
    addWidgetChild (WidgetNameSendRemove idx) $
      initButtonWidget "-"
    addWidgetEventHandler (WidgetNameSendRemove idx) $ \case
      WidgetEventButtonPressed -> removeOutput idx
      _ -> return ()

removeOutput :: Int -> WidgetEventM (SendWidgetState p) p ()
removeOutput idx = do
  remaining <- uses sendOutputsL Map.size
  when (remaining > 1) $ do
    sendOutputsL %= Map.delete idx
    updateFocusList

performSendTransaction :: WidgetEventM (SendWidgetState p) p ()
performSendTransaction = do
  UiLangFace{..} <- use sendLangFaceL
  parentState <- lift $ lift get
  accounts <- uses sendAccountsGetterL $ maybe [] (\getter -> getter parentState)
  outputs <- fmap (\SendOutput{..} -> UiSendOutput sendAddress sendAmount) <$> uses sendOutputsL Map.elems
  passphrase <- use sendPassL
  use sendResultL >>= \case
    SendResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiSend $ UiSendArgs accounts outputs passphrase) >>=
      assign sendResultL . either SendResultError SendResultWaiting

unsafeFromJust :: Lens' (Maybe a) a
unsafeFromJust = lens fromJust setJust
  where
    setJust (Just _) b = Just b
    setJust Nothing  _ = error "setJust: Nothing"
