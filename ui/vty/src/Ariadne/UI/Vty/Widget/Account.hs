module Ariadne.UI.Vty.Widget.Account
       ( initAccountWidget
       ) where

import Universum

import Control.Lens (assign, at, lens, makeLensesWith, uses, (%=), (.=), (<<+=))
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

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

data AccountSendOutput = 
  AccountSendOutput
    { accountSendAddress :: !Text
    , accountSendAmount :: !Text
    }

data AccountWidgetState =
  AccountWidgetState
    { accountLangFace :: !UiLangFace

    , accountName :: !Text
    , accountRenameResult :: !RenameResult
    , accountDerivationPath :: ![Word32]
    , accountBalance :: !BalanceResult

    , accountSendOutputs :: !(Map Int AccountSendOutput)
    , accountSendNextOutput :: !Int
    , accountSendPass :: !Text
    , accountSendResult :: !SendResult

    , accountAddresses :: ![(Word32, Text)]
    }

data RenameResult
  = RenameResultNone
  | RenameResultWaiting !UiCommandId
  | RenameResultError !Text
  | RenameResultSuccess

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

makeLensesWith postfixLFields ''AccountSendOutput
makeLensesWith postfixLFields ''AccountWidgetState

initAccountWidget :: UiLangFace -> Widget p
initAccountWidget langFace =
  initWidget $ do
    setWidgetDrawWithFocus drawAccountWidget
    setWidgetScrollable
    setWidgetHandleEvent handleAccountWidgetEvent
    setWidgetState AccountWidgetState
      { accountLangFace = langFace

      , accountName = ""
      , accountRenameResult = RenameResultNone
      , accountDerivationPath = []
      , accountBalance = BalanceResultNone

      , accountSendOutputs = Map.empty
      , accountSendNextOutput = 0
      , accountSendPass = ""
      , accountSendResult = SendResultNone

      , accountAddresses = []
      }

    addWidgetChild WidgetNameAccountName $
      initEditWidget $ widgetParentLens accountNameL
    addWidgetChild WidgetNameAccountRenameButton $
      initButtonWidget "Rename"
    addWidgetEventHandler WidgetNameAccountRenameButton $ \case
      WidgetEventButtonPressed -> performRename
      _ -> return ()

    withWidgetState addOutput
    addWidgetChild WidgetNameAccountSendAdd $
      initButtonWidget "+"
    addWidgetChild WidgetNameAccountSendPass $
      initPasswordWidget $ widgetParentLens accountSendPassL
    addWidgetChild WidgetNameAccountSendButton $
      initButtonWidget "Send"

    addWidgetEventHandler WidgetNameAccountSendAdd $ \case
      WidgetEventButtonPressed -> addOutput
      _ -> return ()
    addWidgetEventHandler WidgetNameAccountSendButton $ \case
      WidgetEventButtonPressed -> performSendTransaction
      _ -> return ()

    withWidgetState updateFocusList

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

drawAccountWidget :: WidgetName -> AccountWidgetState -> WidgetDrawM AccountWidgetState p (B.Widget WidgetName)
drawAccountWidget focus AccountWidgetState{..} = do
  widget <- ask
  widgetName <- getWidgetName

  let
    padBottom = B.padBottom (B.Pad 1)
    padLeft = B.padLeft (B.Pad 1)
    fillLeft w = T.takeEnd w . (T.append $ T.replicate w " ")
    fillRight w = T.take w . (flip T.append $ T.replicate w " ")

    labelWidth = 16
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
      , drawChild $ WidgetNameAccountSendAddress idx
      , padLeft $ B.hLimit amountWidth $ drawChild $ WidgetNameAccountSendAmount idx
      , padLeft $ drawChild $ WidgetNameAccountSendRemove idx
      ]
    drawOutputsFooter = B.hBox
      [ B.padLeft B.Max $ drawChild WidgetNameAccountSendAdd
      ]

  return $
    B.viewport widgetName B.Vertical $
    B.padAll 1 $
    B.vBox $
    padBottom <$>
      [ label "Account name:"
          B.<+> drawChild WidgetNameAccountName
          B.<+> padLeft (drawChild WidgetNameAccountRenameButton)
      , case accountRenameResult of
          RenameResultNone -> B.emptyWidget
          RenameResultWaiting _ -> B.txt "Renaming..."
          RenameResultError err -> B.txt $ "Couldn't rename the account: " <> err
          RenameResultSuccess -> B.emptyWidget
      , label "Balance:" B.<+> case accountBalance of
          BalanceResultNone -> B.emptyWidget
          BalanceResultWaiting _ -> B.txt "calculating..."
          BalanceResultError err -> B.txt err
          BalanceResultSuccess balance -> B.txt balance

      , B.txt "Send transaction"
      , B.vBox $
          [drawOutputsHeader] ++
          (drawOutput <$> Map.keys accountSendOutputs) ++
          [drawOutputsFooter]
      , label "Passphrase:" B.<+> drawChild WidgetNameAccountSendPass
      , label "" B.<+> drawChild WidgetNameAccountSendButton
      , case accountSendResult of
          SendResultNone -> B.emptyWidget
          SendResultWaiting _ -> B.txt "Sending..."
          SendResultError err -> B.txt $ "Couldn't send a transaction: " <> err
          SendResultSuccess tr -> B.txt $ "Transaction sent: " <> tr

      , B.txt "Addresses"
      , B.vBox $ B.txt . snd <$> accountAddresses
      ]

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

handleAccountWidgetEvent
  :: UiEvent
  -> WidgetEventM AccountWidgetState p ()
handleAccountWidgetEvent = \case
  UiWalletEvent UiWalletUpdate{..} -> do
    whenJust wuPaneInfoUpdate $ \UiWalletInfo{..} -> case wpiType of
      Just (UiWalletInfoAccount dp) -> do
        UiLangFace{..} <- use accountLangFaceL
        accountNameL .= fromMaybe "" wpiLabel
        accountDerivationPathL .= dp
        accountAddressesL .= wpiAddresses
        use accountBalanceL >>= \case
          BalanceResultWaiting commandId
            | Just taskId <- cmdTaskId commandId ->
                void . liftIO . langPutUiCommand $ UiKill taskId
          _ -> return ()
        liftIO (langPutUiCommand UiBalance) >>=
          assign accountBalanceL . either BalanceResultError BalanceResultWaiting
      _ -> return ()
  UiCommandResult commandId (UiRenameCommandResult result) -> do
    accountRenameResultL %= \case
      RenameResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiRenameCommandSuccess -> RenameResultSuccess
          UiRenameCommandFailure err -> RenameResultError err
      other -> other
  UiCommandResult commandId (UiBalanceCommandResult result) -> do
    accountBalanceL %= \case
      BalanceResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiBalanceCommandSuccess balance -> BalanceResultSuccess balance
          UiBalanceCommandFailure err -> BalanceResultError err
      other -> other
  UiCommandResult commandId (UiSendCommandResult result) -> do
    use accountSendResultL >>= \case
      SendResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiSendCommandSuccess tr -> do
            accountSendResultL .= SendResultSuccess tr
            accountSendPassL .= ""
            accountSendOutputsL .= Map.empty
            addOutput
          UiSendCommandFailure err -> do
            accountSendResultL .= SendResultError err
      _ ->
        return ()
  _ ->
    return ()

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

updateFocusList :: Monad m => StateT AccountWidgetState (StateT (WidgetInfo AccountWidgetState p) m) ()
updateFocusList = do
    outputs <- uses accountSendOutputsL Map.keys
    lift $ setWidgetFocusList $
      [ WidgetNameAccountName
      , WidgetNameAccountRenameButton
      ] ++
      concat (outputFocuses <$> outputs) ++
      [ WidgetNameAccountSendAdd
      , WidgetNameAccountSendPass
      , WidgetNameAccountSendButton
      ]
  where
    outputFocuses idx =
      [ WidgetNameAccountSendAddress idx
      , WidgetNameAccountSendAmount idx
      , WidgetNameAccountSendRemove idx
      ]

performRename :: WidgetEventM AccountWidgetState p ()
performRename = do
  UiLangFace{..} <- use accountLangFaceL
  name <- use accountNameL
  use accountRenameResultL >>= \case
    RenameResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiRename name) >>=
      assign accountRenameResultL . either RenameResultError RenameResultWaiting

addOutput :: Monad m => StateT AccountWidgetState (StateT (WidgetInfo AccountWidgetState p) m) ()
addOutput = do
  idx <- accountSendNextOutputL <<+= 1
  accountSendOutputsL %= Map.insert idx (AccountSendOutput "" "")
  updateFocusList

  lift $ do
    addWidgetChild (WidgetNameAccountSendAddress idx) $
      initEditWidget $ widgetParentLens $ accountSendOutputsL . at idx . unsafeFromJust . accountSendAddressL
    addWidgetChild (WidgetNameAccountSendAmount idx) $
      initEditWidget $ widgetParentLens $ accountSendOutputsL . at idx . unsafeFromJust . accountSendAmountL
    addWidgetChild (WidgetNameAccountSendRemove idx) $
      initButtonWidget "-"
    addWidgetEventHandler (WidgetNameAccountSendRemove idx) $ \case
      WidgetEventButtonPressed -> removeOutput idx
      _ -> return ()

removeOutput :: Int -> WidgetEventM AccountWidgetState p ()
removeOutput idx = do
  remaining <- uses accountSendOutputsL Map.size
  when (remaining > 1) $ do
    accountSendOutputsL %= Map.delete idx
    updateFocusList

performSendTransaction :: WidgetEventM AccountWidgetState p ()
performSendTransaction = do
  UiLangFace{..} <- use accountLangFaceL
  outputs <- fmap (\AccountSendOutput{..} -> (accountSendAddress, accountSendAmount)) <$> uses accountSendOutputsL Map.elems
  passphrase <- use accountSendPassL
  use accountSendResultL >>= \case
    SendResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiSend [] outputs passphrase) >>=
      assign accountSendResultL . either SendResultError SendResultWaiting

unsafeFromJust :: Lens' (Maybe a) a
unsafeFromJust = lens fromJust setJust
  where
    setJust (Just _) b = Just b
    setJust Nothing  _ = error "setJust: Nothing"
