-- | Glue code between the frontend and the backends.

module Glue
       ( -- * Knit ↔ Qt
         knitFaceToUI

         -- * Cardano ↔ Qt
       , putCardanoEventToUI

         -- * Wallet ↔ Qt
       , putWalletEventToUI

         -- * Command history ↔ Vty
       , historyToUI

         -- * Password Manager ↔ Vty
       , putPasswordEventToUI

         -- * Cardano ↔ Qt
       , putBackendErrorToUI
       ) where

import qualified Control.Concurrent.Event as CE
import Control.Exception (displayException)
import Control.Lens (ix)
import Data.Double.Conversion.Text (toFixed)
import Data.Tree (Tree(..))
import Data.Unique
import qualified Data.Vector as V
import NType (AllConstrained, Elem, KnownSpine)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.Cardano.Face
import Ariadne.Knit.Face
import Ariadne.TaskManager.Face
import Ariadne.UI.Qt.Face
import Ariadne.UX.CommandHistory
import Ariadne.UX.PasswordManager
import Ariadne.Wallet.Face
import Ariadne.Wallet.UiAdapter

import qualified Ariadne.Cardano.Knit as Knit
import qualified Ariadne.TaskManager.Knit as Knit
import qualified Ariadne.Wallet.Knit as Knit
import qualified Knit

----------------------------------------------------------------------------
-- Glue between Knit backend and Qt frontend
----------------------------------------------------------------------------

knitFaceToUI
  :: forall components.
     ( KnownSpine components
     , AllConstrained (Knit.ComponentTokenizer components) components
     , AllConstrained (Knit.ComponentTokenToLit components) components
     , AllConstrained (Knit.ComponentInflate components) components
     , AllConstrained Knit.ComponentPrinter components
     , Elem components Knit.Core
     , Elem components Knit.Cardano
     , Elem components Knit.TaskManager
     )
  => UiFace
  -> KnitFace components
  -> PutPassword
  -> UiLangFace
knitFaceToUI UiFace{..} KnitFace{..} putPass =
  UiLangFace
    { langPutCommand = putCommand commandHandle
    , langPutUiCommand = putUiCommand
    , langParse = Knit.parse
    , langPpExpr = Knit.ppExpr
    , langPpParseError = Knit.ppParseError
    , langParseErrSpans = Knit.parseErrorSpans
    , langGetHelp = getKnitHelp (Proxy @components)
    }
  where
    putCommand handle expr = do
      cid <- newUnique
      fmap (commandIdToUI cid) . putKnitCommand (handle cid) $ expr
    commandHandle commandId = KnitCommandHandle
      { putCommandResult = \mtid result ->
          whenJust (knitCommandResultToUI (commandIdToUI commandId mtid) result) putUiEvent
      , putCommandOutput = \tid doc ->
          putUiEvent $ knitCommandOutputToUI (commandIdToUI commandId (Just tid)) doc
      }

    putUiCommand op = case opToExpr op of
      Left err -> return $ Left err
      Right expr -> do
        whenJust (extractPass op) pushPassword
        fmap Right $ putCommand (uiCommandHandle op) expr
    uiCommandHandle op commandId = KnitCommandHandle
      { putCommandResult = \mtid result ->
          whenJust (resultToUI result op) $
            putUiEvent . UiCommandResult (commandIdToUI commandId mtid)
      , putCommandOutput = \_ _ ->
          pass
      }

    extractPass = \case
      UiNewWallet _ maybePass -> Just $ fromMaybe "" maybePass
      UiRestoreWallet _ maybePass _ -> Just $ fromMaybe "" maybePass
      _ -> Nothing
    pushPassword password = putPass WalletIdTemporary password Nothing

    opToExpr = \case
      UiSelect ws ->
        Right $ exprProcCall
          (procCall Knit.selectCommandName $
            map (argPos . exprLit . Knit.toLit . Knit.LitNumber . fromIntegral) ws
          )
      UiKill commandId ->
        Right $ exprProcCall
          (procCall Knit.killCommandName
            [argPos . exprLit . Knit.toLit . Knit.LitTaskId . TaskId $ commandId]
          )
      UiSend UiSendArgs{..} -> do
        argOutputs <- forM usaOutputs $ \UiSendOutput{..} -> do
          argAddress <- decodeTextAddress usoAddress
          Right $ argKw "out" . exprProcCall $ procCall Knit.txOutCommandName
            [ argPos . exprLit . Knit.toLit . Knit.LitAddress $ argAddress
            , argPos . exprLit . Knit.toLit . Knit.LitNumber $ usoAmount
            ]
        Right $ exprProcCall
          (procCall Knit.sendCommandName $
            [ argKw "wallet" . exprLit . Knit.toLit . Knit.LitNumber . fromIntegral $ usaWalletIdx
            ] ++
            map
              (argKw "account" . exprLit . Knit.toLit . Knit.LitNumber . fromIntegral) usaAccounts ++
            argOutputs
          )
      UiCalcTotal amounts -> do
        argAmounts <- forM amounts $ Right . (argKw "amount") . exprLit . Knit.toLit . Knit.LitNumber
        Right $ exprProcCall
          (procCall Knit.sumCoinsCommamdName argAmounts)
      UiNewWallet name _ -> do
        Right $ exprProcCall
          (procCall Knit.newWalletCommandName $
            [ argKw "name" . exprLit . Knit.toLit . Knit.LitString $ name
            ]
          )
      UiRestoreWallet name _ mnemonic -> do
        Right $ exprProcCall
          (procCall Knit.restoreCommandName $
            [ argKw "name" . exprLit . Knit.toLit . Knit.LitString $ name
            , argKw "mnemonic" . exprLit . Knit.toLit . Knit.LitString $ mnemonic
            ]
          )
      UiNewAccount name -> do
        Right $ exprProcCall
          (procCall Knit.newAccountCommandName
            [argKw "name" . exprLit . Knit.toLit . Knit.LitString $ name]
          )
      UiNewAddress wIdx aIdx -> do
        Right $ exprProcCall
          (procCall Knit.newAddressCommandName
            [ argKw "wallet" . exprLit . Knit.toLit . Knit.LitNumber . fromIntegral $ wIdx
            , argKw "account" . exprLit . Knit.toLit . Knit.LitNumber . fromIntegral $ aIdx
            ]
          )

      UiRename newName -> do
        Right $ exprProcCall
          (procCall Knit.renameCommandName $
            [argKw "name" . exprLit . Knit.toLit . Knit.LitString $ newName]
          )
      UiRemoveCurrentItem -> do
        Right $ exprProcCall $ procCall Knit.removeCommandName []

    resultToUI result = \case
      UiSend {} ->
        Just . UiSendCommandResult . either UiSendCommandFailure UiSendCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueHash h -> Right $ pretty h
            _ -> Left "Unrecognized return value"
      UiCalcTotal {} ->
        Just . UiCalcTotalCommandResult . either UiCalcTotalCommandFailure UiCalcTotalCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueCoin c ->
              let (amount, unit) = Knit.showCoin c in Right (amount, show unit)
            e -> Left $ "Unrecognized return value: " <> show e
      UiNewWallet {} ->
        Just . UiNewWalletCommandResult .
          either UiNewWalletCommandFailure (const UiNewWalletCommandSuccess) $
          fromResult result
      UiRestoreWallet {} ->
        Just . UiRestoreWalletCommandResult .
          either UiRestoreWalletCommandFailure (const UiRestoreWalletCommandSuccess) $
          fromResult result
      UiNewAccount _ ->
        Just . UiNewAccountCommandResult .
          either UiNewAccountCommandFailure (const UiNewAccountCommandSuccess) $
          fromResult result
      UiNewAddress wIdx aIdx ->
        Just . UiNewAddressCommandResult .
          either UiNewAddressCommandFailure (UiNewAddressCommandSuccess wIdx aIdx) $
          fromResult result >>= fromValue >>= \case
            Knit.ValueAddress a -> Right $ pretty a
            _ -> Left "Unrecognized return value"
      _ -> Nothing

    fromResult = \case
      KnitCommandSuccess v -> Right v
      KnitCommandEvalError _ -> Left $ "Invalid arguments"
      KnitCommandException e -> Left $ fromString $ displayException e
      KnitCommandProcError _ -> error "Undefined command used"

    fromValue
      :: Elem components component
      => Knit.Value components
      -> Either Text (Knit.ComponentValue components component)
    fromValue = maybeToRight "Unrecognized return value" . Knit.fromValue

    exprProcCall = Knit.ExprProcCall Knit.NoExt
    exprLit = Knit.ExprLit Knit.NoExt
    procCall = Knit.ProcCall Knit.NoExt
    argPos = Knit.ArgPos Knit.NoExt
    argKw = Knit.ArgKw Knit.NoExt

commandIdToUI :: Unique -> Maybe TaskId -> UiCommandId
commandIdToUI u mi =
  UiCommandId
    { cmdIdEqObject = fromIntegral (hashUnique u)
    , cmdTaskIdRendered = fmap (\(TaskId i) -> show i) mi
    , cmdTaskId = fmap (\(TaskId i) -> i) mi
    }

-- The 'Maybe' here is not used for now, but in the future might be, if some
-- event couldn't be mapped to a UI event.
knitCommandResultToUI
  :: forall components.
     ( AllConstrained Knit.ComponentPrinter components
     , AllConstrained (Knit.ComponentInflate components) components
     )
  => UiCommandId
  -> KnitCommandResult components
  -> Maybe UiEvent
knitCommandResultToUI commandId = Just . UiCommandEvent commandId . \case
  KnitCommandSuccess v ->
    UiCommandSuccess $ Knit.ppValue v
  KnitCommandEvalError e ->
    UiCommandFailure $ Knit.ppEvalError e
  KnitCommandProcError e ->
    UiCommandFailure $ Knit.ppResolveErrors e
  KnitCommandException e ->
    UiCommandFailure $ PP.text (displayException e)

knitCommandOutputToUI :: UiCommandId -> PP.Doc -> UiEvent
knitCommandOutputToUI commandId doc = UiCommandEvent commandId (UiCommandOutput doc)

----------------------------------------------------------------------------
-- Glue between the Cardano backend and Qt frontend
----------------------------------------------------------------------------

-- The 'Maybe' here is not used for now, but in the future might be, if some
-- event couldn't be mapped to a UI event.
cardanoEventToUI :: CardanoEvent -> Maybe UiEvent
cardanoEventToUI = \case
  CardanoLogEvent message ->
    Just $ UiBackendEvent $
      UiBackendLogEvent message
  CardanoStatusUpdateEvent CardanoStatusUpdate{..} ->
    Just $ UiBackendEvent $
      UiBackendStatusUpdateEvent UiBackendStatusUpdate
        { syncProgress = (<> "%") . toFixed 1 . fromRational . (* 100) <$> syncProgress
        , blockchainLocal = "block " <> pretty tipHeaderHash <> ", " <> pEpochOrSlot tipEpochOrSlot
        , blockchainNetwork = pSlotId currentSlot
        }
  where
    pEpoch = ("epoch " <>) . pretty . getEpochIndex
    pSlotId SlotId{..} = pEpoch siEpoch <> ", slot " <> pretty (getSlotIndex siSlot)
    pEpochOrSlot = either pEpoch pSlotId . unEpochOrSlot

putCardanoEventToUI :: UiFace -> CardanoEvent -> IO ()
putCardanoEventToUI UiFace{..} ev =
  whenJust (cardanoEventToUI ev) putUiEvent

-- Called when an exception in backend thread occurs
putBackendErrorToUI :: UiFace -> SomeException -> IO ()
putBackendErrorToUI UiFace{..} e = putUiEvent . UiBackendExceptionEvent $
    UiBackendException e

----------------------------------------------------------------------------
-- Glue between the Wallet backend and Qt frontend
----------------------------------------------------------------------------

-- The 'Maybe' here is not used for now, but in the future might be, if some
-- event couldn't be mapped to a UI event.
walletEventToUI :: WalletEvent -> Maybe UiEvent
walletEventToUI = \case
  WalletStateSetEvent db sel ->
    Just $ UiWalletEvent $
      UiWalletUpdate
        (uiWalletDatasToTree (toUiWalletDatas db))
        (uiWalletSelectionToTreeSelection . (toUiWalletSelection db) <$> sel)
        ((walletSelectionToInfo (toUiWalletDatas db)) . (toUiWalletSelection db) <$> sel)
  WalletRequireConfirm resVar confirmationType ->
    Just . UiConfirmEvent . UiConfirmRequest resVar $ case confirmationType of
      ConfirmMnemonic mnemonic -> UiConfirmMnemonic mnemonic
      ConfrimRemoveWallet walletName ->
          UiConfirmRemove $ UiDelWallet $ Just $ unWalletName walletName
      ConfirmRemoveAccount accountName ->
          UiConfirmRemove $ UiDelAccount $ Just $ unAccountName accountName
      ConfirmSend confsendInfo -> UiConfirmSend $ map toUiConfirmSendInfo confsendInfo
      ConfirmDelUnknownKeys unknownKeys -> UiConfirmRemove $ UiDelUnknownKeys unknownKeys
      ConfirmDelBrokenWallets walletsWithMissingKeys
        -> UiConfirmRemove $ UiDelBrokenWallets walletsWithMissingKeys


toUiConfirmSendInfo :: ConfirmSendInfo -> UiConfirmSendInfo
toUiConfirmSendInfo ConfirmSendInfo{..} = UiConfirmSendInfo {..}
  where
    csiAddress = confirmSendAddress
    csiAmount = confirmSendAmount
    csiCoin = confirmSendCoin

uiWalletSelectionToTreeSelection :: UiWalletSelection -> UiWalletTreeSelection
uiWalletSelectionToTreeSelection UiWalletSelection{..} =
  UiWalletTreeSelection { wtsWalletIdx = uwsWalletIdx, wtsPath = uwsPath }

putWalletEventToUI :: UiFace -> WalletEvent -> IO ()
putWalletEventToUI UiFace{..} ev =
  whenJust (walletEventToUI ev) putUiEvent

uiWalletDatasToTree :: [UiWalletData] -> [UiWalletTree]
uiWalletDatasToTree = map toTree
  where
    toTree :: UiWalletData -> UiWalletTree
    toTree UiWalletData {..} =
        Node
            { rootLabel = UiWalletTreeItem (Just _uwdName) [] False
            , subForest = toList $ map toAccountNode _uwdAccounts
            }
      where
        toAccountNode :: UiAccountData -> UiWalletTree
        toAccountNode UiAccountData {..} =
            Node
                { rootLabel =
                      UiWalletTreeItem
                          { wtiLabel = Just _uadName
                          , wtiPath = [fromIntegral _uadAccountIdx]
                          , wtiShowPath = True
                          }
                , subForest = []
                }

-- TODO: change to use chain type level
walletSelectionToInfo :: [UiWalletData] -> UiWalletSelection -> UiSelectionInfo
walletSelectionToInfo uiwd UiWalletSelection{..} =
  case uiwd ^? ix (fromIntegral uwsWalletIdx) of
    Nothing -> error "Invalid wallet index"
    Just walletData@UiWalletData{..} -> case uwsPath of
      [] -> UiSelectionWallet $ wallet walletData
      accIdx:_ -> case _uwdAccounts ^? ix (fromIntegral accIdx) of
        Nothing -> error "Invalid account index"
        Just accountData -> UiSelectionAccount $ account accountData
  where
    wallet UiWalletData{..} =
      UiWalletInfo
        { uwiLabel = Just _uwdName
        , uwiWalletIdx = uwsWalletIdx
        , uwiBalance = balance _uwdBalance
        , uwiAccounts = account <$> V.toList _uwdAccounts
        }
    account UiAccountData{..} =
      UiAccountInfo
        { uaciLabel = Just _uadName
        , uaciWalletIdx = uwsWalletIdx
        , uaciPath = fromIntegral _uadAccountIdx :| []
        , uaciBalance = balance _uadBalance
        , uaciAddresses = address (fromIntegral _uadAccountIdx) <$> V.toList _uadAddresses
        }
    address acPath UiAddressData{..} =
      UiAddressInfo
        { uadiWalletIdx = uwsWalletIdx
        , uadiPath = acPath :| [fromIntegral _uiadAddressIdx]
        , uadiAddress = pretty _uiadAddress
        , uadiBalance = balance _uiadBalance
        }
    balance n = let (amount, unit) = Knit.showCoin n in (amount, unitToUI unit)
    unitToUI Knit.ADA = ADA
    unitToUI Knit.Lovelace = Lovelace

----------------------------------------------------------------------------
-- Glue between command history and Vty frontend
----------------------------------------------------------------------------

historyToUI :: CommandHistory -> UiHistoryFace
historyToUI ch = UiHistoryFace
  { historyAddCommand = addCommand ch
  , historySetPrefix = setPrefix ch
  , historyNextCommand = toNextCommand ch
  , historyPrevCommand = toPrevCommand ch
  }

----------------------------------------------------------------------------
-- Glue between the Password Manager and Vty frontend
----------------------------------------------------------------------------

putPasswordEventToUI :: UiFace -> WalletId -> CE.Event -> IO ()
putPasswordEventToUI UiFace{..} walletId cEvent = putUiEvent . UiPasswordEvent $
    UiPasswordRequest walletId cEvent
