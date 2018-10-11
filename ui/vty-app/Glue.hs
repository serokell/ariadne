-- | Glue code between the frontend and the backends.

module Glue
       ( -- * Knit ↔ Vty
         knitFaceToUI

         -- * Cardano ↔ Vty
       , putCardanoEventToUI

         -- * Wallet ↔ Vty
       , walletEventToUI
       , putWalletEventToUI

         -- * Update ↔ Vty
       , putUpdateEventToUI

         -- * Command history ↔ Vty
       , historyToUI

         -- * Password Manager ↔ Vty
       , putPasswordEventToUI
       ) where

import qualified Control.Concurrent.Event as CE
import Control.Exception (displayException)
import Control.Lens (ix)
import Data.Double.Conversion.Text (toFixed)
import Data.Tree (Tree(..))
import Data.Unique
import qualified Data.Vector as V
import Data.Version (Version)
import NType (AllConstrained, Elem, KnownSpine)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.Cardano.Face
import Ariadne.Knit.Face
import Ariadne.TaskManager.Face
import Ariadne.UI.Vty.Face
import Ariadne.UX.CommandHistory
import Ariadne.UX.PasswordManager
import Ariadne.Wallet.Face
import Ariadne.Wallet.UiAdapter

import qualified Ariadne.Cardano.Knit as Knit
import qualified Ariadne.TaskManager.Knit as Knit
import qualified Ariadne.Wallet.Knit as Knit
import qualified Knit

----------------------------------------------------------------------------
-- Glue between Knit backend and Vty frontend
----------------------------------------------------------------------------

knitFaceToUI
  :: forall components.
     ( KnownSpine components
     , AllConstrained (Knit.ComponentTokenizer components) components
     , AllConstrained (Knit.ComponentTokenToLit components) components
     , AllConstrained (Knit.ComponentInflate components) components
     , AllConstrained (Knit.ComponentCommandProcs components) components
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
    { langPutCommand = putCommand False Nothing
    , langPutUiCommand = putUiCommand False
    , langPutUISilentCommand = putUiCommand True
    , langParse = Knit.parse
    , langAutocomplete = Knit.suggestions (Proxy @components)
    , langPpExpr = Knit.ppExpr
    , langPpParseError = Knit.ppParseError
    , langParseErrSpans = Knit.parseErrorSpans
    , langGetHelp = getKnitHelp (Proxy @components)
    }
  where
    putCommand silent mOp expr = do
      cid <- newUnique
      fmap (commandIdToUI cid) . putKnitCommand (commandHandle silent mOp cid) $ expr

    putUiCommand silent op = case opToExpr op of
      Left err -> return $ Left err
      Right expr -> do
        whenJust (extractPass op) pushPassword
        comId <- putCommand silent (Just op) expr
        unless silent $
          putUiEvent . UiCommandEvent comId . UiCommandWidget $ Knit.ppExpr expr
        return $ Right comId

    commandHandle silent mOp commandId = KnitCommandHandle
      { putCommandResult = \mtid result -> do
          unless silent $
            whenJust (knitCommandResultToUI (commandIdToUI commandId mtid) result) putUiEvent
          whenJust (resultToUI result =<< mOp) $ putUiEvent . UiCommandResult (commandIdToUI commandId mtid)
      , putCommandOutput = \tid doc -> unless silent $
          putUiEvent $ knitCommandOutputToUI (commandIdToUI commandId (Just tid)) doc
      }

    extractPass = \case
      UiNewWallet UiNewWalletArgs{..} -> Just (unwaPassphrase, WalletIdTemporary)
      UiRestoreWallet UiRestoreWalletArgs{..} -> Just (urwaPassphrase, WalletIdTemporary)
      UiSend UiSendArgs{..} -> Just (usaPassphrase, maybe WalletIdSelected WalletIdByUiIndex usaWalletIdx)
      _ -> Nothing
    pushPassword (password, walletId) = putPass walletId password Nothing

    optString key value = if null value then [] else
      [argKw key . exprLit . Knit.toLit . Knit.LitString $ value]

    justOptNumber key = maybe [] $ \value ->
      [argKw key . exprLit . Knit.toLit . Knit.LitNumber $ fromIntegral value]

    opToExpr = \case
      UiSelect ws ->
        Right $ exprProcCall
          (procCall Knit.selectCommandName $
            map (argPos . exprLit . Knit.toLit . Knit.LitNumber . fromIntegral) ws
          )
      UiSend UiSendArgs{..} -> do
        argOutputs <- forM usaOutputs $ \UiSendOutput{..} -> do
          argAddress <- decodeTextAddress usoAddress
          argCoin <- readEither usoAmount
          Right $ argKw "out" . exprProcCall $ procCall Knit.txOutCommandName
            [ argPos . exprLit . Knit.toLit . Knit.LitAddress $ argAddress
            , argPos . exprLit . Knit.toLit . Knit.LitNumber $ argCoin
            ]
        Right $ exprProcCall
          (procCall Knit.sendCommandName $
            justOptNumber "wallet" usaWalletIdx ++
            map
              (argKw "account" . exprLit . Knit.toLit . Knit.LitNumber . fromIntegral)
              usaAccounts ++
            argOutputs
          )
      UiFee UiFeeArgs{..} -> do
        -- TODO: Proper fee requesting should be implemented as part of AD-397
        Right $ exprProcCall $ procCall (Knit.CommandIdOperator Knit.OpUnit) []
      UiKill commandId ->
        Right $ exprProcCall
          (procCall Knit.killCommandName
            [argPos . exprLit . Knit.toLit . Knit.LitTaskId . TaskId $ commandId]
          )
      UiNewWallet UiNewWalletArgs{..} -> do
        Right $ exprProcCall
          (procCall Knit.newWalletCommandName $
            optString "name" unwaName
          )
      UiNewAccount UiNewAccountArgs{..} -> do
        Right $ exprProcCall
          (procCall Knit.newAccountCommandName $
            justOptNumber "wallet" unaaWalletIdx ++
            optString "name" unaaName
          )
      UiNewAddress UiNewAddressArgs{..} -> do
        Right $ exprProcCall
          (procCall Knit.newAddressCommandName $
            justOptNumber "wallet" unadaWalletIdx ++
            justOptNumber "account" unadaAccountIdx
          )
      UiRestoreWallet UiRestoreWalletArgs{..} -> do
        Right $ exprProcCall
          (procCall Knit.restoreCommandName $
            [ argKw "mnemonic" . exprLit . Knit.toLit . Knit.LitString $ urwaMnemonic
            , argKw "full" . Knit.componentInflate . Knit.ValueBool $ urwaFull
            ] ++
            optString "name" urwaName
          )
      UiRename UiRenameArgs{..} -> do
        Right $ exprProcCall
          (procCall Knit.renameCommandName $
            optString "name" uraName
          )
      _ -> Left "Not implemented"

    resultToUI result = \case
      UiSend{} ->
        Just . UiSendCommandResult . either UiSendCommandFailure UiSendCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueHash h -> Right $ pretty h
            _ -> Left "Unrecognized return value"
      UiFee{} ->
        Just . UiFeeCommandResult . UiFeeCommandSuccess $ "not implemented"
      UiNewWallet{} ->
        Just . UiNewWalletCommandResult . either UiNewWalletCommandFailure UiNewWalletCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueList l -> Right [s | Just (Knit.ValueString s) <- Knit.fromValue <$> l]
            _ -> Left "Unrecognized return value"
      UiNewAccount{} ->
        Just . UiNewAccountCommandResult . either UiNewAccountCommandFailure (const UiNewAccountCommandSuccess) $
          fromResult result
      UiNewAddress{} ->
        Just . UiNewAddressCommandResult . either UiNewAddressCommandFailure (const UiNewAddressCommandSuccess) $
          fromResult result
      UiRestoreWallet{} ->
        Just . UiRestoreWalletCommandResult . either UiRestoreWalletCommandFailure (const UiRestoreWalletCommandSuccess) $
          fromResult result
      UiRename{} ->
        Just . UiRenameCommandResult . either UiRenameCommandFailure (const UiRenameCommandSuccess) $
          fromResult result
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
    , cmdTaskIdRendered = fmap (\(TaskId i) -> toText $ '<' : show i ++ ">") mi
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
-- Glue between the Cardano backend and Vty frontend
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

----------------------------------------------------------------------------
-- Glue between the Wallet backend and Vty frontend
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

uiWalletSelectionToTreeSelection :: UiWalletSelection -> UiTreeSelection
uiWalletSelectionToTreeSelection UiWalletSelection{..} =
  UiTreeSelection { wtsWalletIdx = uwsWalletIdx, wtsPath = uwsPath }

putWalletEventToUI :: UiFace -> WalletEvent -> IO ()
putWalletEventToUI UiFace{..} ev =
  whenJust (walletEventToUI ev) putUiEvent

uiWalletDatasToTree :: [UiWalletData] -> [UiTree]
uiWalletDatasToTree = map toTree
  where
    toTree :: UiWalletData -> UiTree
    toTree UiWalletData {..} =
        Node
            { rootLabel = UiTreeItem (Just _uwdName) [] False
            , subForest = toList $ map toAccountNode _uwdAccounts
            }
      where
        toAccountNode :: UiAccountData -> UiTree
        toAccountNode UiAccountData {..} =
            Node
                { rootLabel =
                      UiTreeItem
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
        , uwiId = formatHdRootId _uwdId
        , uwiWalletIdx = uwsWalletIdx
        , uwiBalance = balance _uwdBalance
        , uwiAccounts = account <$> V.toList _uwdAccounts
        }
    account UiAccountData{..} =
      UiAccountInfo
        { uaciLabel = Just _uadName
        , uaciWalletIdx = uwsWalletIdx
        , uaciPath = [fromIntegral _uadAccountIdx]
        , uaciBalance = balance _uadBalance
        , uaciAddresses = address [fromIntegral _uadAccountIdx] <$> V.toList _uadAddresses
        }
    address acPath UiAddressData{..} =
      UiAddressInfo
        { uadiWalletIdx = uwsWalletIdx
        , uadiPath = acPath ++ [fromIntegral _uiadAddressIdx]
        , uadiAddress = pretty _uiadAddress
        , uadiBalance = balance _uiadBalance
        }
    balance n = let (amount, unit) = Knit.showCoin n in Just $ amount <> " " <> show unit

----------------------------------------------------------------------------
-- Glue between the Update backend and Vty frontend
----------------------------------------------------------------------------

putUpdateEventToUI :: UiFace -> Version -> Text -> IO ()
putUpdateEventToUI UiFace{..} ver updateURL = putUiEvent $ UiNewVersionEvent $ UiNewVersion ver updateURL

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
