-- | Glue code between the frontend and the backends.

module Glue
       (
         -- * Knit ↔ Vty
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
       ) where

import Universum

import Control.Exception (displayException)
import Control.Lens (ix)
import Data.Double.Conversion.Text (toFixed)
import qualified Data.Text as T
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
     , AllConstrained (Knit.ComponentLitGrammar components) components
     , AllConstrained (Knit.ComponentInflate components) components
     , AllConstrained (Knit.ComponentCommandProcs components) components
     , AllConstrained Knit.ComponentPrinter components
     , Elem components Knit.Core
     , Elem components Knit.Cardano
     , Elem components Knit.TaskManager
     )
  => UiFace
  -> KnitFace components
  -> UiLangFace
knitFaceToUI UiFace{..} KnitFace{..} =
  UiLangFace
    { langPutCommand = putCommand commandHandle
    , langPutUiCommand = putUiCommand
    , langParse = Knit.parse
    , langAutocomplete = autocomplete
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
      Right expr -> fmap Right $ putCommand (uiCommandHandle op) expr
    uiCommandHandle op commandId = KnitCommandHandle
      { putCommandResult = \mtid result ->
          whenJust (resultToUI result op) $ putUiEvent . UiCommandResult (commandIdToUI commandId mtid)
      , putCommandOutput = \_ _ ->
          return ()
      }

    commands :: [Text]
    commands =
        let
            names = map (\(Knit.SomeCommandProc p) -> Knit.cpName p) (Knit.commandProcs @components)
            toCommand = \case
                Knit.CommandIdName n -> Just $ pretty n
                Knit.CommandIdOperator _ -> Nothing
        in
            mapMaybe toCommand names

    autocomplete :: Text -> [Text]
    autocomplete c = filter (T.isPrefixOf c) commands

    optString key value = if null value then [] else [Knit.ArgKw key . Knit.ExprLit . Knit.toLit . Knit.LitString $ value]

    opToExpr = \case
      UiSelect ws ->
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.selectCommandName
           (map (Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitNumber . fromIntegral) ws)
          )
      UiSend UiSendArgs{..} -> do
        argOutputs <- forM usaOutputs $ \UiSendOutput{..} -> do
          argAddress <- decodeTextAddress usoAddress
          argCoin <- readEither usoAmount
          Right $ Knit.ArgKw "out" . Knit.ExprProcCall $ Knit.ProcCall Knit.txOutCommandName
            [ Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitAddress $ argAddress
            , Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitNumber $ argCoin
            ]
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.sendCommandName $
            map (Knit.ArgKw "account" . Knit.ExprLit . Knit.toLit . Knit.LitNumber . fromIntegral) usaAccounts ++
            argOutputs ++
            optString "pass" usaPassphrase
          )
      UiKill commandId ->
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.killCommandName
            [Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitTaskId . TaskId $ commandId]
          )
      UiNewWallet UiNewWalletArgs{..} -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.newWalletCommandName $
            optString "name" unwaName ++
            optString "pass" unwaPassphrase
          )
      UiNewAccount UiNewAccountArgs{..} -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.newAccountCommandName $
            optString "name" unaaName
          )
      UiNewAddress -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.newAddressCommandName [])
      UiRestoreWallet UiRestoreWalletArgs{..} -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.restoreCommandName $
            [ Knit.ArgKw "mnemonic" . Knit.ExprLit . Knit.toLit . Knit.LitString $ urwaMnemonic
            , Knit.ArgKw "full" . Knit.componentInflate . Knit.ValueBool $ urwaFull
            ] ++
            optString "name" urwaName ++
            optString "pass" urwaPassphrase
          )
      UiRename UiRenameArgs{..} -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.renameCommandName $
            optString "name" uraName
          )
      _ -> Left "Not implemented"

    resultToUI result = \case
      UiSend{} ->
        Just . UiSendCommandResult . either UiSendCommandFailure UiSendCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueHash h -> Right $ pretty h
            _ -> Left "Unrecognized return value"
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

commandIdToUI :: Unique -> Maybe TaskId -> UiCommandId
commandIdToUI u mi =
  UiCommandId
    { cmdIdEqObject = fromIntegral (hashUnique u)
    , cmdTaskIdRendered = fmap (\(TaskId i) -> T.pack $ '<' : show i ++ ">") mi
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
