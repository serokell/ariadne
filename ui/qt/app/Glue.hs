-- | Glue code between the frontend and the backends.

module Glue
       (
         -- * Knit ↔ Qt
         knitFaceToUI

         -- * Cardano ↔ Qt
       , putCardanoEventToUI

         -- * Wallet ↔ Qt
       , putWalletEventToUI

         -- * Command history ↔ Vty
       , historyToUI
       ) where

import Universum

import Control.Exception (displayException)
import Data.Double.Conversion.Text (toFixed)
import Data.List.Index (indexed)
import Data.Tree (Tree(..))
import Data.Unique
import IiExtras
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.Cardano.Face
import Ariadne.Knit.Face
import Ariadne.TaskManager.Face
import Ariadne.UI.Qt.Face
import Ariadne.UX.CommandHistory
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet
import Ariadne.Wallet.Face
import Ariadne.Wallet.UiAdapter
import Pos.Core

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
     , AllConstrained (Knit.ComponentLitGrammar components) components
     , AllConstrained (Knit.ComponentInflate components) components
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

    opToExpr = \case
      UiSelect ws ->
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.selectCommandName
           (map (Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitNumber . fromIntegral) ws)
          )
      UiBalance ->
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.balanceCommandName [])
      UiKill commandId ->
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.killCommandName
            [Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitTaskId . TaskId $ commandId]
          )
      UiSend address amount -> do
        argAddress <- decodeTextAddress address
        argCoin <- readEither amount
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.sendCommandName
            [ Knit.ArgKw "out" . Knit.ExprProcCall $ Knit.ProcCall Knit.txOutCommandName
                [ Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitAddress $ argAddress
                , Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitNumber $ argCoin
                ]
            ]
          )
      UiNewWallet name -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.newWalletCommandName
            [Knit.ArgKw "name" . Knit.ExprLit . Knit.toLit . Knit.LitString $ name]
          )
      UiNewAccount name -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.newAccountCommandName
            [Knit.ArgKw "name" . Knit.ExprLit . Knit.toLit . Knit.LitString $ name]
          )
      UiNewAddress -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.newAddressCommandName [])

    resultToUI result = \case
      UiBalance ->
        Just . UiBalanceCommandResult . either UiBalanceCommandFailure UiBalanceCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueCoin n -> Right $ let (amount, unit) = Knit.showCoin n in amount <> " " <> unit
            _ -> Left "Unrecognized return value"
      UiSend _ _ ->
        Just . UiSendCommandResult . either UiSendCommandFailure UiSendCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueHash h -> Right $ pretty h
            _ -> Left "Unrecognized return value"
      UiNewWallet _ ->
        Just . UiNewWalletCommandResult . either UiNewWalletCommandFailure UiNewWalletCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueList l -> Right [s | Just (Knit.ValueString s) <- Knit.fromValue <$> l]
            _ -> Left "Unrecognized return value"
      UiNewAccount _ ->
        Just . UiNewAccountCommandResult . either UiNewAccountCommandFailure (const UiNewAccountCommandSuccess) $
          fromResult result
      UiNewAddress ->
        Just . UiNewAddressCommandResult . either UiNewAddressCommandFailure (const UiNewAddressCommandSuccess) $
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
    Just $ UiCardanoEvent $
      UiCardanoLogEvent message
  CardanoStatusUpdateEvent CardanoStatusUpdate{..} ->
    Just $ UiCardanoEvent $
      UiCardanoStatusUpdateEvent UiCardanoStatusUpdate
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
-- Glue between the Wallet backend and Qt frontend
----------------------------------------------------------------------------

-- -- 'WalletData' like data type, used only in UI glue
-- data UiWalletData = UiWalletData
--   { _uwdName     :: !Text
--   , _uwdAccounts :: !(Vector UiAccountData)
--   } deriving (Show, Generic)

-- data UiAccountData = UiAccountData
--   { _uadName      :: !Text
--   , _uadPath      :: !Word32
--   , _uadAddresses :: !(Vector ((HdAddressChain, Word32), Address))
--   } deriving (Eq, Show, Generic)

-- -- TODO: Move common UI (vty, qt) functions to separate module
-- toUiWalletDatas :: DB -> [UiWalletData]
-- toUiWalletDatas db = toUiWalletData <$> walletList
--   where
--     -- Helpers
--     wallets = (db ^. dbHdWallets)

--     walletList :: [HdRoot]
--     walletList = toWalletsList (readAllHdRoots wallets)

--     accList :: HdRootId -> [HdAccount]
--     accList rootId =
--       map unwrapOrdByPrimKey (toAccountsList $ getAccounts rootId)

--     getAccounts :: HdRootId -> IxSet HdAccount
--     getAccounts hdRootId = fromRight
--       (error "Bug: UnknownHdRoot")
--       (readAccountsByRootId hdRootId wallets)

--     -- External chain listed first
--     addrList :: HdAccountId -> [HdAddress]
--     addrList accId =
--       map unwrapOrdByPrimKey (toAddressList $ getAddresses wallets accId)

--     getAddresses :: HdWallets -> HdAccountId -> IxSet HdAddress
--     getAddresses wallets hdAccountId = fromRight
--       (error "Bug: UnknownHdAccount")
--       (readAddressesByAccountId hdAccountId wallets)
--     ---

--     toUiWalletData :: HdRoot -> UiWalletData
--     toUiWalletData HdRoot {..} = UiWalletData
--       { _uwdName = unHdRootName _hdRootName
--       , _uwdAccounts = toUiAccountData <$> indexed $ accList _hdRootId
--       }

--     toUiAccountData :: (Word32, HdAccount) -> UiAccountData
--     toUiAccountData (accIdx, HdAccount {..}) = UiAccountData
--       { _uadName = _hdAccountName
--       -- path indexation should be the same as in selection
--       , _uadPath =  [accIdx]

--       , _uadAddresses = map toUiAddresses indexed $ addrList _hdAccountId
--       }

--     -- Because of ChainType layer Now it should be ((HdAddressChain, Word32), Address) I guess, but
--     -- AFAIU we don't want a new layer, so addresses of both types wiil be in one list -- External first.

--     -- toUiAddresses :: (Word32, HdAddress) -> (Word32, Address)
--     toUiAddresses (addrIx, HdAddress {..}) = (addrIx, _fromDb _hdAddressAddress)

--     unHdAccountIx (HdAccountIx w) = w


-- The 'Maybe' here is not used for now, but in the future might be, if some
-- event couldn't be mapped to a UI event.
walletEventToUI :: WalletEvent -> Maybe UiEvent
walletEventToUI = \case
  WalletStateSetEvent db sel ->
    Just $ UiWalletEvent $
      UiWalletUpdate
        (uiWalletDatasToTree (toUiWalletDatas db))
        (uiWalletSelectionToTreeSelection . (toUiWalletSelection db) <$> sel)

-- data UiWalletSelection = UiWalletSelection
--   { uwsWalletIdx :: Word
--   , uwsPath :: [Word]
--   }

-- toUiWalletSelection :: DB -> WalletSelection -> UiWalletSelection
-- toUiWalletSelection db WalletSelection{..} = case wsPath of
--   RootPath rootId ->
--     UiWalletSelection (getHdRootIdx rootId) []
--   AccountPath accountId ->
--     let
--       parentRootId = accountId ^. hdAccountIdParent
--     in
--       UiWalletSelection (getHdRootIdx parentRootId) [(getAccountIdx parentRootId accountId)]
--   AddressPath addressId ->
--     let
--       parentAccountId = addressId ^. hdAddressIdParent
--       parentRootId = parentAccountId ^. hdAccountIdParent
--     in
--       UiWalletSelection (getHdRootIdx parentRootId) [(getAccountIdx accountId), (getAddressIdx addressId)]
--   where
--     wallets = (db ^. dbHdWallets)

--     walletList :: [HdRoot]
--     walletList = toWalletsList (readAllHdRoots wallets)

--     -- Selection always exist
--     accountList = indexed . (map unwrapOrdByPrimKey) . toAccountsList $
--       fromRight
--         (error "Bug: parentRootId does not exist")
--         (readAccountsByRootId parentRootId (db ^. hdWallets))

--     addressList = indexed . (map unwrapOrdByPrimKey) . toAddressList $
--       fromRight
--         (error "Bug: parentRootId does not exist")
--         (readAccountsByRootId parentRootId (db ^. hdWallets))


--     getHdRootIdx rootId = fromMaybe
--       (error "Bug: selected Wallet does not exist.")
--       (head filter (\(idx, wal) -> wal ^. hdRootId == rootId) walletList)

--     getAccountIdx accountId = fromMaybe
--       (error "Bug: selected Account does not exist.")
--       (head filter (\(idx, acc) -> acc ^. hdAccountId == accountId) accountList)

--     getAddressIdx addressId = fromMaybe
--       (error "Bug: selected Address does not exist.")
--       (head filter (\(idx, addr) -> addr ^. hdAddressId == addressId) addressList)

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
                          , wtiPath = [fromIntegral _uadPath]
                          , wtiShowPath = True
                          }
                , subForest = toList $ map (toAddressNode _uadPath) _uadAddresses
                }
        toAddressNode :: Word32 -> (Word32, Address) -> UiWalletTree
        toAddressNode accIdx (addrIdx, address) =
            pure $
            UiWalletTreeItem
                { wtiLabel = Just (pretty address)
                , wtiPath = map fromIntegral [accIdx, addrIdx]
                , wtiShowPath = True
                }

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

