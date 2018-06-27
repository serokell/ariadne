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
       , uiGetSelectedItem

         -- * Update ↔ Vty
       , putUpdateEventToUI

         -- * Command history ↔ Vty
       , historyToUI
       ) where

import Universum

import Control.Exception (displayException)
import Control.Lens (ix)
import Data.Double.Conversion.Text (toFixed)
import Data.Text (pack)
import Data.Tree (Tree(..))
import Data.Unique
import Data.Version (Version)
import IiExtras
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.Knit.Face
import Ariadne.TaskManager.Face
import Ariadne.UI.Vty.Face
import Ariadne.UX.CommandHistory
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read
import Ariadne.Wallet.Face
-- import Pos.Core

import qualified Ariadne.Cardano.Knit as Knit
import qualified Ariadne.TaskManager.Knit as Knit
import qualified Ariadne.UI.Vty.Knit as Knit
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet as IxSet
import qualified Ariadne.Wallet.Knit as Knit
import qualified Knit

import Data.List.Index (indexed)

----------------------------------------------------------------------------
-- Glue between Knit backend and Vty frontend
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
      UiSend address amount passphrase -> do
        argAddress <- decodeTextAddress address
        argCoin <- readEither amount
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.sendCommandName $
            [ Knit.ArgKw "out" . Knit.ExprProcCall $ Knit.ProcCall Knit.txOutCommandName
                [ Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitAddress $ argAddress
                , Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitNumber $ argCoin
                ]
            ] ++
            (if null passphrase then [] else [Knit.ArgKw "pass" . Knit.ExprLit . Knit.toLit . Knit.LitString $ passphrase])
          )
      UiKill commandId ->
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.killCommandName
            [Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitTaskId . TaskId $ commandId]
          )
      UiCopySelection -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.copySelectionCommandName [])
      UiNewWallet name passphrase -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.newWalletCommandName $
            (if null name then [] else [Knit.ArgKw "name" . Knit.ExprLit . Knit.toLit . Knit.LitString $ name]) ++
            (if null passphrase then [] else [Knit.ArgKw "pass" . Knit.ExprLit . Knit.toLit . Knit.LitString $ passphrase])
          )
      UiRestoreWallet name mnemonic passphrase full -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.restoreCommandName $
            [ Knit.ArgKw "mnemonic" . Knit.ExprLit . Knit.toLit . Knit.LitString $ mnemonic
            , Knit.ArgKw "full" . Knit.componentInflate . Knit.ValueBool $ full
            ] ++
            (if null name then [] else [Knit.ArgKw "name" . Knit.ExprLit . Knit.toLit . Knit.LitString $ name]) ++
            (if null passphrase then [] else [Knit.ArgKw "pass" . Knit.ExprLit . Knit.toLit . Knit.LitString $ passphrase])
          )

    resultToUI result = \case
      UiBalance ->
        Just . UiBalanceCommandResult . either UiBalanceCommandFailure UiBalanceCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueCoin n -> Right $ let (amount, unit) = Knit.showCoin n in amount <> " " <> unit
            _ -> Left "Unrecognized return value"
      UiSend _ _ _ ->
        Just . UiSendCommandResult . either UiSendCommandFailure UiSendCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueHash h -> Right $ pretty h
            _ -> Left "Unrecognized return value"
      UiNewWallet _ _ ->
        Just . UiNewWalletCommandResult . either UiNewWalletCommandFailure UiNewWalletCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueList l -> Right [s | Just (Knit.ValueString s) <- Knit.fromValue <$> l]
            _ -> Left "Unrecognized return value"
      UiRestoreWallet _ _ _ _ ->
        Just . UiRestoreWalletCommandResult . either UiRestoreWalletCommandFailure (const UiRestoreWalletCommandSuccess) $
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
    , cmdTaskIdRendered = fmap (\(TaskId i) -> pack $ '<' : show i ++ ">") mi
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
-- Glue between the Wallet backend and Vty frontend
----------------------------------------------------------------------------

-- 'WalletData' like data type, used only in UI glue

data UiWalletData = UiWalletData
  { _uwdName     :: !Text
  , _uwdAccounts :: !(Vector UiAccountData)
  } deriving (Show, Generic)

data UiAccountData = UiAccountData
  { _uadName      :: !Text
  , _uadPath      :: !Word32
  , _uadAddresses :: !(Vector ((HdAddressChain, Word32), Address))
  } deriving (Eq, Show, Generic)

-- TODO: Move common UI (vty, qt) functions to separate module
toUiWalletDatas :: DB -> [UiWalletData]
toUiWalletDatas db = toUiWalletData <$> walletList
  where
    -- Helpers
    wallets = (db ^. dbHdWallets)

    walletList :: [HdRoot]
    walletList = toWalletsList (readAllHdRoots wallets)

    accList :: HdRootId -> [HdAccount]
    accList rootId =
      map unwrapOrdByPrimKey (toAccountsList $ getAccounts rootId)

    getAccounts :: HdRootId -> IxSet HdAccount
    getAccounts hdRootId = fromRight
      (error "Bug: UnknownHdRoot")
      (readAccountsByRootId hdRootId wallets)

    -- External chain listed first
    addrList :: HdAccountId -> [HdAddress]
    addrList accId =
      map unwrapOrdByPrimKey (toAddressList $ getAddresses wallets accId)

    getAddresses :: HdWallets -> HdAccountId -> IxSet HdAddress
    getAddresses wallets hdAccountId = fromRight
      (error "Bug: UnknownHdAccount")
      (readAddressesByAccountId hdAccountId wallets)
    ---

    toUiWalletData :: HdRoot -> UiWalletData
    toUiWalletData HdRoot {..} = UiWalletData
      { _uwdName = unHdRootName _hdRootName
      , _uwdAccounts = toUiAccountData <$> indexed $ accList _hdRootId
      }

    toUiAccountData :: (Word32, HdAccount) -> UiAccountData
    toUiAccountData (accIdx, HdAccount {..}) = UiAccountData
      { _uadName = _hdAccountName
      -- path indexation should be the same as in selection
      , _uadPath =  [accIdx]

      , _uadAddresses = map toUiAddresses indexed $ addrList _hdAccountId
      }

    -- Because of ChainType layer Now it should be ((HdAddressChain, Word32), Address) I guess, but
    -- AFAIU we don't want a new layer, so addresses of both types wiil be in one list -- External first.

    -- toUiAddresses :: (Word32, HdAddress) -> (Word32, Address)
    toUiAddresses (addrIx, HdAddress {..}) = (addrIx, _fromDb _hdAddressAddress)

    unHdAccountIx (HdAccountIx w) = w

-- The 'Maybe' here is not used for now, but in the future might be, if some
-- event couldn't be mapped to a UI event.
walletEventToUI :: WalletEvent -> Maybe UiEvent
walletEventToUI = \case
  WalletStateSetEvent db sel ->
    Just $ UiWalletEvent $
      UiWalletUpdate
        (uiWalletDatasToTree uiwd)
        (uiWalletSelectionToTreeSelection . (toUiWalletSelection db) <$> sel)
        ((walletSelectionToPane uiwd) . (toUiWalletSelection db) <$> sel)
  where
    uiwd = toUiWalletDatas (db ^. hdWallets)

data UiWalletSelection = UiWalletSelection
  { uwsWalletIdx :: Word
  , uwsPath :: [Word]
  }

toUiWalletSelection :: DB -> WalletSelection -> UiWalletSelection
toUiWalletSelection db WalletSelection{..} = case wsPath of
  RootPath rootId ->
    UiWalletSelection (getHdRootIdx rootId) []
  AccountPath accountId ->
    let
      parentRootId = accountId ^. hdAccountIdParent
    in
      UiWalletSelection (getHdRootIdx parentRootId) [(getAccountIdx parentRootId accountId)]
  AddressPath addressId ->
    let
      parentAccountId = addressId ^. hdAddressIdParent
      parentRootId = parentAccountId ^. hdAccountIdParent
    in
      UiWalletSelection (getHdRootIdx parentRootId) [(getAccountIdx accountId), (getAddressIdx addressId)]
  where
    walletList :: [HdRoot]
    walletList = toWalletsList (readAllHdRoots wallets)

    -- Selection always exist
    accountList = indexed . (map unwrapOrdByPrimKey) . toAccountsList $
      fromRight
        (error "Bug: parentRootId does not exist")
        (readAccountsByRootId parentRootId (db ^. hdWallets))

    addressList = indexed . (map unwrapOrdByPrimKey) . toAddressList $
      fromRight
        (error "Bug: parentRootId does not exist")
        (readAccountsByRootId parentRootId (db ^. hdWallets))

    getHdRootIdx rootId = fromMaybe
      (error "Bug: selected Wallet does not exist.")
      (head filter (\(idx, wal) -> wal ^. hdRootId == rootId) walletList)

    getAccountIdx accountId = fromMaybe
      (error "Bug: selected Account does not exist.")
      (head filter (\(idx, acc) -> acc ^. hdAccountId == accountId) accountList)

    getAddressIdx addressId = fromMaybe
      (error "Bug: selected Address does not exist.")
      (head filter (\(idx, addr) -> addr ^. hdAddressId == addressId) addressList)

uiWalletSelectionToTreeSelection :: UiWalletSelection -> UiTreeSelection
uiWalletSelectionToTreeSelection UiWalletSelection{..} =
  UiTreeSelection { wtsWalletIdx = uwsWalletIndex, wtsPath = uwsPath }


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
                          , wtiPath = [fromIntegral _uadPath]
                          , wtiShowPath = True
                          }
                , subForest = toList $ map (toAddressNode _uadPath) _uadAddresses
                }
        toAddressNode :: Word32 -> (Word32, Address) -> UiTree
        toAddressNode accIdx (addrIdx, address) =
            pure $
            UiTreeItem
                { wtiLabel = Just (pretty address)
                , wtiPath = map fromIntegral [accIdx, addrIdx]
                , wtiShowPath = True
                }

-- TODO: chnge to use chain type level
walletSelectionToPane :: [UiWalletData] -> UiWalletSelection -> UiWalletInfo
walletSelectionToPane uiwd UiWalletSelection{..} = UiWalletInfo{..}
  where
    wpiWalletIdx = uwsWalletIndex
    wpiPath = uwsPath
    (wpiType, wpiLabel) = case uiwd ^? ix (fromIntegral wsWalletIndex) of
      Nothing -> error "Invalid wallet index"
      Just UiWalletData{..} -> case wsPath of
        [] -> (Just UiWalletInfoWallet, Just _uwdName)
        accIdx:accPath -> case _uwdAccounts ^? ix (fromIntegral accIdx) of
          Nothing -> error "Invalid account index"
          Just UiAccountData{..} -> case accPath of
            [] -> (Just $ UiWalletAccount [_uadPath], Just _uadName)
            addrIdx:_ -> case _uadAddresses ^? ix (fromIntegral addrIdx) of
              Nothing -> error "Invalid address index"
              Just (addrPath, address) -> (Just $ UiWalletAddress [_uadPath, addrPath], Just $ pretty address)

-- | Get currently selected item from the backend and convert it to
-- 'UiSelectedItem'.

-- TODO: Convert backend WalletSelection to UiWalletSelection
-- QUESTION: Need DB to be passed. How caller can do that?
uiGetSelectedItem :: WalletFace -> IO UiSelectedItem
uiGetSelectedItem WalletFace {walletGetSelection} = undefined
  --   walletGetSelection <&> \case
  --       (Nothing, _) -> UiNoSelection
  --       (Just WalletSelection {..}, us) ->
  --           getItem (us ^? usWallets . ix (fromIntegral wsWalletIndex)) wsPath
  -- where
  --   getItem :: Maybe WalletData -> [Word] -> UiSelectedItem
  --   getItem Nothing _ = error "Non-existing wallet is selected"
  --   getItem (Just wd) [] = UiSelectedWallet (_uwdName wd)
  --   getItem (Just wd) (accIdx:rest) =
  --       case wd ^? wdAccounts . ix (fromIntegral accIdx) of
  --           Nothing -> error "Non-existing account is selected"
  --           Just ad ->
  --               case rest of
  --                   [] -> UiSelectedAccount (_uadName ad)
  --                   [addrIdx] ->
  --                       case ad ^? adAddresses . ix (fromIntegral addrIdx) of
  --                           Nothing -> error "Non-existing address is selected"
  --                           Just (_, addr) -> UiSelectedAddress (pretty addr)
  --                   _ -> error "Invalid selection: too long"

----------------------------------------------------------------------------
-- Glue between the Update backend and Vty frontend
----------------------------------------------------------------------------

putUpdateEventToUI :: UiFace -> Version -> IO ()
putUpdateEventToUI UiFace{..} ver = putUiEvent $ UiNewVersionEvent ver

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
