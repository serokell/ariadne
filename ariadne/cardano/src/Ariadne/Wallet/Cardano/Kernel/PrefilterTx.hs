{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeFamilies #-}

module Ariadne.Wallet.Cardano.Kernel.PrefilterTx
       ( PrefilteredBlock(..)
       , PrefilteredUtxo
       , AddrWithId
       , emptyPrefilteredBlock
       , pfbInputs
       , pfbOutputs
       , prefilterBlock
       , prefilterUtxo
       ) where

import Data.List (nub)
import qualified Data.List.NonEmpty as NE

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Buildable
import Data.Text.Lazy.Builder (Builder)
import Formatting (Format, bprint, later, (%))
import Serokell.Util (listBuilder, pairBuilder)

import Data.SafeCopy (base, deriveSafeCopySimple)

import Ariadne.Wallet.Cardano.Kernel.Decrypt
  (HDPassphrase, WAddressMeta(..), eskToHDPassphrase, selectOwnAddresses)
import Pos.Core (Address(..), SlotId)
import Pos.Core.Txp (TxId, TxIn(..), TxOut(..), TxOutAux(..))
import Pos.Crypto (EncryptedSecretKey)
import Pos.Txp (Utxo, formatUtxo)

import Ariadne.Wallet.Cardano.Kernel.Bip44 (Bip44DerivationPath(..))
import Ariadne.Wallet.Cardano.Kernel.DB.BlockMeta
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..), fromDb)
import Ariadne.Wallet.Cardano.Kernel.DB.Resolved
  (ResolvedBlock, ResolvedInput, ResolvedTx, rbSlot, rbTxs, rtxInputs,
  rtxOutputs)

import Ariadne.Wallet.Cardano.Kernel.Types (WalletId(..))

{-------------------------------------------------------------------------------
 Pre-filter Tx Inputs and Outputs; pre-filter a block of transactions.
+-------------------------------------------------------------------------------}

-- | Address extended with an HdAddressId, which embeds information that places
--   the Address in the context of the Wallet/Accounts/Addresses hierarchy.
type AddrWithId = (HdAddressId,Address)

-- | A mapping from (extended) addresses to their corresponding Utxo
type PrefilteredUtxo = Map AddrWithId Utxo

-- | A mapping from (extended) addresses to the inputs that were spent from them
type PrefilteredInputs = Map AddrWithId (Set TxIn)

-- | Prefiltered block
--
-- A prefiltered block is a block that contains only inputs and outputs from
-- the block that are relevant to the wallet.
data PrefilteredBlock = PrefilteredBlock {
      -- | Relevant inputs
      pfbPrefilteredInputs :: PrefilteredInputs

      -- | Relevant utxo
    , pfbPrefilteredUtxo   :: PrefilteredUtxo

      -- | Prefiltered block metadata
    , pfbMeta    :: BlockMeta
    }

deriveSafeCopySimple 1 'base ''PrefilteredBlock

pfbInputs :: PrefilteredBlock -> Set TxIn
pfbInputs PrefilteredBlock {..} = Set.unions $ Map.elems pfbPrefilteredInputs

pfbOutputs :: PrefilteredBlock -> Utxo
pfbOutputs PrefilteredBlock {..} = Map.unions $ Map.elems pfbPrefilteredUtxo

-- | Empty prefiltered block
--
-- An empty prefiltered block is what we get when we filter a block for a
-- particular account and there is nothing in the block that is of
-- relevance to that account
emptyPrefilteredBlock :: PrefilteredBlock
emptyPrefilteredBlock = PrefilteredBlock {
      pfbPrefilteredInputs = Map.empty
    , pfbPrefilteredUtxo   = Map.empty
    , pfbMeta    = mempty
    }

type WalletKey = (WalletId, HDPassphrase)

-- | Summary of an address as it appears in a transaction.
--   NOTE: Since an address can occur in multiple transactions, there could be
--   multiple valid summaries for an address.
data AddressSummary = AddressSummary {
      addrSummaryAddr        :: Address
    ,
      addrSummaryId          :: HdAddressId
    ,
      addrSummaryTxId        :: TxId
    }

-- | Extended Utxo with each output paired with an HdAddressId, required for
--   discovering new Addresses during prefiltering
type UtxoWithAddrId = Map TxIn (TxOutAux,HdAddressId)

-- | Extended Utxo where each output is paired with an AddressSummary. Provides
--   the required metadata for computing address meta data for BlockMeta.
type UtxoSummaryRaw = Map TxIn (TxOutAux,AddressSummary)

{-------------------------------------------------------------------------------
 Pre-filter Tx Inputs and Outputs to those that belong to the given Wallet.
+-------------------------------------------------------------------------------}

-- | Prefilter the inputs and outputs of a resolved transaction.
--   Prefiltered inputs and outputs are indexed by accountId.
--   The output Utxo is extended with address summary information
prefilterTx :: WalletKey
            -> ResolvedTx
            -> (Map HdAccountId PrefilteredInputs -- prefiltered inputs
              , Map HdAccountId UtxoSummaryRaw)   -- prefiltered output utxo, extended with address summary
prefilterTx wKey tx = (prefInps,prefOuts')
    where
        inps = toList (tx ^. rtxInputs  . fromDb)
        outs =         tx ^. rtxOutputs . fromDb

        prefInps = prefilterInputs wKey inps
        prefOuts = prefilterUtxo'  wKey outs

        prefOuts' = Map.map extendWithSummary prefOuts

-- | Prefilter inputs of a transaction
prefilterInputs :: WalletKey
          -> [(TxIn, ResolvedInput)]
          -> Map HdAccountId PrefilteredInputs
prefilterInputs wKey inps
    = prefilterResolvedTxPairs wKey mergeF inps
    where
        mergeF :: [((TxIn, TxOutAux), HdAddressId)] -> Map HdAccountId PrefilteredInputs
        mergeF = Map.fromListWith (Map.unionWith Set.union) . (map f)

        f ((txIn, txOut),addrId) =
            let accId = addrId ^. hdAddressIdParent
                addrWithId = (addrId, txOutAddress $ toaOut txOut)
            in (accId, Map.singleton addrWithId (Set.singleton txIn))

-- | Prefilter utxo using wallet key
prefilterUtxo' :: WalletKey -> Utxo -> Map HdAccountId UtxoWithAddrId
prefilterUtxo' wKey utxo
    = prefilterResolvedTxPairs wKey mergeF (Map.toList utxo)
    where
        mergeF = Map.fromListWith Map.union . (map f)

        f ((txIn, txOut),addrId) = (addrId ^. hdAddressIdParent,
                                    Map.singleton txIn (txOut, addrId))

-- | Prefilter utxo using walletId and esk
prefilterUtxo :: HdRootId -> EncryptedSecretKey -> Utxo -> Map HdAccountId PrefilteredUtxo
prefilterUtxo rootId esk utxo = map (toPrefilteredUtxo id) prefUtxo
    where
        prefUtxo = prefilterUtxo' wKey utxo
        wKey     = (WalletIdHdSeq rootId, eskToHDPassphrase esk)

-- | Group Utxo by address
-- Note: explicit forall is needed because without it it is impossible to write
-- a type signature for @key@ due to how ScopedTypeVariables is implemented.
toPrefilteredUtxo :: forall a . (a -> HdAddressId) -> Map TxIn (TxOutAux, a) -> PrefilteredUtxo
toPrefilteredUtxo toHdAddressId =
    Map.map Map.fromList . groupBy key value
  where
    key :: (TxIn, (TxOutAux, a)) -> AddrWithId
    key (_, (txOutAux, addrId)) = (toHdAddressId addrId, txOutAddress . toaOut $ txOutAux)
    value :: (TxIn, (TxOutAux, a)) -> (TxIn, TxOutAux)
    value (txIn, (txOutAux, _)) = (txIn, txOutAux)

    groupBy
        :: Ord k2
        => ((k1, v1) -> k2)
        -> ((k1, v1) -> v2)
        -> Map k1 v1
        -> Map k2 [v2]
    groupBy getKey getValue =
        Map.fromListWith (++) . map (getKey &&& (one . getValue)) . Map.toList

-- | Prefilter resolved transaction pairs.
--   Also returns a Boolean indicating whether @all@ pairs are "ours"
prefilterResolvedTxPairs :: WalletKey
                         -> ([((TxIn, TxOutAux), HdAddressId)] -> a)
                         -> [(TxIn, TxOutAux)]
                         -> a
prefilterResolvedTxPairs wKey mergeF pairs
    = mergeF prefTxPairs
    where
        selectAddr = txOutAddress . toaOut . snd

        prefTxPairs = prefilter wKey selectAddr pairs

-- TODO(@uroboros/ryan) `selectOwnAddresses` calls `decryptAddress`, which extracts
-- the AccountId from the Tx Attributes. This is not sufficient since it
-- doesn't actually _verify_ that the Tx belongs to the AccountId.
-- We need to add verification (see `deriveLvl2KeyPair`).
prefilter :: WalletKey
     -> (a -> Address)      -- ^ address getter
     -> [a]                 -- ^ list to filter
     -> [(a, HdAddressId)]  -- ^ matching items
prefilter (wid,hdPass) selectAddr rtxs
    = map f $ selectOwnAddresses hdPass selectAddr rtxs
    where f (addr,meta) = (addr, toAddressId wid meta)

          toAddressId :: WalletId -> WAddressMeta -> HdAddressId
          toAddressId (WalletIdHdSeq rootId) meta' = addressId
              where
                  accountIx = bip44AccountIndex $ _wamDerivationPath meta'
                  accountId = HdAccountId rootId accountIx

                  addressChain = bip44AddressChain $ _wamDerivationPath meta'

                  addressIx = bip44AddressIndex $ _wamDerivationPath meta'
                  addressId = HdAddressId accountId addressChain addressIx

extendWithSummary :: Map TxIn (TxOutAux,HdAddressId)
                  -- ^ Utxo extended with HdAddressId
                  -> Map TxIn (TxOutAux,AddressSummary)
                  -- ^ Utxo extended with AddressSummary
extendWithSummary utxoWithAddrId
    = Map.fromList $ mapMaybe toAddrSummary (Map.toList utxoWithAddrId)
    where
        toAddrSummary (txIn,(txOutAux,addressId))
            = case txIn of
                (TxInUtxo txId _) -> Just (txIn,(txOutAux,addrSummary txId))
                (TxInUnknown _ _) -> Nothing -- NOTE: we ignore addresses with 'unknown' inputs
            where
                addrSummary txId' = AddressSummary (txOutAddress . toaOut $ txOutAux)
                                                    addressId
                                                    txId'

{-------------------------------------------------------------------------------
 Pre-filter a block of transactions, adorn each prefiltered block with block metadata
+-------------------------------------------------------------------------------}

-- | Prefilter the transactions of a resolved block for the given wallet.
--
--   Returns prefiltered blocks indexed by HdAccountId.
prefilterBlock :: WalletId
               -> EncryptedSecretKey
               -> ResolvedBlock
               -> Map HdAccountId PrefilteredBlock
prefilterBlock wid esk block
    = Map.fromList $ map (mkPrefBlock slotId inpAll outAll) (Set.toList accountIds)
  where
    hdPass :: HDPassphrase
    hdPass = eskToHDPassphrase esk
    wKey = (wid, hdPass)

    inps :: [Map HdAccountId PrefilteredInputs]
    outs :: [Map HdAccountId UtxoSummaryRaw]
    (inps, outs) = unzip $ map (prefilterTx wKey) (block ^. rbTxs)

    inpAll :: Map HdAccountId PrefilteredInputs
    outAll :: Map HdAccountId UtxoSummaryRaw
    inpAll = Map.unionsWith (Map.unionWith Set.union) inps
    outAll = Map.unionsWith Map.union outs

    slotId = block ^. rbSlot . fromDb
    accountIds = Map.keysSet inpAll `Set.union` Map.keysSet outAll

mkPrefBlock :: SlotId
            -> Map HdAccountId PrefilteredInputs
            -> Map HdAccountId (Map TxIn (TxOutAux, AddressSummary))
            -> HdAccountId
            -> (HdAccountId, PrefilteredBlock)
mkPrefBlock slotId inps outs accId
    = (accId, PrefilteredBlock prefInps prefUtxo blockMeta')
    where
        prefInps           =                  Map.findWithDefault Map.empty accId inps
        (prefUtxo, addrs') = fromUtxoSummary (Map.findWithDefault Map.empty accId outs)

        blockMeta' = mkBlockMeta slotId addrs'

mkBlockMeta :: SlotId -> [AddressSummary] -> BlockMeta
mkBlockMeta slotId addrs_ = BlockMeta{..}
    where
        txIds' = nub $ map addrSummaryTxId addrs_

        indexedAddrs = indexByAddr addrs_

        _blockMetaSlotId      = InDb . Map.fromList . map (,slotId) $ txIds'
        _blockMetaAddressMeta = InDb $ Map.map (const mkAddressMeta) indexedAddrs

-- | This function is called once for each address found in a particular block of
--   transactions. The collection of address summaries passed to this function
--   corresponds to occurances of a given address in transactions in a block.
--   Since the collection was made by indexing the block of transactions by address,
--   we can be sure that the address occurs in at least one transaction and
--   hence that there are at least one or more summaries passed to this function
--   for a given address.
mkAddressMeta :: AddressMeta
mkAddressMeta
    = AddressMeta isUsed
    where
        -- An address is considered "used" if
        -- (1) it is "our" address: we are only dealing with prefiltered transactions
        --     here and can at this stage assume that the address is indeed "ours".
        -- (2) the transaction is confirmed: we are dealing here with transactions that
        --     appear in a block and can assume that they are confirmed.
        isUsed = True

-- | Index the list of address summaries by Address.
--   NOTE: Since there will be at least one AddressSummary per Address,
--   we can safely use NE.fromList.
indexByAddr :: [AddressSummary] -> Map Address (NE.NonEmpty AddressSummary)
indexByAddr addrs
    -- TODO @uroboros/ryan construct NE lists and use NE.concat (would need NE.concat)
    = Map.map NE.fromList (Map.fromListWith (++) addrs')
    where
        fromAddrSummary addrSummary = (addrSummaryAddr addrSummary, [addrSummary])
        addrs' = map fromAddrSummary addrs

fromUtxoSummary :: Map TxIn (TxOutAux,AddressSummary)
                -> (PrefilteredUtxo,[AddressSummary])
fromUtxoSummary summary = (toPrefilteredUtxo addrSummaryId summary, addrs)
    where
        addrs = map snd $ Map.elems summary

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

setBuilder :: (Buildable a, Foldable t) => t a -> Builder
setBuilder = listBuilder ("{" :: Builder) (", " :: Builder) ("}" :: Builder)

mapBuilderExplicit
    :: (k -> Builder)
    -> (v -> Builder)
    -> Map k v
    -> Builder
mapBuilderExplicit formatK formatV =
    setBuilder
    . map (\(k, v) -> (formatK k) <> ": " <> (formatV v))
    . Map.toList

prefilteredInputsF :: Format r (PrefilteredInputs -> r)
prefilteredInputsF = later $ mapBuilderExplicit pairBuilder setBuilder

prefilteredUtxoF :: Format r (PrefilteredUtxo -> r)
prefilteredUtxoF = later $ mapBuilderExplicit pairBuilder formatUtxo

instance Buildable PrefilteredBlock where
  build PrefilteredBlock{..} = bprint
    ( "PrefilteredBlock "
    % "{ inputs:  " % prefilteredInputsF
    % ", outputs: " % prefilteredUtxoF
    % "}"
    )
    pfbPrefilteredInputs
    pfbPrefilteredUtxo
