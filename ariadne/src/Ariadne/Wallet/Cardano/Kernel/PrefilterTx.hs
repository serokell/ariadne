{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeFamilies #-}

module Ariadne.Wallet.Cardano.Kernel.PrefilterTx
       ( PrefilteredBlock(..)
       , PrefilteredUtxo
       , AddrWithId
       , prefilterBlock
       , prefilterUtxo
       ) where

import Universum

import qualified Data.Map as Map
import Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Set as Set
import qualified Data.Text.Buildable
import Formatting (bprint, (%))
import Serokell.Util (listJson, mapJson)

import Ariadne.Wallet.Cardano.Kernel.Decrypt
  (HDPassphrase, WAddressMeta(..), eskToHDPassphrase, selectOwnAddresses)

import Pos.Core (Address(..))
import Pos.Core.Txp (TxIn(..), TxOut(..), TxOutAux(..))
import Pos.Crypto (EncryptedSecretKey)
import Pos.Txp.Toil.Types (Utxo)

import Ariadne.Wallet.Cardano.Kernel.Bip44 (Bip44DerivationPath(..))
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (fromDb)
import Ariadne.Wallet.Cardano.Kernel.DB.Resolved
  (ResolvedBlock, ResolvedInput, ResolvedTx, rbTxs, rtxInputs, rtxOutputs)
import Ariadne.Wallet.Cardano.Kernel.Types (WalletId(..))

{-------------------------------------------------------------------------------
 Pre-filter Tx Inputs and Outputs to those that belong to the given Wallet.
+-------------------------------------------------------------------------------}

-- | Extended Utxo with each output paired with an HdAddressId, required for
--   discovering new Addresses during prefiltering
type UtxoWithAddrId = Map TxIn (TxOutAux,HdAddressId)

-- | Address extended with an HdAddressId, which embeds information that places
--   the Address in the context of the Wallet/Accounts/Addresses hierarchy.
type AddrWithId = (HdAddressId,Address)

-- | A mapping from (extended) addresses to their corresponding Utxo
type PrefilteredUtxo = Map AddrWithId Utxo

-- | Prefiltered block
--
-- A prefiltered block is a block that contains only inputs and outputs from
-- the block that are relevant to the wallet.
data PrefilteredBlock = PrefilteredBlock {
      -- | Relevant inputs
      pfbInputs  :: Set TxIn

      -- | Relevant outputs
    , pfbOutputs :: Utxo

      -- | all output addresses present in the Utxo
    , pfbAddrs   :: [AddrWithId]
    }

deriveSafeCopySimple 1 'base ''PrefilteredBlock

type WalletKey = (WalletId, HDPassphrase)

toPrefilteredUtxo :: UtxoWithAddrId -> PrefilteredUtxo
toPrefilteredUtxo utxoWithAddrs =
    Map.map Map.fromList $ groupBy key value utxoWithAddrs
  where
    key :: (TxIn, (TxOutAux, HdAddressId)) -> AddrWithId
    key (_, (txOutAux, addrId)) = (addrId, txOutAddress . toaOut $ txOutAux)
    value :: (TxIn, (TxOutAux, HdAddressId)) -> (TxIn, TxOutAux)
    value (txIn, (txOutAux, _)) = (txIn, txOutAux)

    groupBy
        :: (Ord k2)
        => ((k1, v1) -> k2)
        -> ((k1, v1) -> v2)
        -> Map k1 v1
        -> Map k2 [v2]
    groupBy getKey getValue =
      Map.fromListWith (++) . map (getKey &&& (one . getValue)) . Map.toList

-- | Prefilter the transactions of a resolved block for the given wallet.
--
--   Returns prefiltered blocks indexed by HdAccountId.
prefilterBlock :: WalletId
               -> EncryptedSecretKey
               -> ResolvedBlock
               -> Map HdAccountId PrefilteredBlock
prefilterBlock wid esk block
    = Map.fromList $ map mkPrefBlock (Set.toList accountIds)
  where
    mkPrefBlock accId'
        = (accId', PrefilteredBlock inps' outs' addrs')
        where
            byAccountId accId'' def dict = fromMaybe def $ Map.lookup accId'' dict

            inps'    =                    byAccountId accId' Set.empty inpAll
            prefUtxo = toPrefilteredUtxo (byAccountId accId' Map.empty outAll)
            addrs'   = Map.keys prefUtxo
            outs'    = Map.unions $ Map.elems prefUtxo

    hdPass :: HDPassphrase
    hdPass = eskToHDPassphrase esk
    wKey = (wid, hdPass)

    inps :: [Map HdAccountId (Set TxIn)]
    outs :: [Map HdAccountId UtxoWithAddrId]
    (inps, outs) = unzip $ map (prefilterTx wKey) (block ^. rbTxs)

    inpAll :: Map HdAccountId (Set TxIn)
    outAll :: Map HdAccountId UtxoWithAddrId
    inpAll = Map.unionsWith Set.union inps
    outAll = Map.unionsWith Map.union outs

    accountIds = Map.keysSet inpAll `Set.union` Map.keysSet outAll

-- | Prefilter the inputs and outputs of a resolved transaction
prefilterTx :: WalletKey
            -> ResolvedTx
            -> (Map HdAccountId (Set TxIn), Map HdAccountId UtxoWithAddrId)
prefilterTx wKey tx = (
      prefilterInputs wKey (toList (tx ^. rtxInputs . fromDb))
    , prefilterUtxo'  wKey (tx ^. rtxOutputs . fromDb)
    )

-- | Prefilter inputs of a transaction
prefilterInputs :: WalletKey
          -> [(TxIn, ResolvedInput)]
          -> Map HdAccountId (Set TxIn)
prefilterInputs wKey inps
    = Map.fromListWith Set.union
      $ map f
      $ prefilterResolvedTxPairs wKey inps
    where
        f (addressId, (txIn, _txOut)) = (addressId ^. hdAddressIdParent, Set.singleton txIn)

-- | Prefilter utxo using wallet key
prefilterUtxo' :: WalletKey -> Utxo -> Map HdAccountId UtxoWithAddrId
prefilterUtxo' wid utxo
    = Map.fromListWith Map.union
      $ map f
      $ prefilterResolvedTxPairs wid (Map.toList utxo)
    where
        f (addressId, (txIn, txOut)) = (addressId ^. hdAddressIdParent,
                                        Map.singleton txIn (txOut, addressId))

-- | Prefilter utxo using walletId and esk
prefilterUtxo :: HdRootId -> EncryptedSecretKey -> Utxo -> Map HdAccountId PrefilteredUtxo
prefilterUtxo rootId esk utxo = map toPrefilteredUtxo (prefilterUtxo' wKey utxo)
    where
        wKey = (WalletIdHdRnd rootId, eskToHDPassphrase esk)

-- | Prefilter resolved transaction pairs
prefilterResolvedTxPairs :: WalletKey
                         -> [(TxIn, TxOutAux)]
                         -> [(HdAddressId, (TxIn, TxOutAux))]
prefilterResolvedTxPairs wid xs = map f $ prefilter wid selectAddr xs
    where
        f ((txIn, txOut), addressId) = (addressId, (txIn, txOut))
        selectAddr = txOutAddress . toaOut . snd

-- | Filter items for addresses that were derived from the given WalletKey.
--   Returns the matching HdAddressId, which embeds the parent HdAccountId
--   discovered for the matching item.
--
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
          toAddressId (WalletIdHdRnd rootId) meta' = addressId
              where
                  accountIx = bip44AccountIndex $ _wamDerivationPath meta'
                  accountId = HdAccountId rootId accountIx

                  addressChain = bip44AddressChain $ _wamDerivationPath meta'

                  addressIx = bip44AddressIndex $ _wamDerivationPath meta'
                  addressId = HdAddressId accountId addressChain addressIx

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable PrefilteredBlock where
  build PrefilteredBlock{..} = bprint
    ( "PrefilteredBlock "
    % "{ inputs:  " % listJson
    % ", outputs: " % mapJson
    % "}"
    )
    (Set.toList pfbInputs)
    pfbOutputs
