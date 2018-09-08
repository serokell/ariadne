-- | READ-only operations on the wallet-spec state
module Ariadne.Wallet.Cardano.Kernel.DB.Spec.Read (
    -- * Queries
    queryAccountTotalBalance
  , queryAccountUtxo
  , queryAccountAvailableUtxo
  , queryAccountAvailableBalance
  ) where

import qualified Data.Map.Strict as Map

import qualified Pos.Core as Core
import Pos.Core.Txp (TxOut(..), TxOutAux(..))
import Pos.Txp (Utxo)

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read as HD
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Spec
import Ariadne.Wallet.Cardano.Kernel.DB.Spec.Util

import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet (IxSet)
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet as IxSet

{-------------------------------------------------------------------------------
  An address is considered "ours" if it belongs to the set of "our" addresses.
  The following pure functions are given the set of "our" addresses to enable filtering.
-------------------------------------------------------------------------------}

-- | If an Address is in the given set, it will occur exactly once or not at all
ourAddr :: IxSet HdAddress -> Core.Address -> Bool
ourAddr addrs addr =
    1 == IxSet.size (IxSet.getEQ addr addrs)

-- | Determines whether the transaction output address is one of "ours"
ourTxOut :: IxSet HdAddress -> TxOutAux -> Bool
ourTxOut addrs tx
    = ourAddr addrs (txOutAddress . toaOut $ tx)

-- | Filters the given utxo by selecting only utxo outputs that are "ours"
ourUtxo :: IxSet HdAddress -> Utxo -> Utxo
ourUtxo addrs = Map.filter (ourTxOut addrs)

{-------------------------------------------------------------------------------
  Pure functions that support read-only operations on an AccCheckpoint, as
  defined in the Wallet Spec
-------------------------------------------------------------------------------}

accountUtxo :: AccCheckpoint -> Utxo
accountUtxo = view (accCheckpointUtxo . fromDb)

accountUtxoBalance :: AccCheckpoint -> Core.Coin
accountUtxoBalance = view (accCheckpointUtxoBalance . fromDb)

accountPendingTxs :: AccCheckpoint -> PendingTxs
accountPendingTxs = view (accCheckpointPending . pendingTransactions . fromDb)

-- | The Available UtxO is the cached utxo balance minus any (pending) spent utxo
accountAvailableUtxo :: AccCheckpoint -> Utxo
accountAvailableUtxo c =
    let pendingIns = txIns (accountPendingTxs c)
    in utxoRemoveInputs (accountUtxo c) pendingIns

-- | The Available Balance is the cached utxo balance minus any (pending) spent utxo
accountAvailableBalance :: AccCheckpoint -> Core.Coin
accountAvailableBalance c =
    fromMaybe subCoinErr balance'
    where
        subCoinErr = error "Coin arithmetic error: subCoin utxoBalance balanceDelta"

        pendingIns = txIns (accountPendingTxs c)
        spentUtxo  = utxoRestrictToInputs (accountUtxo c) pendingIns

        balance' = Core.subCoin (accountUtxoBalance c) (balance spentUtxo)

-- | Account Change refers to any pending outputs paid back into the
--   account (represented by the given checkpoint).
--
-- NOTE: computing 'change' requires filtering "our" addresses
accountChange :: (Utxo -> Utxo) -> AccCheckpoint -> Utxo
accountChange ours
    = ours . pendingUtxo . accountPendingTxs

-- | The Account Total Balance is the 'available' balance plus any 'change'
--
-- NOTE: computing 'total balance' requires filtering "our" addresses, which requires
--       the full set of addresses for this AccCheckpoint
accountTotalBalance :: IxSet HdAddress -> AccCheckpoint -> Core.Coin
accountTotalBalance addrs c
    = add' availableBalance changeBalance
    where
        add' = Core.unsafeAddCoin
        ourUtxo' = ourUtxo addrs

        availableBalance = accountAvailableBalance c
        changeBalance    = balance (accountChange ourUtxo' c)

{-------------------------------------------------------------------------------
  Public queries on an account, as defined in the Wallet Spec
-------------------------------------------------------------------------------}

queryAccountTotalBalance :: HdAccountId -> HD.HdQueryErr UnknownHdAccount Core.Coin
queryAccountTotalBalance accountId db
    = accountTotalBalance <$> ourAddrs <*> checkpoint
    where
        checkpoint = HD.readHdAccountCurrentCheckpoint accountId db
        ourAddrs   = HD.readAddressesByAccountId       accountId db

queryAccountUtxo :: HdAccountId -> HD.HdQueryErr UnknownHdAccount Utxo
queryAccountUtxo accountId db
    = accountUtxo <$> checkpoint
    where
        checkpoint = HD.readHdAccountCurrentCheckpoint accountId db

queryAccountAvailableUtxo :: HdAccountId -> HD.HdQueryErr UnknownHdAccount Utxo
queryAccountAvailableUtxo accountId db
    = accountAvailableUtxo <$> checkpoint
    where
        checkpoint = HD.readHdAccountCurrentCheckpoint accountId db

queryAccountAvailableBalance :: HdAccountId
                             -> HD.HdQueryErr UnknownHdAccount Core.Coin
queryAccountAvailableBalance accountId db
    = accountAvailableBalance <$> checkpoint
    where
        checkpoint = HD.readHdAccountCurrentCheckpoint accountId db
