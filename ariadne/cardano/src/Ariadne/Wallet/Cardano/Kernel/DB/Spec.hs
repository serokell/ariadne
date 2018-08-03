{-# LANGUAGE RankNTypes #-}
-- | Wallet state as mandated by the wallet specification
module Ariadne.Wallet.Cardano.Kernel.DB.Spec (
    -- * Wallet state as mandated by the spec
    Pending(..)
  , PendingTxs
  , Balance
  , AccCheckpoint(..)
  , AccCheckpoints
  , AddrCheckpoint(..)
  , AddrCheckpoints
  , emptyPending
  , singletonPending
  , unionPending
  , removePending
  , emptyAccCheckpoint
  , emptyAddrCheckpoint
    -- ** Lenses
  , pendingTransactions
  , accCheckpointUtxo
  , accCheckpointUtxoBalance
  , accCheckpointPending
  , accCheckpointBlockMeta
  , accCheckpointAddressMeta
  , addrCheckpointUtxo
  , addrCheckpointUtxoBalance
    -- ** Lenses into the current checkpoint
  , currentAccCheckpoint
  , currentAccUtxo
  , currentAccUtxoBalance
  , currentAccPending
  , currentAccPendingTxs
  , currentAccBlockMeta
  , currentAddrCheckpoint
  , currentAddrUtxo
  , currentAddrUtxoBalance
  ) where

import Universum hiding (elems)

import Control.Lens (at, lens, non, to, (?~))
import Control.Lens.TH (makeLenses)
import qualified Data.Map.Strict as M
import Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text.Buildable
import Formatting (bprint, build, (%))
import Serokell.Util.Text (listJsonIndent, mapJson)

import qualified Pos.Core as Core
import qualified Pos.Core.Txp as Txp
import qualified Pos.Txp as Core (Utxo)

import Ariadne.Wallet.Cardano.Kernel.DB.BlockMeta
import Ariadne.Wallet.Cardano.Kernel.DB.InDb

-- This file belongs under Kernel/DB/Spec/State.hs or something similar.
-- However, we don't move it because we want to minimize diff with IOHK.
-- The downside is that Kernel.DB.Spec.Update imports Kernel.DB.Spec.

{-------------------------------------------------------------------------------
  Wallet state as mandated by the spec
-------------------------------------------------------------------------------}

type Balance = Integer

type PendingTxs = Map Txp.TxId Txp.TxAux

-- | Pending transactions
data Pending = Pending {
      _pendingTransactions :: InDb PendingTxs
     } deriving Eq


-- | Returns a new, empty 'Pending' set.
emptyPending :: Pending
emptyPending = Pending . InDb $ mempty

-- | Returns a new, empty 'Pending' set.
singletonPending :: Txp.TxId -> Txp.TxAux -> Pending
singletonPending txId txAux = Pending . InDb $ M.singleton txId txAux

-- | Computes the union between two 'Pending' sets.
unionPending :: Pending -> Pending -> Pending
unionPending (Pending new) (Pending old) =
    Pending (M.union <$> new <*> old)

-- | Computes the difference between two 'Pending' sets.
removePending :: Set Txp.TxId -> Pending -> Pending
removePending ids (Pending (InDb old)) = Pending (InDb $ old `withoutKeys` ids)
    where
        withoutKeys :: Ord k => Map k a -> Set k -> Map k a
        m `withoutKeys` s = m `M.difference` M.fromSet (const ()) s

-- | Per-account state
--
-- NOTE: At the moment this does not included the expected UTxO. The expected
-- UTxO is used for two things:
--
-- * Block resolution (translating tx inputs to their corresponding outputs, so
--   that we know the corresponding addresses, needed for prefilering)
-- * Minimum balance computation
--
-- Fortunately however we can rely on a full node as backing, so we don't need
-- to use the expected UTxO for block resolution (this is explained in the
-- formal spec in section "Prefiltering -- Consequences", under "possible
-- alternatives"), and minimum balance computation is a new feature that we
-- haven't implemented yet.
data AccCheckpoint = AccCheckpoint {
      -- Invariant: this must match the union of utxos of all
      -- addresses in this account.
      _accCheckpointUtxo        :: !(InDb Core.Utxo)
      -- Invariant: this must match the sum of balances of all
      -- addresses in this account.
    , _accCheckpointUtxoBalance :: !(InDb Core.Coin)
    , _accCheckpointPending     :: !Pending
    , _accCheckpointBlockMeta   :: !BlockMeta
    }

emptyAccCheckpoint :: AccCheckpoint
emptyAccCheckpoint = AccCheckpoint {
    _accCheckpointUtxo = InDb M.empty
  , _accCheckpointUtxoBalance = InDb $ Core.Coin 0
  , _accCheckpointPending = emptyPending
  , _accCheckpointBlockMeta = mempty
  }

-- | List of account checkpoints
type AccCheckpoints = NonEmpty AccCheckpoint

-- | Per-address state
data AddrCheckpoint = AddrCheckpoint {
      -- Address checkpoints contain only utxo and utxo balance.
      -- The "pending" set is meaningless for them. There are
      -- multiple reasons for that:
      -- 1. The spec stipulates having a single "pending" set per
      --    wallet, and does not specify anything about its structure.
      --    IOHK have opted to split it into smaller per-account
      --    sets (which implies that the wallet is going to support
      --    selecting transaction inputs only within a single account).
      --    Those smaller sets already cover what is required by the
      --    spec; further subdivision is unnecessary.
      -- 2. There is no meaningful notion of a "pending" set for
      --    an address since many transactions are going to have
      --    inputs from more than one address.
      _addrCheckpointUtxo        :: !(InDb Core.Utxo)
    , _addrCheckpointUtxoBalance :: !(InDb Core.Coin)
    }

emptyAddrCheckpoint :: AddrCheckpoint
emptyAddrCheckpoint = AddrCheckpoint {
    _addrCheckpointUtxo = InDb M.empty
  , _addrCheckpointUtxoBalance = InDb $ Core.Coin 0
  }

-- | List of address checkpoints
type AddrCheckpoints = NonEmpty AddrCheckpoint

makeLenses ''Pending
makeLenses ''AccCheckpoint
makeLenses ''AddrCheckpoint

deriveSafeCopySimple 1 'base ''Pending
deriveSafeCopySimple 1 'base ''AccCheckpoint
deriveSafeCopySimple 1 'base ''AddrCheckpoint

accCheckpointAddressMeta :: Core.Address -> Lens' AccCheckpoint AddressMeta
accCheckpointAddressMeta addr =
    let getMeta = accCheckpointBlockMeta . blockMetaAddressMeta . fromDb
    in lens (\c      -> c ^. getMeta . at addr . non mempty)
            (\c meta -> over ( accCheckpointBlockMeta
                             . blockMetaAddressMeta
                             . fromDb) (at addr ?~ meta) c)

{-------------------------------------------------------------------------------
  Lenses for accessing current checkpoint
-------------------------------------------------------------------------------}

currentAccCheckpoint :: Lens' AccCheckpoints AccCheckpoint
currentAccCheckpoint = neHead

currentAccUtxo        :: Lens' AccCheckpoints Core.Utxo
currentAccUtxoBalance :: Lens' AccCheckpoints Core.Coin
currentAccPending     :: Lens' AccCheckpoints Pending
currentAccBlockMeta   :: Lens' AccCheckpoints BlockMeta
currentAccPendingTxs  :: Lens' AccCheckpoints PendingTxs

currentAccUtxo        = currentAccCheckpoint . accCheckpointUtxo        . fromDb
currentAccUtxoBalance = currentAccCheckpoint . accCheckpointUtxoBalance . fromDb
currentAccBlockMeta   = currentAccCheckpoint . accCheckpointBlockMeta
currentAccPending     = currentAccCheckpoint . accCheckpointPending
currentAccPendingTxs  = currentAccPending . pendingTransactions . fromDb

currentAddrCheckpoint :: Lens' AddrCheckpoints AddrCheckpoint
currentAddrCheckpoint = neHead

currentAddrUtxo        :: Lens' AddrCheckpoints Core.Utxo
currentAddrUtxoBalance :: Lens' AddrCheckpoints Core.Coin

currentAddrUtxo        = currentAddrCheckpoint . addrCheckpointUtxo        . fromDb
currentAddrUtxoBalance = currentAddrCheckpoint . addrCheckpointUtxoBalance . fromDb

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

neHead :: Lens' (NonEmpty a) a
neHead f (x :| xs) = (:| xs) <$> f x

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable Pending where
    build (Pending p) =
      let elems = p ^. fromDb . to M.toList
      in bprint ("Pending " % listJsonIndent 4) (map fst elems)

instance Buildable AccCheckpoint where
    build AccCheckpoint{..} = bprint
        ( "AccCheckpoint"
        % "{ utxo:        " % mapJson
        % ", utxoBalance: " % build
        % ", pending:     " % build
        % ", blockMeta:   " % build
        % "}"
        )
      (_fromDb _accCheckpointUtxo)
      (_fromDb _accCheckpointUtxoBalance)
      _accCheckpointPending
      _accCheckpointBlockMeta
