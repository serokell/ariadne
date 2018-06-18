-- | Wallet state as mandated by the wallet specification
module Ariadne.Wallet.Cardano.Kernel.DB.Spec (
    -- * Wallet state as mandated by the spec
    Pending(..)
  , AccCheckpoint(..)
  , AccCheckpoints
  , AddrCheckpoint(..)
  , AddrCheckpoints
  , emptyPending
  , singletonPending
  , unionPending
  , removePending
    -- ** Lenses
  , pendingTransactions
  , accCheckpointUtxo
  , accCheckpointUtxoBalance
  , accCheckpointExpected
  , accCheckpointPending
  , accCheckpointBlockMeta
  , addrCheckpointUtxo
  , addrCheckpointUtxoBalance
    -- ** Lenses into the current checkpoint
  , currentAccCheckpoint
  , currentAccUtxo
  , currentAccUtxoBalance
  , currentAccExpected
  , currentAccPending
  , currentAccBlockMeta
  , currentAddrCheckpoint
  , currentAddrUtxoBalance
  ) where

import Universum

import Control.Lens (to)
import Control.Lens.TH (makeLenses)
import qualified Data.Map.Strict as M
import Data.SafeCopy (base, deriveSafeCopySimple)
import Data.Text.Buildable (build)
import Formatting (bprint, (%))
import Serokell.Util.Text (listJsonIndent)

import qualified Pos.Core as Core
import qualified Pos.Txp as Core

import Ariadne.Wallet.Cardano.Kernel.DB.BlockMeta
import Ariadne.Wallet.Cardano.Kernel.DB.InDb

-- This file belongs under Kernel/DB/Spec/State.hs or something similar.
-- However, we don't move it because we want to minimize diff with IOHK.
-- The downside is that Kernel.DB.Spec.Update imports Kernel.DB.Spec.

{-------------------------------------------------------------------------------
  Wallet state as mandated by the spec
-------------------------------------------------------------------------------}

-- | Pending transactions
data Pending = Pending {
      _pendingTransactions :: !(InDb (Map Core.TxId Core.TxAux))
     } deriving Eq


-- | Returns a new, empty 'Pending' set.
emptyPending :: Pending
emptyPending = Pending . InDb $ mempty

-- | Returns a new, empty 'Pending' set.
singletonPending :: Core.TxId -> Core.TxAux -> Pending
singletonPending txId txAux = Pending . InDb $ M.singleton txId txAux

-- | Computes the union between two 'Pending' sets.
unionPending :: Pending -> Pending -> Pending
unionPending (Pending new) (Pending old) =
    Pending (M.union <$> new <*> old)

-- | Computes the difference between two 'Pending' sets.
removePending :: Set Core.TxId -> Pending -> Pending
removePending ids (Pending (InDb old)) = Pending (InDb $ old `withoutKeys` ids)
    where
        withoutKeys :: Ord k => Map k a -> Set k -> Map k a
        m `withoutKeys` s = m `M.difference` M.fromSet (const ()) s

-- | Per-account state
data AccCheckpoint = AccCheckpoint {
      -- Invariant: this must match the union of utxos of all
      -- addresses in this account.
      _accCheckpointUtxo        :: !(InDb Core.Utxo)
      -- Invariant: this must match the sum of balances of all
      -- addresses in this account.
    , _accCheckpointUtxoBalance :: !(InDb Core.Coin)
    , _accCheckpointExpected    :: !(InDb Core.Utxo)
    , _accCheckpointPending     :: !Pending
    , _accCheckpointBlockMeta   :: !BlockMeta
    }

-- | List of account checkpoints
type AccCheckpoints = NonEmpty AccCheckpoint

-- | Per-address state
data AddrCheckpoint = AddrCheckpoint {
      _addrCheckpointUtxo        :: !(InDb Core.Utxo)
    , _addrCheckpointUtxoBalance :: !(InDb Core.Coin)
    }

-- | List of address checkpoints
type AddrCheckpoints = NonEmpty AddrCheckpoint

makeLenses ''Pending
makeLenses ''AccCheckpoint
makeLenses ''AddrCheckpoint

deriveSafeCopySimple 1 'base ''Pending
deriveSafeCopySimple 1 'base ''AccCheckpoint
deriveSafeCopySimple 1 'base ''AddrCheckpoint

{-------------------------------------------------------------------------------
  Lenses for accessing current checkpoint
-------------------------------------------------------------------------------}

currentAccCheckpoint :: Lens' AccCheckpoints AccCheckpoint
currentAccCheckpoint = neHead

currentAccUtxo        :: Lens' AccCheckpoints Core.Utxo
currentAccUtxoBalance :: Lens' AccCheckpoints Core.Coin
currentAccExpected    :: Lens' AccCheckpoints Core.Utxo
currentAccPending     :: Lens' AccCheckpoints Pending
currentAccBlockMeta   :: Lens' AccCheckpoints BlockMeta

currentAccUtxo        = currentAccCheckpoint . accCheckpointUtxo        . fromDb
currentAccUtxoBalance = currentAccCheckpoint . accCheckpointUtxoBalance . fromDb
currentAccExpected    = currentAccCheckpoint . accCheckpointExpected    . fromDb
currentAccPending     = currentAccCheckpoint . accCheckpointPending
currentAccBlockMeta   = currentAccCheckpoint . accCheckpointBlockMeta

currentAddrCheckpoint :: Lens' AddrCheckpoints AddrCheckpoint
currentAddrCheckpoint = neHead

currentAddrUtxoBalance :: Lens' AddrCheckpoints Core.Coin
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
      let elements = p ^. fromDb . to M.toList
      in bprint ("Pending " % listJsonIndent 4) (map fst elements)
