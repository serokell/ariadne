-- | UPDATE operations on the wallet-spec state
module Ariadne.Wallet.Cardano.Kernel.DB.Spec.Update (
    -- * Errors
    NewPendingFailed(..)
    -- * Updates
  , UpdatableWalletState(newPending, applyBlock, switchToFork)
  ) where

import Universum

import Data.SafeCopy (base, deriveSafeCopySimple)

import qualified Pos.Core as Core
import Pos.Core.Chrono (OldestFirst(..))

import Ariadne.Wallet.Cardano.Kernel.DB.BlockMeta
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Resolved
import Ariadne.Wallet.Cardano.Kernel.DB.Spec
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Errors thrown by 'newPending'
data NewPendingFailed =
    -- | Some inputs are not in the wallet utxo
    NewPendingInputUnavailable (InDb (Core.TxIn))

deriveSafeCopySimple 1 'base ''NewPendingFailed

{-------------------------------------------------------------------------------
  Wallet spec mandated updates
-------------------------------------------------------------------------------}

class UpdatableWalletState state where
    -- | Insert new pending transaction into the specified wallet
    --
    -- NOTE: Transactions to be inserted must be fully constructed and signed; we do
    -- not offer input selection at this layer. Instead, callers must get a snapshot
    -- of the database, construct a transaction asynchronously, and then finally
    -- submit the transaction. It is of course possible that the state of the
    -- database has changed at this point, possibly making the generated transaction
    -- invalid; 'newPending' therefore returns whether or not the transaction could
    -- be inserted. If this fails, the process must be started again. This is
    -- important for a number of reasons:
    --
    -- * Input selection may be an expensive computation, and we don't want to
    --   lock the database while input selection is ongoing.
    -- * Transactions may be signed off-site (on a different machine or on a
    --   a specialized hardware device).
    -- * We do not actually have access to the key storage inside the DB layer
    --   (and do not store private keys) so we cannot actually sign transactions.
    newPending
        :: InDb (Core.TxAux)
        -> Update' state NewPendingFailed ()

    -- | Apply a block to /all/ wallets' UTxO.
    --
    -- Ideally the block is prefiltered (see 'prefilter').
    applyBlock :: (ResolvedBlock, BlockMeta) -> state -> state

    -- | Rollback
    --
    -- This is an internal function only, and not exported. See 'switchToFork'.
    rollback :: state -> state

    -- | Switch to a fork
    switchToFork
        :: Int  -- ^ Number of blocks to rollback
        -> OldestFirst [] (ResolvedBlock, BlockMeta)  -- ^ Blocks to apply
        -> state -> state
    switchToFork = \n bs -> applyBlocks (getOldestFirst bs) . rollbacks n
      where
        applyBlocks :: [(ResolvedBlock, BlockMeta)] -> state -> state
        applyBlocks []     = identity
        applyBlocks (b:bs) = applyBlocks bs . applyBlock b

        rollbacks :: Int -> state -> state
        rollbacks 0 = identity
        rollbacks n = rollbacks (n - 1) . rollback

instance UpdatableWalletState AccCheckpoints where
    newPending _tx = error "AccCheckpoints: newPending"
    applyBlock (_resolvedBlock, _blockMeta) = error "AccCheckpoints: applyBlock"
    rollback = error "AccCheckpoints: rollback"

instance UpdatableWalletState AddrCheckpoints where
    newPending _tx = error "AddrCheckpoints: newPending"
    applyBlock (_resolvedBlock, _blockMeta) = error "AddrCheckpoints: applyBlock"
    rollback = error "AddrCheckpoints: rollback"
