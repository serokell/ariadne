-- | UPDATE operations on the wallet-spec state
module Ariadne.Wallet.Cardano.Kernel.DB.Spec.Update (
    -- * Errors
  NewPendingFailed(..)
    -- * Updates
  , newPending
  , applyBlock
  , switchToFork
  ) where

import Universum

import Data.SafeCopy (base, deriveSafeCopySimple)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Pos.Core as Core
import Pos.Core.Chrono (OldestFirst(..))
import Pos.Crypto (hash)
import Pos.Txp (Utxo)

import Ariadne.Wallet.Cardano.Kernel.PrefilterTx (PrefilteredBlock(..))

import Ariadne.Wallet.Cardano.Kernel.DB.BlockMeta
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Spec
import Ariadne.Wallet.Cardano.Kernel.DB.Spec.Util
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Errors thrown by 'newPending'
data NewPendingFailed =
    -- | Some inputs are not in the wallet utxo
    NewPendingInputsUnavailable (Set (InDb Core.TxIn))

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
    newPending :: InDb Core.TxAux -> Update' state NewPendingFailed ()

    -- | Apply the prefiltered block to the specified wallet
    applyBlock :: (PrefilteredBlock, BlockMeta) -> state -> state

    -- | Rollback
    --
    -- This is an internal function only, and not exported. See 'switchToFork'.
    rollback :: state -> state

    -- | Switch to a fork
    switchToFork
        :: Int  -- ^ Number of blocks to rollback
        -> OldestFirst [] (PrefilteredBlock, BlockMeta)  -- ^ Blocks to apply
        -> state -> state
    switchToFork = \n bs -> applyBlocks (getOldestFirst bs) . rollbacks n
      where
        applyBlocks :: [(PrefilteredBlock, BlockMeta)] -> state -> state
        applyBlocks []     = identity
        applyBlocks (b:bs) = applyBlocks bs . applyBlock b

        rollbacks :: Int -> state -> state
        rollbacks 0 = identity
        rollbacks n = rollbacks (n - 1) . rollback

-- | Update (utxo,balance) with the given prefiltered block
updateUtxo :: PrefilteredBlock -> (Utxo, Core.Coin) -> (Utxo, Core.Coin)
updateUtxo PrefilteredBlock{..} (currentAccUtxo', currentBalance')
    = (utxo', balance')
    where
        unionUtxo            = Map.union pfbOutputs currentAccUtxo'
        utxo'                = utxoRemoveInputs unionUtxo pfbInputs

        unionUtxoRestricted  = utxoRestrictToInputs unionUtxo pfbInputs
        balanceDelta         = balanceI pfbOutputs - balanceI unionUtxoRestricted
        currentBalanceI      = Core.coinToInteger currentBalance'
        balance'             = Core.unsafeIntegerToCoin $ currentBalanceI + balanceDelta

-- | Update the pending transactions with the given prefiltered block
updatePending :: PrefilteredBlock -> PendingTxs -> PendingTxs
updatePending PrefilteredBlock{..} =
    Map.filter (\t -> disjoint (txAuxInputSet t) pfbInputs)

instance UpdatableWalletState AccCheckpoints where
    newPending tx = do
        checkpoints <- get
        let available' = available (checkpoints ^. currentAccUtxo) (checkpoints ^. currentAccPendingTxs)
        if isValidPendingTx tx' available'
            then
                put (insertPending checkpoints)
            else
                inputUnavailableErr available'

        where
            tx' = tx ^. fromDb

            insertPending :: AccCheckpoints -> AccCheckpoints
            insertPending cs = cs & currentAccPendingTxs %~ Map.insert txId tx'
                where txId = hash $ Core.taTx tx'

            inputUnavailableErr available_ = do
                let unavailableInputs = txAuxInputSet tx' `Set.difference` utxoInputs available_
                throwError $ NewPendingInputsUnavailable (Set.map InDb unavailableInputs)

    applyBlock (prefBlock, _bMeta) checkpoints
        = AccCheckpoint {
              _accCheckpointUtxo           = InDb utxo''
            , _accCheckpointUtxoBalance    = InDb balance''
            , _accCheckpointPending        = Pending . InDb $ pending''
            , _accCheckpointExpected       = InDb expected''
            , _accCheckpointBlockMeta      = blockMeta''
            } NE.<| checkpoints
        where
            utxo'        = checkpoints ^. currentAccUtxo
            utxoBalance' = checkpoints ^. currentAccUtxoBalance
            pending'     = checkpoints ^. currentAccPendingTxs

            (utxo'', balance'') = updateUtxo prefBlock (utxo', utxoBalance')
            pending''           = updatePending prefBlock pending'
            -- TODO(@uroboros/ryan) applyBlock.updateExpected/updateBlockMeta
            -- (as part of CBR-150 Extend pure data layer to support rollback)
            expected''          = checkpoints ^. currentAccExpected
            blockMeta''         = checkpoints ^. currentAccBlockMeta

    rollback = error "AccCheckpoints: rollback"

instance UpdatableWalletState AddrCheckpoints where
    newPending _tx = pass
    applyBlock (_prefBlock, _bMeta) _checkpoints = error "AddrCheckpoints: applyBlock"
    rollback = error "AddrCheckpoints: rollback"
