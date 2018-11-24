-- | UPDATE operations on the wallet-spec state
module Ariadne.Wallet.Cardano.Kernel.DB.Spec.Update
       ( -- * Errors
         NewPendingFailed(..)
         -- * Updates
       , newPending
       , cancelPending
       , applyBlock
       , switchToFork
         -- * Testing
       , observableRollbackUseInTestsOnly
       ) where

import Data.SafeCopy (base, deriveSafeCopySimple)

import Test.QuickCheck (Arbitrary(..))

import qualified Data.Text.Buildable
import Formatting (bprint, (%))
import Serokell.Util (listJsonIndent)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Lens (each)
import qualified Pos.Core as Core
import Pos.Core.Chrono (OldestFirst(..))
import qualified Pos.Core.Txp as Txp
import Pos.Crypto (hash)
import Pos.Txp (Utxo)

import Ariadne.Wallet.Cardano.Kernel.PrefilterTx
  (PrefilteredBlock(..), pfbInputs, pfbOutputs)

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
    NewPendingInputsUnavailable (Set (InDb Txp.TxIn))
    deriving Show

deriveSafeCopySimple 1 'base ''NewPendingFailed

instance Buildable NewPendingFailed where
    build (NewPendingInputsUnavailable inputs) =
        let curatedInputs = map (view fromDb) (Set.toList inputs)
        in bprint ("NewPendingInputsUnavailable { inputs = " % listJsonIndent 4 % " }") curatedInputs

-- NOTE(adn) Short-circuiting the rabbit-hole with this instance by generating
-- an empty set, thus avoiding the extra dependency on @cardano-sl-core-test@.
instance Arbitrary NewPendingFailed where
    arbitrary = pure . NewPendingInputsUnavailable $ mempty

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
    newPending :: InDb Txp.TxAux -> Update' state NewPendingFailed ()

    -- | Cancel the input set of cancelled transactions from @all@ the 'Checkpoints'
    -- of an 'Account'.
    cancelPending :: Set Txp.TxId -> state -> state

    -- | Apply the prefiltered block to the specified wallet
    applyBlock :: PrefilteredBlock -> state -> state

    -- | Rollback
    --
    -- For the base case, see section "Rollback -- Omitting checkpoints" in the
    -- formal specification.
    --
    -- This is an internal function only, and not exported. See 'switchToFork'.
    rollback :: state -> state

    -- | Observable rollback, used in testing only
    --
    -- See 'switchToFork' for production use.
    observableRollbackUseInTestsOnly :: state -> state
    observableRollbackUseInTestsOnly = rollback

    -- | Switch to a fork
    switchToFork
        :: Int  -- ^ Number of blocks to rollback
        -> OldestFirst [] PrefilteredBlock  -- ^ Blocks to apply
        -> state -> state
    switchToFork = \n bs -> applyBlocks (getOldestFirst bs) . rollbacks n
      where
        applyBlocks :: [PrefilteredBlock] -> state -> state
        applyBlocks []     = identity
        applyBlocks (b:bs) = applyBlocks bs . applyBlock b

        rollbacks :: Int -> state -> state
        rollbacks 0 = identity
        rollbacks n = rollbacks (n - 1) . rollback

updateBlockMeta :: PrefilteredBlock -> BlockMeta -> BlockMeta
updateBlockMeta PrefilteredBlock{..} meta
    = meta `mappend` pfbMeta

-- | Update (utxo,balance) with the given prefiltered block
updateUtxo :: PrefilteredBlock -> (Utxo, Core.Coin) -> (Utxo, Core.Coin)
updateUtxo pfb (currentUtxo', currentBalance')
    = (utxo', balance')
    where
        inputs               = pfbInputs pfb
        outputs              = pfbOutputs pfb
        unionUtxo            = Map.union outputs currentUtxo'
        utxo'                = utxoRemoveInputs unionUtxo inputs

        unionUtxoRestricted  = utxoRestrictToInputs unionUtxo inputs
        balanceDelta         = balanceI outputs - balanceI unionUtxoRestricted
        currentBalanceI      = Core.coinToInteger currentBalance'
        balance'             = Core.unsafeIntegerToCoin $ currentBalanceI + balanceDelta

-- | Update the pending transactions with the given prefiltered block
updatePending :: PrefilteredBlock -> PendingTxs -> PendingTxs
updatePending pfb =
    Map.filter (\t -> disjoint (txAuxInputSet t) (pfbInputs pfb))

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
                where txId = hash $ Txp.taTx tx'

            inputUnavailableErr available_ = do
                let unavailableInputs = txAuxInputSet tx' `Set.difference` utxoInputs available_
                throwError $ NewPendingInputsUnavailable (Set.map InDb unavailableInputs)

    cancelPending txids checkpoints =
        checkpoints & over each
                    (\ckpoint ->
                        ckpoint & over accCheckpointPending
                                (removePending txids)
                    )

    applyBlock prefBlock checkpoints
        = AccCheckpoint {
              _accCheckpointUtxo           = InDb utxo''
            , _accCheckpointUtxoBalance    = InDb balance''
            , _accCheckpointPending        = Pending . InDb $ pending''
            , _accCheckpointBlockMeta      = blockMeta''
            } NE.<| checkpoints
        where
            utxo'        = checkpoints ^. currentAccUtxo
            utxoBalance' = checkpoints ^. currentAccUtxoBalance

            (utxo'', balance'') = updateUtxo      prefBlock (utxo', utxoBalance')
            pending''           = updatePending   prefBlock (checkpoints ^. currentAccPendingTxs)
            blockMeta''         = updateBlockMeta prefBlock (checkpoints ^. currentAccBlockMeta)

    rollback (c :| [])      = c :| []
    rollback (c :| c' : cs) = AccCheckpoint {
          _accCheckpointUtxo        = c' ^. accCheckpointUtxo
        , _accCheckpointUtxoBalance = c' ^. accCheckpointUtxoBalance
        , _accCheckpointBlockMeta   = c' ^. accCheckpointBlockMeta
        , _accCheckpointPending     = unionPending (c  ^. accCheckpointPending)
                                                   (c' ^. accCheckpointPending)
        } :| cs

instance UpdatableWalletState AddrCheckpoints where
    newPending _tx = pass
    cancelPending _txids checkpoints = checkpoints

    applyBlock prefBlock checkpoints
        = AddrCheckpoint {
              _addrCheckpointUtxo = InDb utxo''
            , _addrCheckpointUtxoBalance = InDb balance''
            } NE.<| checkpoints
        where
            utxo'        = checkpoints ^. currentAddrUtxo
            utxoBalance' = checkpoints ^. currentAddrUtxoBalance

            (utxo'', balance'') = updateUtxo prefBlock (utxo', utxoBalance')

    rollback (c :| []) = c :| []
    rollback (_ :| c' : cs) = AddrCheckpoint {
          _addrCheckpointUtxo        = c' ^. addrCheckpointUtxo
        , _addrCheckpointUtxoBalance = c' ^. addrCheckpointUtxoBalance
        } :| cs
