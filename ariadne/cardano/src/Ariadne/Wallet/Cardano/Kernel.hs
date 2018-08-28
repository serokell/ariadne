-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL), so that we can test it outside of a node context also
-- (in unit tests).
module Ariadne.Wallet.Cardano.Kernel (
    -- * Passive wallet
    PassiveWallet -- opaque
  , DB -- opaque
  , WalletId
  , passiveWalletCustomDBComponent
  , init
  , walletLogMessage
  , walletPassive
  , walletKeystore
    -- ** Respond to block chain events
  , applyBlock
  , applyBlocks
  , switchToFork
    -- *** Testing
  , observableRollbackUseInTestsOnly
    -- ** The only effectful getter you will ever need
  , getWalletSnapshot
    -- * Active wallet
  , ActiveWallet -- opaque
  , activeWalletComponent
  , bracketActiveWallet
  , newPending
  , NewPendingError
  ) where

import Prelude hiding (init)

import Control.Monad.Component
  (ComponentM, buildComponent, buildComponent_, runComponentM)
import qualified Data.Map.Strict as Map

import System.Wlog (Severity(..))

import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')

import Ariadne.Wallet.Cardano.Kernel.Internal

import Ariadne.Wallet.Cardano.Kernel.Keystore (Keystore)
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx
  (PrefilteredBlock(..), prefilterBlock)
import Ariadne.Wallet.Cardano.Kernel.Types (WalletId(..))

import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
  (ApplyBlock(..), DB, NewPending(..), NewPendingError,
  ObservableRollbackUseInTestsOnly(..), Snapshot(..), SwitchToFork(..))
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..))
import Ariadne.Wallet.Cardano.Kernel.DB.Resolved (ResolvedBlock)

import Pos.Core.Chrono (OldestFirst)
import qualified Pos.Core.Txp as Txp

{-------------------------------------------------------------------------------
  Passive Wallet Resource Management
-------------------------------------------------------------------------------}

-- | Allocate wallet resources
--
-- Here and elsewhere we'll want some constraints on this monad here, but
-- it shouldn't be too specific.

passiveWalletCustomDBComponent
    :: (Severity -> Text -> IO ())
    -> Keystore
    -> AcidState DB
    -> ComponentM PassiveWallet
passiveWalletCustomDBComponent logMsg keystore acidDB =
    buildComponent_ "Passive wallet" $ initPassiveWallet logMsg keystore acidDB

{-------------------------------------------------------------------------------
  Manage the Wallet's ESKs
-------------------------------------------------------------------------------}

withKeystore :: forall a. PassiveWallet -> (Keystore -> IO a) -> IO a
withKeystore pw action = action (pw ^. walletKeystore)

{-------------------------------------------------------------------------------
  Wallet Initialisers
-------------------------------------------------------------------------------}

-- | Initialise Passive Wallet with empty Wallets collection
initPassiveWallet :: (Severity -> Text -> IO ())
                  -> Keystore
                  -> AcidState DB
                  -> IO PassiveWallet
initPassiveWallet logMessage keystore db = do
    return $ PassiveWallet logMessage keystore db

-- | Initialize the Passive wallet (specified by the ESK) with the given Utxo
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: PassiveWallet -> IO ()
init PassiveWallet{..} =
    _walletLogMessage Info $ "Passive Wallet kernel initialized."

{-------------------------------------------------------------------------------
  Passive Wallet API implementation
-------------------------------------------------------------------------------}

-- | Prefilter the block for each esk in the `WalletESK` map.
--   Return a unified Map of accountId and prefiltered blocks (representing multiple ESKs)
-- TODO(@uroboros/ryan) optimisation: we are prefiltering the block n times for n keys, change this to be a single pass
prefilterBlock' :: PassiveWallet
                -> ResolvedBlock
                -> IO (Map HdAccountId PrefilteredBlock)
prefilterBlock' pw b =
    withKeystore pw $ \ks ->
        (Map.unions . map prefilterBlock_) <$> Keystore.toList ks
    where
        prefilterBlock_ (wid,esk) = prefilterBlock wid esk b

-- | Notify all the wallets in the PassiveWallet of a new block
applyBlock :: PassiveWallet
           -> ResolvedBlock
           -> IO ()
applyBlock pw@PassiveWallet{..} b
    = do
        blocksByAccount <- prefilterBlock' pw b
        -- apply block to all Accounts in all Wallets
        unless (null blocksByAccount) $
            update' _wallets $ ApplyBlock blocksByAccount

-- | Apply multiple blocks, one at a time, to all wallets in the PassiveWallet
--
--   TODO(@matt-noonan) this will be the responsibility of the worker thread (as part of CBR-243: Wallet restoration)
applyBlocks :: PassiveWallet
            -> OldestFirst [] ResolvedBlock
            -> IO ()
applyBlocks = mapM_ . applyBlock

-- | Switch to a new fork
--
-- NOTE: The Ouroboros protocol says that this is only valid if the number of
-- resolved blocks exceeds the length of blocks to roll back.
switchToFork :: PassiveWallet
             -> Int             -- ^ Number of blocks to roll back
             -> [ResolvedBlock] -- ^ Blocks in the new fork
             -> IO ()
switchToFork pw@PassiveWallet{..} n bs = do
    blockssByAccount <- mapM (prefilterBlock' pw) bs
    update' _wallets $ SwitchToFork n blockssByAccount

-- | Observable rollback
--
-- Only used for tests. See 'switchToFork'.
observableRollbackUseInTestsOnly :: PassiveWallet -> IO ()
observableRollbackUseInTestsOnly PassiveWallet{..} =
    update' _wallets $ ObservableRollbackUseInTestsOnly

{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Initialize the active wallet
activeWalletComponent
    :: PassiveWallet
    -> ComponentM ActiveWallet
activeWalletComponent walletPassive = do
    let logMsg = _walletLogMessage walletPassive
    buildComponent "Active wallet"
        (return ActiveWallet{..})
        (\_ -> liftIO $ do
            logMsg Error "stopping the wallet submission layer..."
        )

bracketActiveWallet
    :: PassiveWallet
    -> (ActiveWallet -> IO a) -> IO a
bracketActiveWallet walletPassive runActiveWallet = do
    runComponentM "Active wallet"
        (activeWalletComponent walletPassive)
        runActiveWallet

-- | Submit a new pending transaction
--
-- Will fail if the HdAccountId does not exist or if some inputs of the
-- new transaction are not available for spending.
--
-- If the pending transaction is successfully added to the wallet state, the
-- submission layer is notified accordingly.
newPending :: ActiveWallet -> HdAccountId -> Txp.TxAux -> IO (Either NewPendingError ())
newPending aw accountId tx =
    update' (walletPassive aw ^. wallets) $ NewPending accountId (InDb tx)

-- | The only effectful query on this 'PassiveWallet'.
getWalletSnapshot :: PassiveWallet -> IO DB
getWalletSnapshot pw = query' (pw ^. wallets) Snapshot
