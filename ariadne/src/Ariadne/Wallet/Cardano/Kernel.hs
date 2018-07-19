{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL), so that we can test it outside of a node context also
-- (in unit tests).
module Ariadne.Wallet.Cardano.Kernel (
    -- * Passive wallet
    PassiveWallet -- opaque
  , WalletId
  , accountUtxo
  , accountTotalBalance
  , applyBlock
  , applyBlocks
  , applyBlunds
  , bracketPassiveWallet
  , initPassiveWallet
  , createWalletHdRnd
  , init
  , walletLogMessage
  , walletPassive
  , wallets
    -- * Active wallet
  , ActiveWallet -- opaque
  , bracketActiveWallet
  , newPending
  ) where

import Universum hiding (State, init)

import Control.Concurrent.MVar (modifyMVar_, withMVar)
import Control.Lens.TH
import Data.Function (fix)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Formatting (build, sformat)

import System.Wlog (Severity(..))

import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import Data.Acid.Memory (openMemoryState)

import Ariadne.Wallet.Cardano.Kernel.Diffusion (WalletDiffusion(..))
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx
  (PrefilteredBlock(..), prefilterBlock, prefilterUtxo)
import Ariadne.Wallet.Cardano.Kernel.Types (WalletESKs, WalletId(..))

import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
  (ApplyBlock(..), CreateHdWallet(..), DB, NewPending(..), NewPendingError,
  Snapshot(..), dbHdWallets, defDB)
import Ariadne.Wallet.Cardano.Kernel.DB.BlockMeta (BlockMeta(..))
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet as HD
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Create as HD
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read (HdQueryErr)
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Resolved (ResolvedBlock)
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Spec.Read as Spec


import Pos.Block.Types (Blund, Undo(..))
import Pos.Core (AddressHash, Coin, Timestamp(..), TxAux(..))

import Ariadne.Wallet.Cardano.Kernel.Types
  (RawResolvedBlock(..), fromRawResolvedBlock)
import Pos.Core.Chrono (NE, OldestFirst(..))
import Pos.Crypto (EncryptedSecretKey, PublicKey)
import Pos.Txp (Utxo)

{-------------------------------------------------------------------------------
  Passive wallet
-------------------------------------------------------------------------------}

-- | Passive wallet
--
-- A passive wallet can receive and process blocks, keeping track of state,
-- but cannot send new transactions.
--
data PassiveWallet = PassiveWallet {
      -- | Send log message
      _walletLogMessage :: Severity -> Text -> IO ()     -- ^ Logger
    , _walletESKs       :: MVar WalletESKs               -- ^ ESKs indexed by WalletId
    , _wallets          :: AcidState DB                  -- ^ Database handle
    , _applyBlunds      :: OldestFirst NE Blund -> IO () -- ^ Blunds apply handle
    }

makeLenses ''PassiveWallet

{-------------------------------------------------------------------------------
  Passive Wallet Resource Management
-------------------------------------------------------------------------------}

-- | Allocate wallet resources
--
-- Here and elsewhere we'll want some constraints on this monad here, but
-- it shouldn't be too specific.
bracketPassiveWallet :: (MonadMask m, MonadIO m)
                     => (Severity -> Text -> IO ())
                     -> (PassiveWallet -> m a) -> m a
bracketPassiveWallet _walletLogMessage f =
    bracket (liftIO $ openMemoryState defDB)
            (\_ -> return ())
            (\db ->
                bracket
                  (liftIO $ initPassiveWallet _walletLogMessage db)
                  (\_ -> return ())
                  f)

{-------------------------------------------------------------------------------
  Manage the WalletESKs Map
-------------------------------------------------------------------------------}

-- | Insert an ESK, indexed by WalletId, to the WalletESK map
insertWalletESK :: PassiveWallet -> WalletId -> EncryptedSecretKey -> IO ()
insertWalletESK pw wid esk
    = modifyMVar_ (pw ^. walletESKs) (return . f)
    where f = Map.insert wid esk

withWalletESKs :: forall a. PassiveWallet -> (WalletESKs -> IO a) -> IO a
withWalletESKs pw = withMVar (pw ^. walletESKs)

{-------------------------------------------------------------------------------
  Wallet Initialisers
-------------------------------------------------------------------------------}

-- | Initialise Passive Wallet with empty Wallets collection
initPassiveWallet :: (Severity -> Text -> IO ())
                  -> AcidState DB
                  -> IO PassiveWallet
initPassiveWallet logMessage db = do
    esks <- Universum.newMVar Map.empty
    return $ fix (\pw -> PassiveWallet logMessage esks db (createApplyHandler pw))

-- | Initialize the Passive wallet (specified by the ESK) with the given Utxo
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
init :: PassiveWallet -> IO ()
init PassiveWallet{..} = _walletLogMessage Info "Passive Wallet kernel initialized"

createApplyHandler :: PassiveWallet -> (OldestFirst NE Blund -> IO ())
createApplyHandler pw = applyBlocks pw . mapMaybe' blundToResolvedBlock
  where
    mapMaybe' :: (a -> Maybe b) -> OldestFirst NE a -> OldestFirst [] b
    mapMaybe' f = OldestFirst . mapMaybe f . toList . getOldestFirst
    rightToJust   = either (const Nothing) Just
    -- The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
    -- by the invariants established in the 'Blund'.
    blundToResolvedBlock :: Blund -> Maybe ResolvedBlock
    blundToResolvedBlock (b,u)
        = rightToJust b <&> \mainBlock ->
            fromRawResolvedBlock
            $ UnsafeRawResolvedBlock mainBlock (map (map fromJust) $ undoTx u)


{-------------------------------------------------------------------------------
  Wallet Creation
-------------------------------------------------------------------------------}

-- | Creates an HD wallet with randomly generated addresses.
--
-- Prefilters the Utxo before passing it to the Acidstate update.

-- Adds an HdRoot and HdAccounts (which are discovered during prefiltering of utxo).
-- (In the case of empty utxo, no HdAccounts are created.)
-- May fail with CreateHdWalletError if the HdRootId already exists

-- The ESK is indexed by WalletId and added to the WalletESK map.
createWalletHdRnd :: PassiveWallet
                  -> HD.WalletName
                  -> HasSpendingPassword
                  -> AssuranceLevel
                  -> (AddressHash PublicKey, EncryptedSecretKey)
                  -> Utxo
                  -> IO (Either HD.CreateHdRootError [HdAccountId])
createWalletHdRnd pw@PassiveWallet{..} name spendingPassword assuranceLevel (pk,esk) utxo = do
    created <- InDb <$> getCurrentTimestamp
    let newRoot = HD.initHdRoot rootId name spendingPassword assuranceLevel created

    res <- update' _wallets $ CreateHdWallet newRoot utxoByAccount Map.empty
    either (return . Left) insertESK res
    where
        utxoByAccount = prefilterUtxo rootId esk utxo
        accountIds    = Map.keys utxoByAccount

        rootId        = HD.HdRootId . InDb $ pk
        walletId      = WalletIdHdRnd rootId

        insertESK _arg = insertWalletESK pw walletId esk >> return (Right accountIds)

-- (NOTE: we are abandoning the 'Mockable time' strategy of the Cardano code base)
getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = Timestamp . round . (* 1000000) <$> getPOSIXTime

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
    withWalletESKs pw $ \esks ->
        return
        $ Map.unions
        $ map prefilterBlock_
        $ Map.toList esks
    where
        prefilterBlock_ (wid,esk) = prefilterBlock wid esk b

-- | Notify all the wallets in the PassiveWallet of a new block
applyBlock :: PassiveWallet
           -> ResolvedBlock
           -> IO ()
applyBlock pw@PassiveWallet{..} b
    = do
        blocksByAccount <- prefilterBlock' pw b
        -- TODO(@uroboros/ryan) do proper metadata initialisation (as part of CBR-239: Support history tracking and queries)
        let blockMeta = BlockMeta . InDb $ Map.empty

        -- apply block to all Accounts in all Wallets
        void $ update' _wallets $ ApplyBlock (blocksByAccount, blockMeta)

-- | Apply multiple blocks, one at a time, to all wallets in the PassiveWallet
--
--   TODO(@matt-noonan) this will be the responsibility of the worker thread (as part of CBR-243: Wallet restoration)
applyBlocks :: PassiveWallet
            -> OldestFirst [] ResolvedBlock
            -> IO ()
applyBlocks = mapM_ . applyBlock

{-------------------------------------------------------------------------------
  Active wallet
-------------------------------------------------------------------------------}

-- | Active wallet
--
-- An active wallet can do everything the passive wallet can, but also
-- send new transactions.
data ActiveWallet = ActiveWallet {
      -- | The underlying passive wallet
      walletPassive   :: PassiveWallet

      -- | The wallet diffusion layer
    , walletDiffusion :: WalletDiffusion
    }

-- | Initialize the active wallet
bracketActiveWallet :: MonadMask m
                    => PassiveWallet
                    -> WalletDiffusion
                    -> (ActiveWallet -> m a) -> m a
bracketActiveWallet walletPassive walletDiffusion =
    bracket
      (return ActiveWallet{..})
      (\_ -> return ())

-- | Submit a new pending transaction
--
-- Will fail if the HdAccountId does not exist or if some inputs of the
-- new transaction are not available for spending.
newPending :: ActiveWallet -> HdAccountId -> TxAux -> IO (Either NewPendingError ())
newPending ActiveWallet{..} accountId tx
  = update' (walletPassive ^. wallets) $ NewPending accountId (InDb tx)

{-------------------------------------------------------------------------------
  Wallet Account read-only API
-------------------------------------------------------------------------------}

walletQuery' :: forall e a. (Buildable e)
             => PassiveWallet
             -> HdQueryErr e a
             -> IO a
walletQuery' pw qry= do
    snapshot <- query' (pw ^. wallets) Snapshot
    let res = qry (snapshot ^. dbHdWallets)
    either err return res
    where
        err = error . sformat build

accountUtxo :: PassiveWallet -> HdAccountId -> IO Utxo
accountUtxo pw accountId
    = walletQuery' pw (Spec.queryAccountUtxo accountId)

accountTotalBalance :: PassiveWallet -> HdAccountId -> IO Coin
accountTotalBalance pw accountId
    = walletQuery' pw (Spec.queryAccountTotalBalance accountId)
