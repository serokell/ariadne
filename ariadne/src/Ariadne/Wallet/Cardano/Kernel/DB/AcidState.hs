{-# LANGUAGE RankNTypes, TemplateHaskell #-}

-- | Acid-state database for the wallet kernel
module Ariadne.Wallet.Cardano.Kernel.DB.AcidState (
    -- * Top-level database
    DB(..)
  , dbHdWallets
    -- * Acid-state operations
    -- ** Snapshot
  , Snapshot(..)
    -- ** Spec mandated updates
  , NewPending(..)
  , ApplyBlock(..)
  , SwitchToFork(..)
    -- ** Updates on HD wallets
    -- *** CREATE
  , CreateHdRoot(..)
  , CreateHdAccount(..)
  , CreateHdAddress(..)
    -- *** UPDATE
  , UpdateHdRootAssurance
  , UpdateHdRootName(..)
  , UpdateHdAccountName(..)
    -- *** DELETE
  , DeleteHdRoot(..)
  , DeleteHdAccount(..)
  ) where

import Universum

import Control.Lens.TH (makeLenses)
import Data.Acid (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopySimple)

import qualified Pos.Core as Core
import Pos.Util.Chrono (OldestFirst(..))

import Ariadne.Wallet.Cardano.Kernel.DB.BlockMeta
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Create as HD
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Delete as HD
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Update as HD
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Resolved
import Ariadne.Wallet.Cardano.Kernel.DB.Spec
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Spec.Update as Spec
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet as IxSet
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState

{-------------------------------------------------------------------------------
  Top-level database
-------------------------------------------------------------------------------}

-- | Full state of the wallet, with the exception of transaction metadata
--
-- We store the different kinds of wallets in different maps for increased
-- type safety. Moreover, since we currently only have a single type of wallet,
-- trying to factor our common parts would be premature at this point.
--
--  References:
--
--  * The acid-state DB for the legacy wallet is defined in module
--    "Pos.Wallet.Web.State.Storage".
--  * V1 API defined in "Ariadne.Wallet.Cardano.API.V1.*" (in @src/@)
data DB = DB {
      _dbHdWallets :: HdWallets
    }

makeLenses ''DB
deriveSafeCopySimple 1 'base ''DB

{-------------------------------------------------------------------------------
  Wrap wallet spec
-------------------------------------------------------------------------------}

-- | Errors thrown by 'newPending'
data NewPendingError =
    -- | Unknown account
    NewPendingUnknown UnknownHdAccount

    -- | Some inputs are not in the wallet utxo
  | NewPendingFailed Spec.NewPendingFailed

deriveSafeCopySimple 1 'base ''NewPendingError

newPending :: HdAccountId
           -> InDb (Core.TxAux)
           -> Update DB (Either NewPendingError ())
newPending accountId tx = runUpdate' . zoom dbHdWallets $ do
    zoomHdAccountId NewPendingUnknown accountId $
      zoom hdAccountCheckpoints $
        mapUpdateErrors NewPendingFailed $ Spec.newPending tx
    zoom hdWalletsAddresses $ do
      allAddresses <- get
      newAddresses <- flip (IxSet.updateIxManyM accountId) allAddresses $ \addr ->
        coerceAction addr $ mapUpdateErrors NewPendingFailed $ Spec.newPending tx
      put newAddresses
  where
    coerceAction
        :: HdAddress
        -> Update' AddrCheckpoints NewPendingError ()
        -> Update' a NewPendingError HdAddress
    coerceAction addr action =
        let oldCheckpoints = view hdAddressCheckpoints addr
            checkpointsOrErr = runIdentity $ runExceptT $ runStateT action oldCheckpoints
        in case checkpointsOrErr of
          Left err -> throwError err
          Right ((), newCheckpoints) -> pure $ set hdAddressCheckpoints newCheckpoints addr

-- | Apply a block
--
-- The block should be prefiltered to contain only inputs and outputs relevant
-- to /any/ of the wallets and accounts.
--
-- NOTE: Calls to 'applyBlock' must be sequentialized by the caller
-- (although concurrent calls to 'applyBlock' cannot interfere with each
-- other, 'applyBlock' must be called in the right order.)
applyBlock :: (ResolvedBlock, BlockMeta) -> Update DB ()
applyBlock block = runUpdateNoErrors $ do
    zoomAll (dbHdWallets . hdWalletsAccounts) $
      hdAccountCheckpoints %~ Spec.applyBlock block
    zoomAll (dbHdWallets . hdWalletsAddresses) $
      hdAddressCheckpoints %~ Spec.applyBlock block

-- | Switch to a fork
--
-- See comments about prefiltering for 'applyBlock'.
--
-- TODO: We use a plain list here rather than 'OldestFirst' since the latter
-- does not have a 'SafeCopy' instance.
switchToFork :: Int
             -> [(ResolvedBlock, BlockMeta)]
             -> Update DB ()
switchToFork n blocks = runUpdateNoErrors $ do
    zoomAll (dbHdWallets . hdWalletsAccounts) $
      hdAccountCheckpoints %~ Spec.switchToFork n (OldestFirst blocks)
    zoomAll (dbHdWallets . hdWalletsAddresses) $
      hdAddressCheckpoints %~ Spec.switchToFork n (OldestFirst blocks)

{-------------------------------------------------------------------------------
  Wrap HD C(R)UD operations
-------------------------------------------------------------------------------}

createHdRoot :: HdRootId
             -> WalletName
             -> HasSpendingPassword
             -> AssuranceLevel
             -> InDb Core.Timestamp
             -> Update DB (Either HD.CreateHdRootError ())
createHdRoot rootId name hasPass assurance created = runUpdate' . zoom dbHdWallets $
    HD.createHdRoot rootId name hasPass assurance created

createHdAccount :: HdRootId
                -> AccountName
                -> AccCheckpoint
                -> Update DB (Either HD.CreateHdAccountError HdAccountId)
createHdAccount rootId name checkpoint = runUpdate' . zoom dbHdWallets $
    HD.createHdAccount rootId name checkpoint

createHdAddress :: HdAddressId
                -> InDb Core.Address
                -> HdAddressChain
                -> AddrCheckpoint
                -> Update DB (Either HD.CreateHdAddressError ())
createHdAddress addrId address chain checkpoint = runUpdate' . zoom dbHdWallets $
    HD.createHdAddress addrId address chain checkpoint

updateHdRootAssurance :: HdRootId
                      -> AssuranceLevel
                      -> Update DB (Either UnknownHdRoot ())
updateHdRootAssurance rootId assurance = runUpdate' . zoom dbHdWallets $
    HD.updateHdRootAssurance rootId assurance

updateHdRootName :: HdRootId
                 -> WalletName
                 -> Update DB (Either UnknownHdRoot ())
updateHdRootName rootId name = runUpdate' . zoom dbHdWallets $
    HD.updateHdRootName rootId name

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update DB (Either UnknownHdAccount ())
updateHdAccountName accId name = runUpdate' . zoom dbHdWallets $
    HD.updateHdAccountName accId name

deleteHdRoot :: HdRootId -> Update DB ()
deleteHdRoot rootId = runUpdateNoErrors . zoom dbHdWallets $
    HD.deleteHdRoot rootId

deleteHdAccount :: HdAccountId -> Update DB (Either UnknownHdRoot ())
deleteHdAccount accId = runUpdate' . zoom dbHdWallets $
    HD.deleteHdAccount accId

{-------------------------------------------------------------------------------
  Acid-state magic
-------------------------------------------------------------------------------}

snapshot :: Query DB DB
snapshot = ask

makeAcidic ''DB [
      -- Database snapshot
      'snapshot
      -- Updates on the "spec state"
    , 'newPending
    , 'applyBlock
    , 'switchToFork
      -- Updates on HD wallets
    , 'createHdRoot
    , 'createHdAccount
    , 'createHdAddress
    , 'updateHdRootAssurance
    , 'updateHdRootName
    , 'updateHdAccountName
    , 'deleteHdRoot
    , 'deleteHdAccount
    ]
