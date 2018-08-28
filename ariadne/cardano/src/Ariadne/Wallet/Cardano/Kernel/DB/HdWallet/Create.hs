{-# LANGUAGE RankNTypes #-}

-- | CREATE operations on HD wallets
module Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Create (
    -- * Errors
    CreateHdRootError(..)
  , CreateHdAccountError(..)
  , CreateHdAddressError(..)
    -- * Functions
  , createHdRoot
  , createHdAccount
  , createHdAddress
    -- * Initial values
  , initHdRoot
  , initHdAccount
  , initHdAddress
  ) where

import Universum

import Control.Lens (at, (.=))
import Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text.Buildable
import Formatting (bprint, build, (%))

import qualified Pos.Core as Core

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Spec
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet as IxSet

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Errors thrown by 'createHdWallet'
data CreateHdRootError =
    -- | We already have a wallet with the specified ID
    CreateHdRootExists HdRootId
    deriving (Eq, Show)

-- | Errors thrown by 'createHdAccount'
data CreateHdAccountError =
    -- | The specified wallet could not be found
    CreateHdAccountUnknownRoot UnknownHdRoot

    -- | Account already exists
  | CreateHdAccountExists HdAccountId
  deriving (Eq, Show)

-- | Errors thrown by 'createHdAddress'
data CreateHdAddressError =
    -- | Account not found
    CreateHdAddressUnknown UnknownHdAccount

    -- | Address already used
  | CreateHdAddressExists HdAddressId
  deriving (Eq, Show)

instance Exception CreateHdRootError where
  displayException (CreateHdRootExists rootId) =
    "The wallet " ++ show rootId ++ " already exists."

instance Exception CreateHdAccountError where
  displayException (CreateHdAccountUnknownRoot (UnknownHdRoot rootId)) =
    toString $ "The wallet '" <> pretty rootId <> "' does not exist."
  displayException (CreateHdAccountExists accountId) =
    toString $ "The account '" <> pretty accountId <> "' already exists."

instance Exception CreateHdAddressError where
  displayException (CreateHdAddressUnknown (UnknownHdAccountRoot rootId)) =
    toString $ "The wallet " <> pretty rootId <> " does not exist."
  displayException (CreateHdAddressUnknown (UnknownHdAccount accId)) =
    toString $ "The account " <> pretty accId <> " does not exist."
  displayException (CreateHdAddressExists addrId) =
    toString $ "The address " <> pretty addrId <> " is already used."

deriveSafeCopySimple 1 'base ''CreateHdRootError
deriveSafeCopySimple 1 'base ''CreateHdAccountError
deriveSafeCopySimple 1 'base ''CreateHdAddressError

{-------------------------------------------------------------------------------
  CREATE
-------------------------------------------------------------------------------}

-- | Create a new wallet
createHdRoot :: HdRoot -> Update' HdWallets CreateHdRootError ()
createHdRoot hdRoot =
    zoom hdWalletsRoots $ do
      exists <- gets $ IxSet.member rootId
      when exists $ throwError $ CreateHdRootExists rootId
      at rootId .= Just hdRoot
  where
    rootId = hdRoot ^. hdRootId

-- | Create a new account
createHdAccount :: HdAccount -> Update' HdWallets CreateHdAccountError ()
createHdAccount hdAccount = do
    -- Check that the root ID exists
    zoomHdRootId CreateHdAccountUnknownRoot rootId $
      return ()

    zoom hdWalletsAccounts $ do
      exists <- gets $ IxSet.member accountId
      when exists $ throwError $ CreateHdAccountExists accountId
      at accountId .= Just hdAccount
  where
    accountId = hdAccount ^. hdAccountId
    rootId    = accountId ^. hdAccountIdParent

-- | Create a new address
createHdAddress :: HdAddress -> Update' HdWallets CreateHdAddressError ()
createHdAddress hdAddress = do
    -- Check that the account ID exists
    zoomHdAccountId CreateHdAddressUnknown (addrId ^. hdAddressIdParent) $
      return ()
    -- Create the new address
    zoom hdWalletsAddresses $ do
      exists <- gets $ IxSet.member addrId
      when exists $ throwError $ CreateHdAddressExists addrId
      at addrId .= Just hdAddress
  where
    addrId = hdAddress ^. hdAddressId

{-------------------------------------------------------------------------------
  Initial values
-------------------------------------------------------------------------------}

-- | New wallet
--
-- The encrypted secret key of the wallet is assumed to be stored elsewhere in
-- some kind of secure key storage; here we ask for the hash of the public key
-- only (i.e., a 'HdRootId'). It is the responsibility of the caller to use the
-- 'BackupPhrase' and (optionally) the 'SpendingPassword' to create a new key
-- add it to the key storage. This is important, because these are secret
-- bits of information that should never end up in the DB log.
initHdRoot :: HdRootId
           -> WalletName
           -> HasSpendingPassword
           -> AssuranceLevel
           -> InDb Core.Timestamp
           -> HdRoot
initHdRoot rootId name hasPass assurance created = HdRoot {
      _hdRootId          = rootId
    , _hdRootName        = name
    , _hdRootHasPassword = hasPass
    , _hdRootAssurance   = assurance
    , _hdRootCreatedAt   = created
    }

-- | New account
--
-- It is the responsibility of the caller to check the wallet's spending
-- password.
--
-- TODO: If any key derivation is happening when creating accounts, should we
-- store a public key or an address or something?
initHdAccount :: HdAccountId
              -> AccountName
              -> AccCheckpoint
              -> HdAccount
initHdAccount accountId accountName checkpoint = HdAccount {
      _hdAccountId          = accountId
    , _hdAccountName        = accountName
    , _hdAccountCheckpoints = checkpoint :| []
    }

-- | New address in the specified account
--
-- Since the DB does not contain the private key of the wallet, we cannot
-- do the actual address derivation here; this will be the responsibility of
-- the caller (which will require the use of the spending password, if
-- one exists).
--
-- Similarly, it will be the responsibility of the caller to pick a random
-- address index, as we do not have access to a random number generator here.
initHdAddress :: HdAddressId
              -> InDb Core.Address
              -> Bool
              -> AddrCheckpoint
              -> HdAddress
initHdAddress addrId address isUsed checkpoint = HdAddress {
      _hdAddressId          = addrId
    , _hdAddressAddress     = address
    , _hdAddressIsUsed      = isUsed
    , _hdAddressCheckpoints = checkpoint :| []
    }

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Buildable CreateHdRootError where
    build (CreateHdRootExists rootId)
        = bprint ("CreateHdRootError::CreateHdRootExists "%build) rootId

instance Buildable CreateHdAccountError where
    build (CreateHdAccountUnknownRoot (UnknownHdRoot rootId))
        = bprint ("CreateHdAccountError::CreateHdAccountUnknownRoot "%build) rootId
    build (CreateHdAccountExists accountId)
        = bprint ("CreateHdAccountError::CreateHdAccountExists "%build) accountId

instance Buildable CreateHdAddressError where
  build (CreateHdAddressUnknown unknownRoot)
      = bprint ("CreateHdAddressUnknown: "%build) unknownRoot
  build (CreateHdAddressExists addressId)
      = bprint ("CreateHdAddressExists: "%build) addressId
