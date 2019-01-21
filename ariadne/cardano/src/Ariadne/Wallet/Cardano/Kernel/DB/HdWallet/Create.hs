{-# LANGUAGE RankNTypes #-}

-- | CREATE operations on HD wallets
module Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Create
       ( -- * Errors
         CreateHdRootError(..)
       , CreateHdAccountError(..)
       , CreateHdAddressError(..)
         -- * Functions
       , createAddrPrefiltered
       , createHdRoot
       , createHdAccount
       , createHdAddress
       , createPrefiltered
         -- * Utilities
       , doNothing
       , mkSingleton
         -- * Initial values
       , initHdRoot
       , initHdAccount
       ) where

import Control.Lens (at, non, to, (.=))
import qualified Data.Map.Strict as Map
import Data.SafeCopy (base, deriveSafeCopySimple)
import Fmt (pretty)
import Formatting (bprint, build, sformat, (%))
import Formatting.Buildable (Buildable)
import qualified Formatting.Buildable

import Pos.Chain.Txp (Utxo)
import qualified Pos.Core as Core

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Spec
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Spec.Util as Spec (balance)
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet as IxSet
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx (AddrWithId, PrefilteredUtxo)

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
    "The wallet '" <> pretty rootId <> "' does not exist."
  displayException (CreateHdAccountExists accountId) =
    "The account '" <> pretty accountId <> "' already exists."

instance Exception CreateHdAddressError where
  displayException (CreateHdAddressUnknown (UnknownHdAccountRoot rootId)) =
    "The wallet " <> pretty rootId <> " does not exist."
  displayException (CreateHdAddressUnknown (UnknownHdAccount accId)) =
    "The account " <> pretty accId <> " does not exist."
  displayException (CreateHdAddressExists addrId) =
    "The address " <> pretty addrId <> " is already used."

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
createHdAccount :: HdAccountId
                -> PrefilteredUtxo
                -> AccountName
                -> Update' HdWallets CreateHdAccountError HdAccount
createHdAccount accountId prefilteredUtxo accountName = do
    -- Check that the root ID exists
    zoomHdRootId CreateHdAccountUnknownRoot rootId $
      pass

    zoom hdWalletsAccounts $ do
      exists <- gets $ IxSet.member accountId
      when exists $ throwError $ CreateHdAccountExists accountId

    createAccPrefiltered
      identity
      doNothing
      mkSingleton
      doNothing
      accountId
      prefilteredUtxo
      (Just accountName)

  where
    rootId = accountId ^. hdAccountIdParent

-- | Create a new address
createHdAddress :: HdAddressId
                -> Core.Address
                -> Update' HdWallets CreateHdAddressError HdAddress
createHdAddress addressId address = do
    zoomHdAccountId CreateHdAddressUnknown (addressId ^. hdAddressIdParent) $
        pass

    -- check newAddress exists
    zoom hdWalletsAddresses $ do
        exists <- gets $ IxSet.member addressId
        when exists $ throwError $ CreateHdAddressExists addressId

    createAddrPrefiltered addressId address Nothing pass

-- | For each of the specified accounts, create them if they do not exist,
-- and apply the specified function.
createPrefiltered :: forall p e.
                     (p -> PrefilteredUtxo)
                      -- ^ Initial UTxO for each of the addresses in the
                      -- newly created account, as well as the addresses
                      -- themselves
                  -> (p -> Update' HdAccount e ())
                      -- ^ Function to apply to the account
                  -> (AddrWithId -> p -> p)
                      -- ^ Function that prefilters p further, leaving only
                      -- stuff that is relevant to the provided address.
                  -> (p -> Update' HdAddress e ())
                      -- ^ Function to apply to the address
                  -> Map HdAccountId p
                  -> Map HdAccountId AccountName
                  -> Update' HdWallets e ()
createPrefiltered mkPrefilteredUtxo accApplyP narrowP addrApplyP pByAccount accountNames =
    forM_ (toPairs pByAccount) $ \(accId, p) ->
        createAccPrefiltered mkPrefilteredUtxo accApplyP narrowP addrApplyP accId p
            (accountNames ^. at accId)

-- | See @createPrefiltered@ for comments on parameters.
createAccPrefiltered :: forall p e.
                        (p -> PrefilteredUtxo)
                     -> (p -> Update' HdAccount e ())
                     -> (AddrWithId -> p -> p)
                     -> (p -> Update' HdAddress e ())
                     -> HdAccountId
                     -> p
                     -> Maybe AccountName
                     -> Update' HdWallets e HdAccount
createAccPrefiltered mkPrefilteredUtxo accApplyP narrowP addrApplyP accId p mbAccountName = do
    let prefilteredUtxo :: PrefilteredUtxo
        prefilteredUtxo = mkPrefilteredUtxo p
        accUtxo :: Utxo
        accUtxo = Map.unions $ elems prefilteredUtxo

    -- apply the update to the account
    let newAccount = initHdAccount accId accountName (firstAccCheckpoint accUtxo)
    zoomOrCreateHdAccount
        assumeHdRootExists
        newAccount
        accId
        (accApplyP p)

    -- create addresses (if they don't exist)
    forM_ (toPairs prefilteredUtxo) $ \(addrWithId@(addressId, address), addrUtxo) ->
        createAddrPrefiltered
            addressId
            address
            (Just addrUtxo)
            (addrApplyP $ narrowP addrWithId p)
    pure newAccount
    where
        accountName :: AccountName
        accountName = fromMaybe defName mbAccountName
          where
            defName = AccountName $ sformat ("Discovered account " % build)
                                            (accId ^. hdAccountIdIx . to getHdAccountIx)

        firstAccCheckpoint :: Utxo -> AccCheckpoint
        firstAccCheckpoint utxo' = AccCheckpoint {
              _accCheckpointUtxo        = InDb utxo'
            , _accCheckpointUtxoBalance = InDb $ Spec.balance utxo'
            , _accCheckpointPending     = Pending . InDb $ Map.empty
            -- Since this is the first checkpoint before we have applied
            -- any blocks, the block metadata is empty
            , _accCheckpointBlockMeta   = mempty
            }

createAddrPrefiltered :: forall e a.
                         HdAddressId
                      -> Core.Address
                      -> Maybe Utxo
                      -> Update' HdAddress e a
                      -> Update' HdWallets e HdAddress
createAddrPrefiltered addressId address mAddrUtxo addrUpdate = do
    _ <- zoomOrCreateHdAddress
          assumeHdAccountExists -- created in previous steps
          newAddress
          addressId
          addrUpdate

    pure newAddress

    where
      newAddress :: HdAddress
      newAddress = initHdAddress addressId (InDb address) firstAddrCheckpoint

      firstAddrCheckpoint :: AddrCheckpoint
      firstAddrCheckpoint = AddrCheckpoint
            { _addrCheckpointUtxo =
                  InDb $ maybeToMonoid mAddrUtxo
            , _addrCheckpointUtxoBalance =
                  InDb $ maybe (Core.mkCoin 0) Spec.balance mAddrUtxo
            }


{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

mkSingleton :: (Ord k, Eq m, Monoid m) => k -> Map k m -> Map k m
mkSingleton addrWithId prefilteredUtxo =
    let utxo = prefilteredUtxo ^. at addrWithId . non mempty in
    one (addrWithId, utxo)

doNothing :: forall p a e. p -> Update' a e ()
doNothing _ = pass

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
              -> AddrCheckpoint
              -> HdAddress
initHdAddress addrId address checkpoint = HdAddress {
      _hdAddressId          = addrId
    , _hdAddressAddress     = address
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
