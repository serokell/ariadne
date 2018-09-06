module Ariadne.Wallet.Cardano.Kernel.Accounts (
      createAccount
    , deleteAccount
    , updateAccount
    -- * Errors
    , CreateAccountError(..)
    ) where

import qualified Prelude
import Universum

import Control.Monad.Error.Class (liftEither, throwError)
import qualified Data.Text.Buildable
import Formatting (bprint, build, formatToString, (%))
import qualified Formatting as F

import Data.Acid (AcidState, query, update)

import Pos.Crypto (EncryptedSecretKey)

import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
  (CreateHdAccount(..), DB, DeleteHdAccount(..), Snapshot(..),
  UpdateHdAccountName(..), dbHdWallets)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (AccountName(..), HdAccount(..), HdAccountId(..), HdRootId,
  UnknownHdAccount(..))
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Create
  (CreateHdAccountError(..))
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Delete
  (DeleteHdAccountError(..))
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Derivation (mkHdAccountIx)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read (readAccountsByRootId)
import Ariadne.Wallet.Cardano.Kernel.Internal
  (PassiveWallet, walletKeystore, wallets)
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import Ariadne.Wallet.Cardano.Kernel.Types (WalletId(..))

data CreateAccountError =
      CreateAccountUnknownHdRoot HdRootId
      -- ^ When trying to create the 'Account', the parent 'HdRoot' was not
      -- there.
    | CreateAccountKeystoreNotFound WalletId
      -- ^ When trying to create the 'Account', the 'Keystore' didn't have
      -- any secret associated with the input 'WalletId'.
    | CreateAccountHdSeqAccountSpaceSaturated HdRootId
      -- ^ The space of available HD accounts for this root is
      -- completely exhausted.
    deriving Eq

instance Buildable CreateAccountError where
    build (CreateAccountUnknownHdRoot uRoot) =
        bprint ("CreateAccountUnknownHdRoot " % F.build) uRoot
    build (CreateAccountKeystoreNotFound accId) =
        bprint ("CreateAccountKeystoreNotFound " % F.build) accId
    build (CreateAccountHdSeqAccountSpaceSaturated hdAcc) =
        bprint ("CreateAccountHdSeqAccountSpaceSaturated " % F.build) hdAcc

instance Show CreateAccountError where
    show = formatToString build

instance Exception CreateAccountError

-- | Creates a new 'Account' for the input wallet.
-- Note: @it does not@ generate a new 'Address' to go in tandem with this
-- 'Account'. This will be responsibility of the wallet layer.
createAccount :: AccountName
              -- ^ The name for this account.
              -> WalletId
              -- ^ An abstract notion of a 'Wallet identifier
              -> PassiveWallet
              -> IO (Either CreateAccountError HdAccount)
createAccount accountName walletId pw = do
    let keystore = pw ^. walletKeystore
    case walletId of
         WalletIdHdSeq hdRootId -> do
             mbEsk <- Keystore.lookup (WalletIdHdSeq hdRootId) keystore
             case mbEsk of
                  Nothing  -> return (Left $ CreateAccountKeystoreNotFound walletId)
                  Just esk ->
                      createHdSeqAccount accountName
                                         esk
                                         hdRootId
                                         pw

-- | Creates a new 'Account' using sequential HD derivation under the hood.
-- This code follows the same pattern of 'createHdSeqAddress', but the two
-- functions are "similarly different" enough to not make it convenient to generalise
-- the code.
createHdSeqAccount :: AccountName
                   -> EncryptedSecretKey
                   -> HdRootId
                   -> PassiveWallet
                   -> IO (Either CreateAccountError HdAccount)
createHdSeqAccount accountName _esk rootId pw = runExceptT $ go 0
  where
    go :: Word32 -> ExceptT CreateAccountError IO HdAccount
    go collisions =
        case collisions >= maxAllowedCollisions of
            True  -> throwError $ CreateAccountHdSeqAccountSpaceSaturated rootId
            False -> tryGenerateAccount collisions

    tryGenerateAccount :: Word32
                       -- ^ The current number of collisions
                       -> ExceptT CreateAccountError IO HdAccount
    tryGenerateAccount collisions = do
        snapshot <- liftIO $ query db Snapshot

        hdAccounts <- liftEither $
            bimap
            (\_ -> CreateAccountUnknownHdRoot rootId)
            toList
            $ readAccountsByRootId rootId $ snapshot ^. dbHdWallets

        newIndex <- liftEither $
            maybe
            (Left (CreateAccountHdSeqAccountSpaceSaturated rootId))
            Right
            $ mkHdAccountIx hdAccounts

        let hdAccountId = HdAccountId rootId newIndex

        res <- liftIO $ update db (CreateHdAccount hdAccountId mempty accountName)

        case res of
            (Left (CreateHdAccountExists _)) ->
                go (succ collisions)
            (Left (CreateHdAccountUnknownRoot _)) ->
                throwError $ CreateAccountUnknownHdRoot rootId
            Right newAccount -> pure newAccount

    db :: AcidState DB
    db = pw ^. wallets

    -- The maximum number of allowed collisions. This number was
    -- empirically calculated based on a [beta distribution](https://en.wikipedia.org/wiki/Beta_distribution).
    -- In particular, it can be shown how even picking small values for
    -- @alpha@ and @beta@, the probability of failing after the next
    -- collision rapidly approaches 99%. With 50 attempts, our probability
    -- to fail is 98%, and the 42 is a nice easter egg very close to 50,
    -- this is why it was picked.
    maxAllowedCollisions :: Word32
    maxAllowedCollisions = 42

-- | Deletes an HD 'Account' from the data storage.
deleteAccount :: HdAccountId
              -> PassiveWallet
              -> IO (Either DeleteHdAccountError ())
deleteAccount hdAccountId pw = do
    res <- liftIO $ update (pw ^. wallets) (DeleteHdAccount hdAccountId)
    return $ case res of
         Left dbErr -> Left dbErr
         Right ()   -> Right ()

-- | Updates an HD 'Account'.
updateAccount :: HdAccountId
              -> AccountName
              -- ^ The new name for this account.
              -> PassiveWallet
              -> IO (Either UnknownHdAccount (DB, HdAccount))
updateAccount hdAccountId newAccountName pw = do
    res <- liftIO $ update (pw ^. wallets) (UpdateHdAccountName hdAccountId newAccountName)
    return $ case res of
         Left dbError        -> Left dbError
         Right (db, account) -> Right (db, account)
