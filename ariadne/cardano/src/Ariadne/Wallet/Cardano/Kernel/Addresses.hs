module Ariadne.Wallet.Cardano.Kernel.Addresses (
    createAddress
    -- * Errors
    , CreateAddressError(..)
    ) where

import qualified Prelude
import Universum

import qualified Data.Text.Buildable
import Formatting (bprint, build, formatToString, (%))
import qualified Formatting as F

import Data.Acid (AcidState, query, update)

import Pos.Core (IsBootstrapEraAddr(..), mkCoin)
import Pos.Crypto (EncryptedSecretKey, PassPhrase)

import Ariadne.Wallet.Cardano.Kernel.Bip44 (Bip44DerivationPath(..))
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
  (CreateHdAddress(..), DB, Snapshot(..), dbHdWallets)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (HdAccountId, HdAddress, HdAddressChain, HdAddressId(..), hdAccountIdIx,
  hdAccountIdParent)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Create
  (CreateHdAddressError(..), initHdAddress)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Derivation
  (deriveBip44KeyPair, mkHdAddressIx)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read (readAddressesByAccountId)
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..))
import Ariadne.Wallet.Cardano.Kernel.DB.Spec (AddrCheckpoint(..))
import Ariadne.Wallet.Cardano.Kernel.Internal
  (PassiveWallet, walletKeystore, wallets)
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import Ariadne.Wallet.Cardano.Kernel.Types (AccountId(..), WalletId(..))

import Test.QuickCheck (Arbitrary(..), oneof)

data CreateAddressError =
      CreateAddressUnknownHdAccount HdAccountId
      -- ^ When trying to create the 'HdAddress', the parent 'HdAccount' was not
      -- there.
    | CreateAddressKeystoreNotFound AccountId
      -- ^ When trying to create the 'HdAddress', the 'Keystore' didn't have
      -- any secret associated with this 'AccountId'.
      -- there.
    | CreateAddressHdRndGenerationFailed HdAccountId
      -- ^ The crypto-related part of address generation failed. This is
      -- likely to happen if the 'PassPhrase' does not match the one used
      -- to encrypt the 'EncryptedSecretKey'.
    | CreateAddressHdRndAddressSpaceSaturated HdAccountId
      -- ^ The available number of HD addresses in use in such that trying
      -- to find another random index would be too expensive
    deriving Eq

-- TODO(adn): This will be done as part of my work on the 'newTransaction'
-- endpoint, see [CBR-313].
instance Arbitrary CreateAddressError where
    arbitrary = oneof []

instance Buildable CreateAddressError where
    build (CreateAddressUnknownHdAccount uAccount) =
        bprint ("CreateAddressUnknownHdAccount " % F.build) uAccount
    build (CreateAddressKeystoreNotFound accId) =
        bprint ("CreateAddressKeystoreNotFound " % F.build) accId
    build (CreateAddressHdRndGenerationFailed hdAcc) =
        bprint ("CreateAddressHdRndGenerationFailed " % F.build) hdAcc
    build (CreateAddressHdRndAddressSpaceSaturated hdAcc) =
        bprint ("CreateAddressHdRndAddressSpaceSaturated " % F.build) hdAcc

instance Show CreateAddressError where
    show = formatToString build

instance Exception CreateAddressError

-- | Creates a new 'HdAddress' for the input account.
createAddress :: PassPhrase
              -- ^ The 'Passphrase' (a.k.a the \"Spending Password\").
              -> AccountId
              -- ^ An abstract notion of an 'Account' identifier
              -> HdAddressChain
              -> PassiveWallet
              -> IO (Either CreateAddressError HdAddress)
createAddress passphrase accId chain pw = do
    let keystore = pw ^. walletKeystore
    case accId of
         -- \"Standard\" HD random derivation. The strategy is as follows:
         --
         -- 1. Generate the HdAddress' @index@ and @HdAddress@ structure outside
         --    of an atomic acid-state transaction. This could lead to data
         --    races in the sense that an index is picked and such index
         --    is already claimed, but if this happens we simply try again.
         -- 2. Perform the actual creation of the 'HdAddress' as an atomic
         --    transaction in acid-state.
         --
         -- The reason why we do this is because:
         -- 1. We cannot do IO (thus index derivation) in an acid-state
         --    transaction
         -- 2. In order to create an 'HdAddress' we need a proper 'HdAddress',
         -- but this cannot be derived with having access to the
         -- 'EncryptedSecretKey' and the 'PassPhrase', and we do not want
         -- these exposed in the acid-state transaction log.
         (AccountIdHdRnd hdAccId) -> do
             mbEsk <- Keystore.lookup (WalletIdHdRnd (hdAccId ^. hdAccountIdParent))
                                      keystore
             case mbEsk of
                  Nothing  -> return (Left $ CreateAddressKeystoreNotFound accId)
                  Just esk -> createHdRndAddress passphrase esk hdAccId chain pw

-- | Creates a new 'HdAddress' using the random HD derivation under the hood.
-- Being this an operation bound not only by the number of available derivation
-- indexes \"left\" in the account, some form of short-circuiting is necessary.
-- Currently, the algorithm is as follows:
--
-- 1. Try to generate an 'HdAddress' by picking a random index;
-- 2. If the operation succeeds, return the 'HdAddress';
-- 3. If the DB operation fails due to a collision, try again, up to a max of
--    1024 attempts.
-- 4. If after 1024 attempts there is still no result, flag this upstream.
createHdRndAddress :: PassPhrase
                   -> EncryptedSecretKey
                   -> HdAccountId
                   -> HdAddressChain
                   -> PassiveWallet
                   -> IO (Either CreateAddressError HdAddress)
createHdRndAddress passphrase esk accId chain pw = go 0
  where
    go :: Word32 -> IO (Either CreateAddressError HdAddress)
    go collisions =
        case collisions >= maxAllowedCollisions of
            True  -> return $ Left (CreateAddressHdRndAddressSpaceSaturated accId)
            False -> tryGenerateAddress collisions

    tryGenerateAddress :: Word32
                       -- ^ The current number of collisions
                       -> IO (Either CreateAddressError HdAddress)
    tryGenerateAddress collisions = runExceptT $ do
        snapshot <- liftIO $ query db Snapshot

        hdAddresses <- eitherToExceptT $
            bimap
            (\_ -> CreateAddressUnknownHdAccount accId)
            toList
            $ readAddressesByAccountId accId $ snapshot ^. dbHdWallets

        newIndex <- eitherToExceptT $
            maybe
            (Left (CreateAddressHdRndAddressSpaceSaturated accId))
            Right
            $ mkHdAddressIx hdAddresses

        let hdAddressId = HdAddressId accId chain newIndex
            bip44derPath = Bip44DerivationPath
                { bip44AccountIndex = accId ^. hdAccountIdIx
                , bip44AddressChain = chain
                , bip44AddressIndex = newIndex
                }
            mbAddr = deriveBip44KeyPair (IsBootstrapEraAddr True)
                                        passphrase
                                        esk
                                        bip44derPath
        newAddress <- eitherToExceptT $
            maybe
            (Left $ CreateAddressHdRndGenerationFailed accId)
            (Right . fst)
            mbAddr

        let hdAddress =
                initHdAddress hdAddressId (InDb newAddress) firstCheckpoint

        res <- liftIO $ update db (CreateHdAddress hdAddress)

        case res of
            (Left (CreateHdAddressExists _)) ->
                ExceptT $ go (succ collisions)
            (Left (CreateHdAddressUnknown _)) ->
                throwM $ CreateAddressUnknownHdAccount accId
            Right () -> pure hdAddress

    eitherToExceptT :: forall e m a . Monad m => Either e a -> ExceptT e m a
    eitherToExceptT = ExceptT . pure

    db :: AcidState DB
    db = pw ^. wallets

    -- The maximum number of allowed collisions.
    maxAllowedCollisions :: Word32
    maxAllowedCollisions = 32

    firstCheckpoint :: AddrCheckpoint
    firstCheckpoint = AddrCheckpoint
        { _addrCheckpointUtxo        = InDb mempty
        , _addrCheckpointUtxoBalance = InDb (mkCoin 0)
        }
