module Ariadne.Wallet.Cardano.Kernel.Addresses
       ( createAddress
         -- * Errors
       , CreateAddressError(..)
       ) where

import Control.Monad.Error.Class (liftEither, throwError)
import Formatting (bprint, build, formatToString, (%))
import qualified Formatting as F
import Formatting.Buildable (Buildable)
import qualified Formatting.Buildable
import qualified Text.Show

import Data.Acid (AcidState, query, update)

import Pos.Core (IsBootstrapEraAddr(..))
import Pos.Core.NetworkMagic (makeNetworkMagic)
import Pos.Crypto (EncryptedSecretKey, PassPhrase)

import Ariadne.Wallet.Cardano.Kernel.Bip44 (Bip44DerivationPath(..))
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
  (CreateHdAddress(..), DB, Snapshot(..), dbHdWallets)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (HdAccountId, HdAddress, HdAddressChain, HdAddressId(..), hdAccountIdIx,
  hdAccountIdParent)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Create
  (CreateHdAddressError(..))
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Derivation
  (deriveBip44KeyPair, mkHdAddressIx)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read (readAddressesByAccountId)
import Ariadne.Wallet.Cardano.Kernel.Internal
  (PassiveWallet, walletKeystore, walletProtocolMagic, wallets)
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import Ariadne.Wallet.Cardano.Kernel.Types (AccountId(..), WalletId(..))
import Ariadne.Wallet.Face

import Test.QuickCheck (Arbitrary(..))

data CreateAddressError =
      CreateAddressUnknownHdAccount HdAccountId
      -- ^ When trying to create the 'HdAddress', the parent 'HdAccount' was not
      -- there.
    | CreateAddressKeystoreNotFound AccountId
      -- ^ When trying to create the 'HdAddress', the 'Keystore' didn't have
      -- any secret associated with this 'AccountId'.
      -- there.
    | CreateAddressHdSeqGenerationFailed HdAccountId
      -- ^ The crypto-related part of address generation failed. This is
      -- likely to happen if the 'PassPhrase' does not match the one used
      -- to encrypt the 'EncryptedSecretKey'.
    | CreateAddressHdSeqAddressSpaceSaturated HdAccountId
      -- ^ The space of available HD addresses for this account is
      -- completely exhausted.
    deriving Eq

-- TODO(adn): This will be done as part of my work on the 'newTransaction'
-- endpoint, see [CBR-313].
instance Arbitrary CreateAddressError where
    arbitrary = error "Arbitrary CreateAddressError is not implemented"

instance Buildable CreateAddressError where
    build (CreateAddressUnknownHdAccount uAccount) =
        bprint ("CreateAddressUnknownHdAccount " % F.build) uAccount
    build (CreateAddressKeystoreNotFound accId) =
        bprint ("CreateAddressKeystoreNotFound " % F.build) accId
    build (CreateAddressHdSeqGenerationFailed hdAcc) =
        bprint ("CreateAddressHdSeqGenerationFailed " % F.build) hdAcc
    build (CreateAddressHdSeqAddressSpaceSaturated hdAcc) =
        bprint ("CreateAddressHdSeqAddressSpaceSaturated " % F.build) hdAcc

instance Show CreateAddressError where
    show = formatToString build

instance Exception CreateAddressError where
    toException e = case e of
        CreateAddressHdSeqGenerationFailed _ -> walletPassExceptionToException e
        _ -> SomeException e
    fromException = walletPassExceptionFromException

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
         -- The strategy is as follows:
         --
         -- 1. Generate the HdAddress' @index@ and @HdAddress@ structure outside
         --    of an atomic acid-state transaction. This could lead to data
         --    races in the sense that an index is picked and such index
         --    is already claimed, but if this happens we simply try again.
         -- 2. Perform the actual creation of the 'HdAddress' as an atomic
         --    transaction in acid-state.
         --
         -- The reason why we do this is because in order to create an 'HdAddress'
         --  we need a proper 'HdAddress', but this cannot be derived with having
         -- access to the 'EncryptedSecretKey' and the 'PassPhrase', and we do not
         -- want these exposed in the acid-state transaction log.
         (AccountIdHdSeq hdAccId) -> do
             mbEsk <- Keystore.lookup (WalletIdHdSeq (hdAccId ^. hdAccountIdParent))
                                      keystore
             case mbEsk of
                  Nothing  -> return (Left $ CreateAddressKeystoreNotFound accId)
                  Just esk -> createHdSeqAddress passphrase esk hdAccId chain pw

-- | Creates a new 'HdAddress' using sequential HD derivation under the hood.
-- Currently, the algorithm is as follows:
--
-- 1. Try to generate an 'HdAddress' by picking the smallest index that is
--    currently unused;
-- 2. If the operation succeeds, return the 'HdAddress';
-- 3. If the DB operation fails due to a collision (because the same index
--    was claimed by a concurrent operation), try again, up to a max of
--    32 attempts.
-- 4. If after 32 attempts there is still no result, flag this upstream.
createHdSeqAddress :: PassPhrase
                   -> EncryptedSecretKey
                   -> HdAccountId
                   -> HdAddressChain
                   -> PassiveWallet
                   -> IO (Either CreateAddressError HdAddress)
createHdSeqAddress passphrase esk accId chain pw = runExceptT $ go 0
  where
    go :: Word32 -> ExceptT CreateAddressError IO HdAddress
    go collisions =
        case collisions >= maxAllowedCollisions of
            True  -> throwError $ CreateAddressHdSeqAddressSpaceSaturated accId
            False -> tryGenerateAddress collisions

    tryGenerateAddress :: Word32
                       -- ^ The current number of collisions
                       -> ExceptT CreateAddressError IO HdAddress
    tryGenerateAddress collisions = do
        snapshot <- liftIO $ query db Snapshot

        hdAddresses <- liftEither $
            bimap
            (\_ -> CreateAddressUnknownHdAccount accId)
            toList
            $ readAddressesByAccountId accId $ snapshot ^. dbHdWallets

        newIndex <- liftEither $
            maybeToRight
            (CreateAddressHdSeqAddressSpaceSaturated accId)
            $ mkHdAddressIx hdAddresses

        let hdAddressId = HdAddressId accId chain newIndex
            bip44derPath = Bip44DerivationPath
                { bip44AccountIndex = accId ^. hdAccountIdIx
                , bip44AddressChain = chain
                , bip44AddressIndex = newIndex
                }
            nm = makeNetworkMagic $ pw ^. walletProtocolMagic
            mbAddr = deriveBip44KeyPair
                nm
                (IsBootstrapEraAddr True)
                passphrase
                esk
                bip44derPath
        (newAddress, _) <- liftEither $
            maybeToRight
            (CreateAddressHdSeqGenerationFailed accId)
            mbAddr

        res <- liftIO $ update db (CreateHdAddress hdAddressId newAddress)

        case res of
            (Left (CreateHdAddressExists _)) ->
                go (succ collisions)
            (Left (CreateHdAddressUnknown _)) ->
                throwError $ CreateAddressUnknownHdAccount accId
            Right hdAddress -> pure hdAddress

    db :: AcidState DB
    db = pw ^. wallets

    -- The maximum number of allowed collisions.
    maxAllowedCollisions :: Word32
    maxAllowedCollisions = 32
