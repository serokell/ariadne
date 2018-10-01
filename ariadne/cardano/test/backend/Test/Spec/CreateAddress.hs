{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Spec.CreateAddress (spec) where

import Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, withMaxSuccess)
import Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)

import qualified Data.ByteString as B
import qualified Data.Map.Strict as M

import Data.Acid (update)

import Pos.Crypto
  (EncryptedSecretKey, PassPhrase, emptyPassphrase, safeDeterministicKeyGen)

import qualified Ariadne.Wallet.Cardano.Kernel.Addresses as Kernel
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (AssuranceLevel(..), HasSpendingPassword(..), HdAccountId(..),
  HdAccountIx(..), HdAddressChain, HdRootId(..), WalletName(..), eskToHdRootId)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Create (initHdRoot)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Derivation ()
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..))
import Ariadne.Wallet.Cardano.Kernel.Internal (PassiveWallet, wallets)
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import Ariadne.Wallet.Cardano.Kernel.Types (AccountId(..), WalletId(..))
import Ariadne.Wallet.Cardano.Kernel.Word31 (unsafeMkWord31)
import Ariadne.Wallet.Cardano.WalletLayer (PassiveWalletLayer)
import qualified Ariadne.Wallet.Cardano.WalletLayer as WalletLayer

import Test.Pos.Core.Arbitrary ()
import qualified Test.Spec.Fixture as Fixture
import Util.Buildable (ShowThroughBuild(..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data Fixture = Fixture {
      fixtureHdRootId     :: HdRootId
    , fixtureESK          :: EncryptedSecretKey
    , fixturePassphrase   :: PassPhrase
    , fixtureAccountId    :: AccountId
    , fixtureAddressChain :: HdAddressChain
    , fixturePw           :: PassiveWallet
    }

-- | Prepare some fixtures using the 'PropertyM' context to prepare the data,
-- and execute the 'acid-state' update once the 'PassiveWallet' gets into
-- scope (after the bracket initialisation).
prepareFixtures :: Fixture.GenPassiveWalletFixture Fixture
prepareFixtures = do
    let passphrase = emptyPassphrase
        (_, esk) = safeDeterministicKeyGen (B.pack $ replicate 32 0x42) passphrase
        newRootId = eskToHdRootId esk
    newRoot <- initHdRoot <$> pure newRootId
                          <*> pure (WalletName "A wallet")
                          <*> pure NoSpendingPassword
                          <*> pure AssuranceLevelNormal
                          <*> (InDb <$> pick arbitrary)
    chain <- pick arbitrary
    let newAccountId = HdAccountId newRootId (HdAccountIx $ unsafeMkWord31 0)
        utxoByAccount = M.singleton newAccountId mempty
    return $ \pw -> do
        void $ liftIO $ update (pw ^. wallets) (CreateHdWallet newRoot utxoByAccount mempty)
        return $ Fixture {
                           fixtureHdRootId = newRootId
                         , fixtureAccountId = AccountIdHdSeq newAccountId
                         , fixtureESK = esk
                         , fixturePassphrase = passphrase
                         , fixtureAddressChain = chain
                         , fixturePw  = pw
                         }

withFixture :: MonadIO m
            => (  Keystore.Keystore
               -> PassiveWalletLayer m
               -> PassiveWallet
               -> Fixture
               -> IO a
               )
            -> PropertyM IO a
withFixture = Fixture.withPassiveWalletFixture prepareFixtures

spec :: Spec
spec = describe "CreateAddress" $ do
    describe "Address creation (wallet layer)" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 200 $
            monadicIO $ do
                withFixture $ \keystore layer _ Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdSeq fixtureHdRootId) fixtureESK keystore
                    let AccountIdHdSeq myAccountId = fixtureAccountId
                    res <- liftIO $ WalletLayer.pwlCreateAddress layer emptyPassphrase myAccountId fixtureAddressChain
                    liftIO ((bimap STB STB res) `shouldSatisfy` isRight)

    describe "Address creation (kernel)" $ do
        prop "works as expected in the happy path scenario" $ withMaxSuccess 200 $
            monadicIO $ do
                withFixture @IO $ \keystore _ _ Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdSeq fixtureHdRootId) fixtureESK keystore
                    res <- liftIO $ Kernel.createAddress fixturePassphrase fixtureAccountId fixtureAddressChain fixturePw
                    liftIO ((bimap STB STB res) `shouldSatisfy` isRight)

        prop "fails if the account has no associated key in the keystore" $ do
            monadicIO $ do
                withFixture @IO $ \_ _ _ Fixture{..} -> do
                    res <- liftIO $ Kernel.createAddress fixturePassphrase fixtureAccountId fixtureAddressChain fixturePw
                    case res of
                        (Left (Kernel.CreateAddressKeystoreNotFound acc)) | acc == fixtureAccountId -> pass
                        x -> fail (show (bimap STB STB x))

        prop "fails if the parent account doesn't exist" $ do
            monadicIO $ do
                withFixture @IO $ \keystore _ _ Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdSeq fixtureHdRootId) fixtureESK keystore
                    let (AccountIdHdSeq hdAccountId) = fixtureAccountId
                    void $ liftIO $ update (fixturePw ^. wallets) (DeleteHdAccount hdAccountId)
                    res <- liftIO $ Kernel.createAddress fixturePassphrase fixtureAccountId fixtureAddressChain fixturePw
                    case res of
                        Left (Kernel.CreateAddressUnknownHdAccount _) -> pass
                        x -> fail (show (bimap STB STB x))

    describe "Address creation (wallet layer & kernel consistency)" $ do
        prop "layer & kernel agrees on the result" $ do
            monadicIO $ do
                res1 <- withFixture @IO $ \keystore _ _ Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdSeq fixtureHdRootId) fixtureESK keystore
                    liftIO (Kernel.createAddress fixturePassphrase fixtureAccountId fixtureAddressChain fixturePw)
                res2 <- withFixture @IO $ \keystore layer _ Fixture{..} -> do
                    liftIO $ Keystore.insert (WalletIdHdSeq fixtureHdRootId) fixtureESK keystore
                    let AccountIdHdSeq myAccountId = fixtureAccountId
                    liftIO $ WalletLayer.pwlCreateAddress layer fixturePassphrase myAccountId fixtureAddressChain
                case res2 of
                     Left err ->
                         return $ (bimap STB STB res1) `shouldBe` (bimap STB STB (Left err))
                     Right _ -> do
                         -- If we get and 'Address', let's check that this is the case also for
                         -- the kernel run. Unfortunately we cannot compare the two addresses for equality
                         -- because the random index will be generated with a seed which changes every time
                         -- as we uses random, IO-based generation deep down the guts.
                         return $ (bimap STB STB res1) `shouldSatisfy` isRight
