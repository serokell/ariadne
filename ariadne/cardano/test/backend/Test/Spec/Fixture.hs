{--  | Helper module which tries to get rid of a bit of the boilerplate
       needed to initialise a kernel & an active/passive wallet.
--}

module Test.Spec.Fixture (
      withLayer
    , withPassiveWalletFixture
    , GenPassiveWalletFixture
    -- * Useful generators
    , genSpendingPassword
    ) where

import System.Wlog (Severity)

import Pos.Crypto (PassPhrase, emptyPassphrase)

import Test.QuickCheck (arbitrary, frequency)
import Test.QuickCheck.Monadic (PropertyM, pick)

import Ariadne.Wallet.Cardano.Kernel.Internal (PassiveWallet)
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import Ariadne.Wallet.Cardano.WalletLayer (PassiveWalletLayer)
import qualified Ariadne.Wallet.Cardano.WalletLayer as WalletLayer

-- | Do not pollute the test runner output with logs.
devNull :: Severity -> Text -> IO ()
devNull _ _ = pass

genSpendingPassword :: PropertyM IO PassPhrase
genSpendingPassword =
    pick (frequency [(20, pure emptyPassphrase), (80, arbitrary)])

withLayer :: MonadIO m
          => (PassiveWalletLayer m -> PassiveWallet -> IO a)
          -> PropertyM IO a
withLayer cc = do
    liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
        WalletLayer.bracketKernelPassiveWallet devNull keystore $ \layer wallet -> do
            cc layer wallet

type GenPassiveWalletFixture x = PropertyM IO (PassiveWallet -> IO x)

withPassiveWalletFixture :: MonadIO m
                         => GenPassiveWalletFixture x
                         -> (Keystore.Keystore -> PassiveWalletLayer m -> PassiveWallet -> x -> IO a)
                         -> PropertyM IO a
withPassiveWalletFixture prepareFixtures cc = do
    generateFixtures <- prepareFixtures
    liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
        WalletLayer.bracketKernelPassiveWallet devNull keystore $ \layer wallet -> do
            fixtures <- generateFixtures wallet
            cc keystore layer wallet fixtures
