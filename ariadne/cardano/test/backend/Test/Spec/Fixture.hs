{--  | Helper module which tries to get rid of a bit of the boilerplate
       needed to initialise a kernel & an active/passive wallet.
--}

module Test.Spec.Fixture (
      withLayer
    , withPassiveWalletFixture
    , withActiveWalletFixture
    , GenActiveWalletFixture
    , GenPassiveWalletFixture
    -- * Useful generators
    , genSpendingPassword
    , inMemoryDBComponent
    , bracketPassiveWallet
    , bracketKernelPassiveWallet
    ) where

import Control.Monad.Component (ComponentM, buildComponent_, runComponentM)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Acid (AcidState)
import Data.Acid.Memory (openMemoryState)
import System.Wlog (Severity)

import Pos.Crypto (PassPhrase, emptyPassphrase)

import Test.QuickCheck (arbitrary, frequency)
import Test.QuickCheck.Monadic (PropertyM, pick)

import qualified Ariadne.Wallet.Cardano.Kernel as Kernel
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (defDB)
import Ariadne.Wallet.Cardano.Kernel.Keystore (Keystore)
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import Ariadne.Wallet.Cardano.WalletLayer
  (ActiveWalletLayer, PassiveWalletLayer)
import qualified Ariadne.Wallet.Cardano.WalletLayer as WalletLayer

-- | Do not pollute the test runner output with logs.
devNull :: Severity -> Text -> IO ()
devNull _ _ = pass

genSpendingPassword :: PropertyM IO PassPhrase
genSpendingPassword =
    pick (frequency [(20, pure emptyPassphrase), (80, arbitrary)])

withLayer :: MonadIO m
          => (PassiveWalletLayer m -> Kernel.PassiveWallet -> IO a)
          -> PropertyM IO a
withLayer cc = do
    liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
        bracketKernelPassiveWallet devNull keystore $ \layer wallet -> do
            cc layer wallet

type GenPassiveWalletFixture x = PropertyM IO (Kernel.PassiveWallet -> IO x)
type GenActiveWalletFixture x  = PropertyM IO (Keystore.Keystore -> Kernel.ActiveWallet -> IO x)

withPassiveWalletFixture :: MonadIO m
                         => GenPassiveWalletFixture x
                         -> (Keystore -> PassiveWalletLayer m -> Kernel.PassiveWallet -> x -> IO a)
                         -> PropertyM IO a
withPassiveWalletFixture prepareFixtures cc = do
    generateFixtures <- prepareFixtures
    liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
        bracketKernelPassiveWallet devNull keystore $ \layer wallet -> do
            fixtures <- generateFixtures wallet
            cc keystore layer wallet fixtures

withActiveWalletFixture :: MonadIO m
                        => GenActiveWalletFixture x
                        -> (Keystore.Keystore -> ActiveWalletLayer m -> Kernel.ActiveWallet -> x -> IO a)
                        -> PropertyM IO a
withActiveWalletFixture prepareFixtures cc = do
    generateFixtures <- prepareFixtures
    liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
        bracketKernelPassiveWallet devNull keystore $ \passiveLayer passiveWallet -> do
            bracketKernelActiveWallet passiveLayer passiveWallet $ \activeLayer activeWallet -> do
                fixtures <- generateFixtures keystore activeWallet
                cc keystore activeLayer activeWallet fixtures

{-------------------------------------------------------------------------------
  Utilities for creating and running in-memory acid-state DB
-------------------------------------------------------------------------------}

inMemoryDBComponent
    :: ComponentM (AcidState Kernel.DB)
inMemoryDBComponent = buildComponent_ "In-memory DB" (openMemoryState defDB)

bracketPassiveWallet
    :: forall a.
       (Severity -> Text -> IO ())
    -> Keystore
    -> (Kernel.PassiveWallet -> IO a)
    -> IO a
bracketPassiveWallet logFunction keystore f =
    runComponentM "Passive wallet (in-memory DB)" pwComponent f
  where
    pwComponent :: ComponentM Kernel.PassiveWallet
    pwComponent = do
        acidDB <- inMemoryDBComponent
        Kernel.passiveWalletCustomDBComponent logFunction keystore acidDB

bracketKernelPassiveWallet
    :: forall m n a. (MonadIO m, MonadUnliftIO n)
    => (Severity -> Text -> IO ())
    -> Keystore
    -> (PassiveWalletLayer m -> Kernel.PassiveWallet -> n a) -> n a
bracketKernelPassiveWallet logFunction keystore f =
    withRunInIO $ \runInIO ->
        runComponentM "Passive wallet layer (in-memory DB)"
            pwlComponent
            (\(pwl, pw) -> runInIO $ f pwl pw)
  where
    pwlComponent :: ComponentM (PassiveWalletLayer m, Kernel.PassiveWallet)
    pwlComponent = do
        acidDB <- inMemoryDBComponent
        WalletLayer.passiveWalletLayerCustomDBComponent logFunction keystore acidDB

bracketKernelActiveWallet
    :: forall m n a. (MonadIO m, MonadUnliftIO n)
    => PassiveWalletLayer m
    -> Kernel.PassiveWallet
    -> (ActiveWalletLayer m -> Kernel.ActiveWallet -> n a) -> n a
bracketKernelActiveWallet pwl pw f =
    withRunInIO $ \runInIO ->
        runComponentM "Active wallet layer"
            awlComponent
            (\(awl, aw) -> runInIO $ f awl aw)
  where
    awlComponent :: ComponentM (ActiveWalletLayer m, Kernel.ActiveWallet)
    awlComponent = WalletLayer.activeWalletLayerComponent pwl pw
