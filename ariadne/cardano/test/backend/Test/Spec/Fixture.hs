{--  | Helper module which tries to get rid of a bit of the boilerplate
       needed to initialise a kernel & an active/passive wallet.
--}

module Test.Spec.Fixture (
      withLayerInMemoryStorage
    , withLayerLocalStorage
    , withPassiveWalletFixture
    , withActiveWalletFixture
    , GenActiveWalletFixture
    , GenPassiveWalletFixture
    -- * Useful generators
    , genSpendingPassword
    , inMemoryDBComponent
    , bracketPassiveWallet
    , bracketKernelPassiveWallet
    , bracketActiveWallet
    ) where

import Control.Monad.Component (ComponentM, buildComponent_, runComponentM)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.Acid (AcidState)
import Data.Acid.Memory (openMemoryState)
import System.Wlog (Severity)

import Pos.Crypto (PassPhrase, ProtocolMagic, emptyPassphrase)

import Test.QuickCheck (arbitrary, frequency)
import Test.QuickCheck.Monadic (PropertyM, pick)

import qualified Ariadne.Wallet.Cardano.Kernel as Kernel
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (defDB)
import Ariadne.Wallet.Cardano.Kernel.Keystore (Keystore)
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import Ariadne.Wallet.Cardano.WalletLayer
  (ActiveWalletLayer, PassiveWalletLayer)
import qualified Ariadne.Wallet.Cardano.WalletLayer as WalletLayer

data FileOrMemoryDB = Filesystem FilePath | Memory

-- | Do not pollute the test runner output with logs.
devNull :: Severity -> Text -> IO ()
devNull _ _ = pass

genSpendingPassword :: PropertyM IO PassPhrase
genSpendingPassword =
    pick (frequency [(20, pure emptyPassphrase), (80, arbitrary)])

withLayerInMemoryStorage
  :: MonadIO m
  => ProtocolMagic
  -> (PassiveWalletLayer m -> Kernel.PassiveWallet -> IO a)
  -> PropertyM IO a
withLayerInMemoryStorage pm cc = liftIO $ withLayer pm Memory cc

withLayerLocalStorage
  :: MonadIO m
  => ProtocolMagic
  -> FilePath
  -> (PassiveWalletLayer m -> Kernel.PassiveWallet -> IO a)
  -> IO a
withLayerLocalStorage pm pathToDB cc = withLayer pm (Filesystem pathToDB) cc

withLayer
  :: MonadIO m
  => ProtocolMagic
  -> FileOrMemoryDB
  -> (PassiveWalletLayer m -> Kernel.PassiveWallet -> IO a)
  -> IO a
withLayer pm fileOrMemory cc =
  liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
    bracketKernelPassiveWallet pm devNull keystore fileOrMemory $ \layer wallet -> do
      cc layer wallet


type GenPassiveWalletFixture x = PropertyM IO (Kernel.PassiveWallet -> IO x)
type GenActiveWalletFixture x  = PropertyM IO (Keystore.Keystore -> Kernel.ActiveWallet -> IO x)

withPassiveWalletFixture
  :: MonadIO m
  => ProtocolMagic
  -> GenPassiveWalletFixture x
  -> (Keystore -> PassiveWalletLayer m -> Kernel.PassiveWallet -> x -> IO a)
  -> PropertyM IO a
withPassiveWalletFixture pm prepareFixtures cc = do
  generateFixtures <- prepareFixtures
  liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
    bracketKernelPassiveWallet pm devNull keystore Memory $ \layer wallet -> do
      fixtures <- generateFixtures wallet
      cc keystore layer wallet fixtures

withActiveWalletFixture
  :: MonadIO m
  => ProtocolMagic
  -> GenActiveWalletFixture x
  -> (Keystore.Keystore -> ActiveWalletLayer m -> Kernel.ActiveWallet -> x -> IO a)
  -> PropertyM IO a
withActiveWalletFixture pm prepareFixtures cc = do
  generateFixtures <- prepareFixtures
  liftIO $ Keystore.bracketTestKeystore $ \keystore -> do
    bracketKernelPassiveWallet pm devNull keystore Memory $ \passiveLayer passiveWallet -> do
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
     ProtocolMagic
  -> (Severity -> Text -> IO ())
  -> Keystore
  -> (Kernel.PassiveWallet -> IO a)
  -> IO a
bracketPassiveWallet pm logFunction keystore f =
    runComponentM "Passive wallet (in-memory DB)" pwComponent f
  where
    pwComponent :: ComponentM Kernel.PassiveWallet
    pwComponent = do
      acidDB <- inMemoryDBComponent
      Kernel.passiveWalletCustomDBComponent logFunction keystore acidDB pm

bracketKernelPassiveWallet
  :: forall m n a. (MonadIO m, MonadUnliftIO n)
  => ProtocolMagic
  -> (Severity -> Text -> IO ())
  -> Keystore
  -> FileOrMemoryDB
  -> (PassiveWalletLayer m -> Kernel.PassiveWallet -> n a) -> n a
bracketKernelPassiveWallet pm logFunction keystore tempDBType f =
  withRunInIO $ \runInIO ->
    runComponentM "Passive wallet layer"
      pwlComponent
        (\(pwl, pw) -> runInIO $ f pwl pw)
  where
    pwlComponent :: ComponentM (PassiveWalletLayer m, Kernel.PassiveWallet)
    pwlComponent = do
      acidDB <- case tempDBType of
        Memory -> inMemoryDBComponent
        Filesystem tmpPath -> WalletLayer.walletDBComponent tmpPath
      WalletLayer.passiveWalletLayerCustomDBComponent logFunction keystore acidDB pm

bracketActiveWallet
  :: Kernel.PassiveWallet
  -> (Kernel.ActiveWallet -> IO a) -> IO a
bracketActiveWallet walletPassive runActiveWallet = do
  runComponentM "Active wallet"
    (Kernel.activeWalletComponent walletPassive)
    runActiveWallet

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
