module Test.Spec.AcidState (spec) where

import System.IO.Temp

import Data.Acid (closeAcidState, openLocalStateFrom, query)
import Data.Acid.Local (getState)

import qualified Data.ByteString.Lazy as BL
import Data.Text.Buildable (build)
import Data.Text.Lazy.Builder (toLazyText)

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, withMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, pick, run)

import Test.Spec.CreateWallet (NewWallet, applyNewWallet, genNewWalletRq)
import Test.Spec.Fixture (genSpendingPasswords, withLayerLocalStorage)

import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (DB(..), Snapshot(..), defDB)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet (HdWallets(..))
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet (IxSet, size)
import Ariadne.Wallet.Cardano.Kernel.Internal (PassiveWallet(..), wallets)
import Ariadne.Wallet.Cardano.WalletLayer (PassiveWalletLayer, pwlCreateWallet)

spec :: Spec
spec = describe "Checking AcidState related functions behavior" $ do
  prop "Check empty DB state" $ withMaxSuccess 1 $
    monadicIO $ run $ withSystemTempDirectory "testWalletDBEmpty" $ \path -> do
      getState path defDB 0 True >>= \case
             Left err -> fail err
             Right (acidDB, eventTag) -> do
               db <- liftIO $ query acidDB Snapshot
               let isEmptyDB = chekEmptyWalletDB db
               return $ isEmptyDB && (eventTag == noEventsMsg)
  prop "Check that opened state is the same as it was before closing." $ withMaxSuccess 10 $ do
            monadicIO $ do
                passwds <- genSpendingPasswords 10
                requests <- mapM genNewWalletRq $ ordNub passwds
                pm <- pick arbitrary
                liftIO $ withSystemTempDirectory "testWalletDBNonEmpty" $ \path -> do
                  withLayerLocalStorage pm path $ \layer wallet -> do
                    res <- checkAcidDBOpenedState wallet layer path requests
                    case res of
                      Left err -> fail $ toString err
                      Right () -> pass

chekEmptyWalletDB :: DB -> Bool
chekEmptyWalletDB (DB (HdWallets hdWallets hdAccounts hdAddresses)) =
  all (== 0) [size hdWallets, size hdAccounts, size hdAddresses]

noEventsMsg :: BL.ByteString
noEventsMsg = "This is the initial state. No methods were applied to the DB."

checkAcidDBOpenedState
  :: PassiveWallet
  -> PassiveWalletLayer IO
  -> FilePath
  -> [NewWallet]
  -> IO (Either Text ())
checkAcidDBOpenedState pw pwl tempDBDir walletsToCreate = do
  let oldDB = pw ^. wallets
  mapM_ (applyNewWallet  (pwlCreateWallet pwl)) walletsToCreate
  (DB (HdWallets oldWallets _ _)) <- query oldDB Snapshot
  closeAcidState oldDB
  newDB <- openLocalStateFrom tempDBDir defDB
  (DB (HdWallets newWallets _ _)) <- query newDB Snapshot

  if (oldWallets /= newWallets)
  then return . Left $ showDifferences oldWallets newWallets
  else return $ Right ()
  where
    showDifferences
      :: Buildable a => IxSet a -> IxSet a -> Text
    showDifferences oldSet newSet = "There are differences \
      \between the database's state when it was closed and its state after opening.\n" <>
      "Here is the the old list (Before closing of the Acid State DB) of wallets: " <>
      (toStrict . toLazyText $ build oldSet) <> "\n" <>
      "Here is the the new list (After opening of the Acid State DB) of wallets: " <>
      (toStrict . toLazyText $ build newSet) <> "\n"
