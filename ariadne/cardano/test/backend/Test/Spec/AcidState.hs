module Test.Spec.AcidState (spec) where

import Control.Exception.Base (ErrorCall(..), handle)
import Control.Monad.Component hiding (throwM)

import System.IO.Temp (withSystemTempDirectory)

import Data.Acid (closeAcidState, openLocalStateFrom, query)

import Data.Text.Buildable (build)
import Data.Text.Lazy.Builder (toLazyText)

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, withMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, pick)

import Test.Spec.Fixture (genSpendingPassword, withLayerLocalStorage)
import Test.Spec.Wallets (NewWallet, applyNewWallet, genNewWalletRq)

import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (DB(..), Snapshot(..), defDB)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet (HdWallets(..))
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet (IxSet, size)
import Ariadne.Wallet.Cardano.Kernel.Internal (PassiveWallet(..), wallets)
import Ariadne.Wallet.Cardano.WalletLayer (PassiveWalletLayer, pwlCreateWallet)

spec :: Spec
spec = describe "AcidState" $ do
  prop "can open empty database" $ withMaxSuccess 1 $
    monadicIO $ do
      pm <- pick arbitrary
      liftIO $ withSystemTempDirectory "testWalletDBEmpty" $ \path -> do
        handle emptyDBErrorCatch $
          withLayerLocalStorage @IO pm path $ \_ wallet -> checkEmptyDB wallet

  prop "opened state is the same as it was before closing" $ withMaxSuccess 10 $ do
    monadicIO $ do
      passwds <- replicateM 10 genSpendingPassword
      requests <- mapM genNewWalletRq passwds
      pm <- pick arbitrary
      liftIO $ withSystemTempDirectory "testWalletDBNonEmpty" $ \path -> do
        handle emptyDBErrorReThrow $
          withLayerLocalStorage pm path $ \layer wallet -> do
            res <- checkAcidDBOpenedState wallet layer path requests
            case res of
              Left err -> fail $ toString err
              Right () -> pass
  where
    emptyDBErrorCatch :: ComponentError -> IO Bool
    emptyDBErrorCatch err = emptyDBErrorHandler err >> return False

    emptyDBErrorReThrow :: ComponentError -> IO ()
    emptyDBErrorReThrow err = emptyDBErrorHandler err >>= \errMsg -> fail errMsg

    emptyDBErrorHandler :: ComponentError -> IO String
    emptyDBErrorHandler
      excp@(ComponentBuildFailed [ComponentAllocationFailed desc internalException] _) =
        case fromException internalException of
          Just (ErrorCall msg) ->
            if ifShouldCatchComponentError (toString desc) msg
            then return $ "Failed to open an empty acid-state DB,\
              \ probably your version of acid-state is broken.\
              \ The text of the internal error: " ++ msg
            else throwM excp
          Nothing -> throwM excp
    emptyDBErrorHandler exception = throwM exception

    ifShouldCatchComponentError :: String -> String -> Bool
    ifShouldCatchComponentError description errMsg =
      description == "Wallet DB" &&
        errMsg == "getState returned Left with pos == 0"

checkEmptyDB :: PassiveWallet -> IO Bool
checkEmptyDB pw = do
  (DB (HdWallets hdWallets hdAccounts hdAddresses)) <- query (pw ^. wallets) Snapshot
  return $ all (== 0) [size hdWallets, size hdAccounts, size hdAddresses]

-- | Add few wallets, closes DB, open it again and check if wallets are the same.
checkAcidDBOpenedState
  :: PassiveWallet
  -> PassiveWalletLayer IO
  -> FilePath
  -> [NewWallet]
  -> IO (Either Text ())
checkAcidDBOpenedState pw pwl tempDBDir walletsToCreate = do
  let oldDB = pw ^. wallets
  mapM_ (applyNewWallet (pwlCreateWallet pwl)) walletsToCreate
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
