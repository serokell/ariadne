{-# LANGUAGE DataKinds, TypeApplications #-}
module Test.Spec.Accounts (spec) where

import Test.Hspec (Spec, describe, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, withMaxSuccess)
import Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)
import qualified Test.Spec.CreateWallet as Wallets

import qualified Data.Text.Buildable
import Formatting (bprint, build, formatToString, (%))

import Pos.Crypto (ProtocolMagic)

import qualified Ariadne.Wallet.Cardano.Kernel.Accounts as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Delete as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Internal as Internal
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import qualified Ariadne.Wallet.Cardano.Kernel.Wallets as Kernel
import qualified Ariadne.Wallet.Cardano.WalletLayer as WalletLayer
import Ariadne.Wallet.Cardano.WalletLayer.Types (PassiveWalletLayer)

import qualified Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet as IxSet
import Ariadne.Wallet.Cardano.Kernel.Word31 (unsafeMkWord31)

import Test.Spec.Fixture
  (GenPassiveWalletFixture, genSpendingPassword, withLayerInMemoryStorage,
  withPassiveWalletFixture)
import Util.Buildable (ShowThroughBuild(..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data NewAccount = NewAccount
    { naccRootId :: !Kernel.HdRootId
    , naccName   :: !Kernel.AccountName
    }

instance Buildable NewAccount where
    build NewAccount{..} =
        bprint
            ( "NewAccount { naccRootId = " % build
            % ", naccName = " % build
            % " }"
            ) naccRootId naccName

applyNewAccount
    :: ( Kernel.HdRootId
      -> Kernel.AccountName
      -> a
      )
    -> NewAccount
    -> a
applyNewAccount f NewAccount {..} =
    f naccRootId naccName

data Fixture = Fixture
    { fixtureNewAccountRq :: NewAccount
    }

mkHdAccId :: NewAccount -> Word32 -> Kernel.HdAccountId
mkHdAccId NewAccount{..} accIndex =
    Kernel.HdAccountId naccRootId (Kernel.HdAccountIx $ unsafeMkWord31 accIndex)

genNewAccountRq :: Kernel.HdRootId -> PropertyM IO NewAccount
genNewAccountRq hdrId = do
    name <- pick arbitrary
    return $ NewAccount hdrId name

prepareFixtures :: GenPassiveWalletFixture Fixture
prepareFixtures = do
    spendingPassword <- genSpendingPassword
    newWalletRq <- Wallets.genNewWalletRq spendingPassword
    let hdrId = Kernel.eskToHdRootId $ Wallets.newwalESK newWalletRq
    newAccountRq <- genNewAccountRq hdrId
    return $ \pw -> do
        res <- Kernel.createHdWallet pw `Wallets.applyNewWallet` newWalletRq
        case res of
             Left e         -> error (show e)
             Right hdr -> do
                unless (hdr ^. Kernel.hdRootId == hdrId) $ do
                    fail "Incorrect HdRootId generated while preparing fixtures"
                return (Fixture newAccountRq)

withFixture ::
       ProtocolMagic
    -> (Keystore.Keystore -> PassiveWalletLayer IO -> Internal.PassiveWallet -> Fixture -> IO a)
    -> PropertyM IO a
withFixture pm cc = withPassiveWalletFixture pm prepareFixtures cc

spec :: Spec
spec = describe "Accounts" $ do
    describe "CreateAccount" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    res <- (WalletLayer.pwlCreateAccount layer)
                           `applyNewAccount`
                           fixtureNewAccountRq
                    (bimap STB STB res) `shouldSatisfy` isRight

        prop "fails if the parent wallet doesn't exist" $ withMaxSuccess 50 $ do
            monadicIO $ do
                hdrId <- pick arbitrary
                request <- genNewAccountRq hdrId
                pm <- pick arbitrary
                withLayerInMemoryStorage pm $ \layer _ -> do
                    res <- (WalletLayer.pwlCreateAccount layer) `applyNewAccount` request
                    case res of
                         Left (Kernel.CreateAccountKeystoreNotFound _) ->
                             pass
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be created, but it was. HdRootId "
                                        % build
                                        % " , NewAccount request "
                                        % build
                             in fail $ formatToString errMsg hdrId request

    describe "DeleteAccount" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    (Right Kernel.HdAccount{..}) <-
                        (WalletLayer.pwlCreateAccount layer)
                            `applyNewAccount` fixtureNewAccountRq
                    res <- (WalletLayer.pwlDeleteAccount layer) _hdAccountId
                    (bimap STB STB res) `shouldSatisfy` isRight

        prop "fails if the parent wallet doesn't exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                hdAccId <- pick arbitrary
                pm <- pick arbitrary
                withLayerInMemoryStorage pm $ \layer _ -> do
                    res <- (WalletLayer.pwlDeleteAccount layer) hdAccId
                    case res of
                         Left (Kernel.DeleteUnknownHdAccount (Kernel.UnknownHdAccountRoot _)) ->
                             pass
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be deleted, but it was. random HdAccountId "
                                        % build
                             in fail $ formatToString errMsg hdAccId

        prop "fails if the account doesn't exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let hdAccId = mkHdAccId fixtureNewAccountRq 100
                    res <- (WalletLayer.pwlDeleteAccount layer) hdAccId
                    case res of
                         Left (Kernel.DeleteUnknownHdAccount (Kernel.UnknownHdAccount _)) ->
                             pass
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be deleted, but it was. HdAccountId "
                                        % build
                             in fail $ formatToString errMsg hdAccId

    describe "UpdateAccount" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    (Right Kernel.HdAccount{..}) <-
                        (WalletLayer.pwlCreateAccount layer)
                            `applyNewAccount` fixtureNewAccountRq
                    res <- (WalletLayer.pwlUpdateAccountName layer) _hdAccountId "My nice account"
                    case res of
                         Left e -> fail (show e)
                         Right updatedAccount ->
                             (updatedAccount ^. Kernel.hdAccountName) `shouldBe` "My nice account"

        prop "fails if the parent wallet doesn't exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                hdAccId <- pick arbitrary
                pm <- pick arbitrary
                withLayerInMemoryStorage pm $ \layer _ -> do
                    res <- (WalletLayer.pwlUpdateAccountName layer) hdAccId "new account"
                    case res of
                         Left (Kernel.UnknownHdAccountRoot _) ->
                             pass
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be updated, but it was. random HdAccountId "
                                        % build
                             in fail $ formatToString errMsg hdAccId

        prop "fails if the account doesn't exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let hdAccId = mkHdAccId fixtureNewAccountRq 100
                    res <- (WalletLayer.pwlUpdateAccountName layer) hdAccId "new account"
                    case res of
                         Left (Kernel.UnknownHdAccount _) ->
                             pass
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be updated, but it was. random HdAccountId "
                                        % build
                             in fail $ formatToString errMsg hdAccId

    describe "GetAccount" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    (Right Kernel.HdAccount{..}) <-
                        (WalletLayer.pwlCreateAccount layer) `applyNewAccount`
                                                              fixtureNewAccountRq
                    res <- (WalletLayer.pwlGetAccount layer) _hdAccountId
                    case res of
                         Left e    -> fail (show e)
                         Right acc -> acc ^. Kernel.hdAccountId `shouldBe` _hdAccountId

        prop "fails if the parent wallet doesn't exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                hdAccId <- pick arbitrary
                pm <- pick arbitrary
                withLayerInMemoryStorage pm $ \layer _ -> do
                    res <- (WalletLayer.pwlGetAccount layer) hdAccId
                    case res of
                         Left (Kernel.UnknownHdAccountRoot _) ->
                             pass
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be retrieved, but it was. random HdAccountId "
                                        % build
                             in fail $ formatToString errMsg hdAccId

        prop "fails if the account doesn't exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    let hdAccId = mkHdAccId fixtureNewAccountRq 100
                    res <- (WalletLayer.pwlGetAccount layer) hdAccId
                    case res of
                         Left (Kernel.UnknownHdAccount _) ->
                             pass
                         Left unexpectedErr ->
                             fail $ "expecting different failure than " <> show unexpectedErr
                         Right _ ->
                             let errMsg = "expecting account not to be retrieved, but it was. random HdAccountId "
                                        % build
                             in fail $ formatToString errMsg hdAccId

    describe "GetAccounts" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 25 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    forM_ [1..5] $ \(_i :: Int) ->
                        (WalletLayer.pwlCreateAccount layer) `applyNewAccount`
                                                              fixtureNewAccountRq
                    res <- (WalletLayer.pwlGetAccounts layer) (naccRootId fixtureNewAccountRq)
                    case res of
                         Left e     -> fail (show e)
                         Right accs -> IxSet.size accs `shouldBe` 5

        prop "fails if the parent wallet doesn't exists" $ withMaxSuccess 25 $ do
            monadicIO $ do
                hdrId <- pick arbitrary
                pm <- pick arbitrary
                withLayerInMemoryStorage pm $ \layer _ -> do
                    res <- (WalletLayer.pwlGetAccounts layer) hdrId
                    case res of
                         Left (Kernel.UnknownHdRoot _) ->
                             pass
                         Right _ ->
                             let errMsg = "expecting accounts not to be retrieved, but it was. random WalletId "
                                        % build
                                        % " , Wallet "
                             in fail $ formatToString errMsg hdrId
