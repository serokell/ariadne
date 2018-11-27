{-# LANGUAGE DataKinds, TypeApplications #-}
module Test.Spec.CreateWallet (
      NewWallet(..)
    , spec
    , genNewWalletRq
    , applyNewWallet
    ) where

import Test.Hspec (Spec, describe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, withMaxSuccess)
import Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)

import Pos.Crypto
  (EncryptedSecretKey, PassPhrase, changeEncPassphrase, emptyPassphrase)

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (AssuranceLevel(..), WalletName(..), hdRootId)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Create (CreateHdRootError(..))
import qualified Ariadne.Wallet.Cardano.Kernel.Internal as Internal
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx (UtxoByAccount)
import Ariadne.Wallet.Cardano.Kernel.Types (WalletId(..))
import Ariadne.Wallet.Cardano.Kernel.Wallets
  (CreateWalletError(..), CreateWithAddress(..), HasNonemptyPassphrase,
  mkHasPP)

import qualified Ariadne.Wallet.Cardano.Kernel.Wallets as Kernel
import qualified Ariadne.Wallet.Cardano.WalletLayer as WalletLayer

import Test.Spec.Fixture (genSpendingPassword, withLayerInMemoryStorage)
import Util.Buildable (ShowThroughBuild(..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data NewWallet = NewWallet
    { newwalESK            :: !EncryptedSecretKey
    , newwalHasNonemptyPP  :: !HasNonemptyPassphrase
    , newwalCreateWithA    :: !CreateWithAddress
    , newwalAssuranceLevel :: !AssuranceLevel
    , newwalName           :: !WalletName
    , newwalUtxoByAccount  :: !UtxoByAccount
    }

applyNewWallet
    :: ( EncryptedSecretKey
      -> HasNonemptyPassphrase
      -> CreateWithAddress
      -> AssuranceLevel
      -> WalletName
      -> UtxoByAccount
      -> a
      )
    -> NewWallet
    -> a
applyNewWallet f NewWallet {..} =
    f newwalESK
      newwalHasNonemptyPP
      newwalCreateWithA
      newwalAssuranceLevel
      newwalName
      newwalUtxoByAccount

genNewWalletRq :: PassPhrase -> PropertyM IO NewWallet
genNewWalletRq pp = do
    assuranceLevel <- pick arbitrary
    walletName     <- pick arbitrary
    eskNoPass      <- pick arbitrary
    Just esk       <- changeEncPassphrase emptyPassphrase pp eskNoPass
    let utxoByAccount = mempty
    return $ NewWallet esk (mkHasPP pp) WithoutAddress assuranceLevel walletName utxoByAccount

spec :: Spec
spec = describe "CreateWallet" $ do
    describe "Wallet creation (wallet layer)" $ do

        prop "works as expected in the happy path scenario" $ withMaxSuccess 50 $ do
            monadicIO $ do
                request <- genNewWalletRq =<< genSpendingPassword
                pm <- pick arbitrary
                withLayerInMemoryStorage pm $ \layer _ -> do
                    liftIO $ do
                        res <- (WalletLayer.pwlCreateWallet layer) `applyNewWallet` request
                        (bimap STB STB res) `shouldSatisfy` isRight

        prop "fails if the wallet already exists" $ withMaxSuccess 50 $ do
            monadicIO $ do
                request <- genNewWalletRq =<< genSpendingPassword
                pm <- pick arbitrary
                withLayerInMemoryStorage pm $ \layer _ -> do
                    liftIO $ do
                        -- The first time it must succeed.
                        res1 <- (WalletLayer.pwlCreateWallet layer) `applyNewWallet` request
                        (bimap STB STB res1) `shouldSatisfy` isRight

                        -- The second time it must not.
                        res2 <- (WalletLayer.pwlCreateWallet layer) `applyNewWallet` request
                        case res2 of
                             Left (CreateWalletFailed (CreateHdRootExists _)) ->
                                 pass
                             Left _ -> fail "creation of new wallet failed for an unexpected reason"
                             Right _ -> fail "expecting wallet not to be created, but it was"

        prop "supports Unicode characters" $ withMaxSuccess 1 $ do
            monadicIO $ do
                request <- genNewWalletRq =<< genSpendingPassword
                pm <- pick arbitrary
                withLayerInMemoryStorage pm $ \layer _ -> do
                    let w' = request { newwalName = "İıÀļƒȑĕďŏŨƞįťŢęșťıİ 日本" }
                    liftIO $ do
                        res <- (WalletLayer.pwlCreateWallet layer) `applyNewWallet` w'
                        (bimap STB STB res) `shouldSatisfy` isRight


    describe "Wallet creation (kernel)" $ do
        prop "correctly persists the ESK in the keystore" $ withMaxSuccess 50 $
            monadicIO $ do
                request <- genNewWalletRq =<< genSpendingPassword

                pm <- pick arbitrary
                withLayerInMemoryStorage @IO pm $ \_ wallet -> do
                    liftIO $ do
                        res <- (Kernel.createHdWallet wallet) `applyNewWallet` request
                        case res of
                             Left e -> throwM e
                             Right hdRoot -> do
                                 --  Check that the key is in the keystore
                                 let wid = WalletIdHdSeq (hdRoot ^. hdRootId)
                                 mbEsk <- Keystore.lookup wid (wallet ^. Internal.walletKeystore)
                                 mbEsk `shouldSatisfy` isJust
