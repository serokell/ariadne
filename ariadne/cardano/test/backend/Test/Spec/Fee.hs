-- | Tests for fees functionality of the wallet layer.

module Test.Spec.Fee
       ( spec
       ) where

import Data.Default (def)
import qualified Data.Text.Buildable
import Formatting (bprint, sformat, (%))
import Serokell.Util (listJson)
import Test.Hspec (Spec, describe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  (Gen, arbitrary, listOf, resize, suchThat, withMaxSuccess)
import Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)
import Time (Millisecond, threadDelay)

import Pos.Block.Types (Blund, SlogUndo(..), Undo(..))
import Pos.Core.Block (MainBlock, MainBody(..), mkMainBlockExplicit)
import Pos.Core.Chrono (NE, OldestFirst(..))
import Pos.Core.Common
  (Coin(..), coinToInteger, divCoin, largestPubKeyAddressBoot, sumCoins,
  unsafeIntegerToCoin)
import Pos.Core.NetworkMagic (NetworkMagic(NMNothing))
import Pos.Core.Slotting (SlotId(..), localSlotIndexMinBound)
import Pos.Core.Ssc (SscPayload(CertificatesPayload))
import Pos.Core.Txp
  (Tx(..), TxAux(..), TxIn(..), TxInWitness(..), TxOut(..), TxOutAux(..),
  TxSigData(..), TxUndo, TxpUndo, mkTxPayload)
import Pos.Core.Update
  (ApplicationName(..), BlockVersion(..), SoftwareVersion(..))
import Pos.Crypto
  (ProtocolMagic, SecretKey, SignTag(..), hash, sign, toPublic, unsafeHash)
import Pos.Data.Attributes (mkAttributes)
import Pos.Delegation.Types (DlgUndo(..))
import Test.Pos.Txp.Arbitrary ()
import Test.Pos.Util.QuickCheck.Arbitrary (runGen)

import qualified Ariadne.Wallet.Cardano.Kernel as Kernel (getWalletSnapshot)
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (dbHdWallets)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (hdAddressAddress, hdAddressId, hdAddressIdParent)
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read as HDRead
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(_fromDb))
import qualified Ariadne.Wallet.Cardano.Kernel.Internal as Internal
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import qualified Ariadne.Wallet.Cardano.Kernel.Wallets as Kernel
import qualified Ariadne.Wallet.Cardano.WalletLayer as WalletLayer
import Ariadne.Wallet.Cardano.WalletLayer.Types (PassiveWalletLayer)

import Test.Spec.Fixture
  (GenPassiveWalletFixture, genSpendingPassword, withPassiveWalletFixture)
import qualified Test.Spec.Wallets as Wallets
import Util.Buildable (ShowThroughBuild(..))

data EstimateFees = EstimateFees
    { eeInputAccounts :: ![Kernel.HdAccountId]
    , eeOutputs :: !(NonEmpty TxOut)
    }

instance Buildable EstimateFees where
    build EstimateFees{..} =
        bprint
            ( "EstimateFees { inputs = " % listJson
            % ", outputs = " % listJson
            % " }"
            ) eeInputAccounts eeOutputs

applyEstimateFees
    :: ( [Kernel.HdAccountId]
      -> NonEmpty TxOut
      -> a
      )
    -> EstimateFees
    -> a
applyEstimateFees f EstimateFees {..} = f eeInputAccounts eeOutputs

data ApplyBlocks = ApplyBlocks
    { abBlunds :: !(OldestFirst NE Blund)
    }

applyApplyBlocks
    :: ( OldestFirst NE Blund
      -> a
      )
    -> ApplyBlocks
    -> a
applyApplyBlocks f ApplyBlocks {..} = f abBlunds

data Fixture = Fixture
    { fixtureEstimateFeesRq :: !EstimateFees
    , fixtureApplyBlocks :: !ApplyBlocks
    }

-- Here we prepare a fixture for the following scenario:
-- 1. Generate a new wallet and add it to our wallet (with address).
-- 2. We should have one address in our wallet, read it.
-- 3. Generate a list of outputs to be passed to the fee estimation function.
-- 4. Generate a fake transaction which sends enough money to our address.
-- 5. Generate a fake block with the fake transaction from (4).
-- 6. Apply this fake block.
-- 7. Estimate fees for a new transaction and check the value is reasonable.
prepareFixtures :: GenPassiveWalletFixture Fixture
prepareFixtures = do
    spendingPassword <- genSpendingPassword
    newWalletRq' <- Wallets.genNewWalletRq spendingPassword
    let newWalletRq = newWalletRq'
          { Wallets.newwalCreateWithA = Kernel.WithAddress spendingPassword }
    let hdrId = Kernel.eskToHdRootId $ Wallets.newwalESK newWalletRq
    outs <- pick $
        genOuts `suchThat` ((coinToInteger maxOutput >=) . sumOfOutputs)
    return $ \pw -> do
        createWalletRes <-
            Kernel.createHdWallet pw `Wallets.applyNewWallet` newWalletRq
        case createWalletRes of
             Left e         -> error (show e)
             Right _ -> do
                 snapshot <- Kernel.getWalletSnapshot pw
                 let addresses = toList <$>
                       HDRead.readAddressesByRootId hdrId
                       (snapshot ^. dbHdWallets)
                 case addresses of
                      Right [hdAddr] -> prepareFixturesDo pw outs hdAddr
                      Right addrs ->
                          error $ "Number of addresses differs from 1: " <>
                                  sformat listJson addrs
                      Left e -> error (show e)
  where
    maxOutput :: Coin
    maxOutput = maxBound `divCoin` (2 :: Int)
    sumOfOutputs :: NonEmpty TxOut -> Integer
    sumOfOutputs outputs = sumCoins (map txOutValue outputs)

    genOuts :: Gen (NonEmpty TxOut)
    genOuts = (:|) <$> genOut <*> resize 5 (listOf genOut)
      where
        genOut = TxOut <$> genOutAddr <*> arbitrary
        -- Doesn't really matter, but it must not be a redeem address.
        genOutAddr = pure (largestPubKeyAddressBoot NMNothing)

    prepareFixturesDo ::
        Internal.PassiveWallet -> NonEmpty TxOut -> Kernel.HdAddress -> IO Fixture
    prepareFixturesDo pw outs hdAddr = do
        let pm :: ProtocolMagic
            pm = pw ^. Internal.walletProtocolMagic
        let hdAccId = hdAddr ^. hdAddressId . hdAddressIdParent
        let estimateFees = EstimateFees
              { eeInputAccounts = one hdAccId
              , eeOutputs = outs
              }

        let fakeSecretKey :: SecretKey
            fakeSecretKey = runGen arbitrary
        let fakeTxIn :: TxIn
            fakeTxIn = TxInUtxo
                { txInHash = unsafeHash True
                , txInIndex = 0
                }
        let txOut = TxOut
                { txOutAddress = _fromDb (hdAddr ^. hdAddressAddress)
                -- Make sure there are enough fees.
                , txOutValue = unsafeIntegerToCoin
                    (sumOfOutputs outs + 1000 * 1000 * 1000)
                }
        let fakeTx :: Tx
            fakeTx = UnsafeTx
                { _txInputs = pure fakeTxIn
                , _txOutputs = pure txOut
                , _txAttributes = mkAttributes ()
                }
        let fakeTxInWitness :: TxInWitness
            fakeTxInWitness = PkWitness
                { twKey = toPublic fakeSecretKey
                , twSig = sign pm SignTx fakeSecretKey (TxSigData $ hash fakeTx)
                }
        let fakeTxAux :: TxAux
            fakeTxAux = TxAux
                { taTx = fakeTx
                , taWitness = one fakeTxInWitness
                }
        let fakeBody :: MainBody
            fakeBody = MainBody
                { _mbTxPayload = mkTxPayload [fakeTxAux]
                , _mbSscPayload = CertificatesPayload mempty
                , _mbDlgPayload = def
                , _mbUpdatePayload = def
                }
        let fakeBlockVersion :: BlockVersion
            fakeBlockVersion = BlockVersion 0 0 0
        let fakeSoftwareVersion :: SoftwareVersion
            fakeSoftwareVersion = SoftwareVersion (ApplicationName "a") 0
        let fakeBlock :: MainBlock
            fakeBlock =
                mkMainBlockExplicit pm fakeBlockVersion fakeSoftwareVersion
                (unsafeHash False) 0 (SlotId 0 localSlotIndexMinBound)
                fakeSecretKey Nothing fakeBody
        let fakeTxUndo :: TxUndo
            fakeTxUndo = one (Just (TxOutAux txOut))
        let fakeTxpUndo :: TxpUndo
            fakeTxpUndo = [fakeTxUndo]
        let fakeUndo :: Undo
            fakeUndo = Undo
                { undoTx = fakeTxpUndo
                , undoDlg = DlgUndo mempty mempty
                , undoUS = def
                , undoSlog = SlogUndo Nothing
                }
        let applyBlocks = ApplyBlocks
              { abBlunds = OldestFirst $ one (Right fakeBlock, fakeUndo)
              }
        let fixture = Fixture
              { fixtureEstimateFeesRq = estimateFees
              , fixtureApplyBlocks = applyBlocks
              }
        pure fixture

withFixture ::
       ProtocolMagic
    -> (Keystore.Keystore -> PassiveWalletLayer IO -> Internal.PassiveWallet -> Fixture -> IO a)
    -> PropertyM IO a
withFixture pm cc = withPassiveWalletFixture pm prepareFixtures cc

spec :: Spec
spec = describe "Fees" $ do
    describe "EstimateFees" $ do

        prop "computes reasonable fees in the happy path scenario" $
            withMaxSuccess 5 $ do
            monadicIO $ do
                pm <- pick arbitrary
                withFixture pm $ \_ layer _ Fixture{..} -> do
                    WalletLayer.pwlApplyBlocks layer
                        `applyApplyBlocks`
                        fixtureApplyBlocks
                    -- Block is applied in a separate thread :(
                    -- It's a bit hacky approach to make sure it will be applied.
                    -- Hopefully 100 milliseconds will always be enough.
                    threadDelay @Millisecond 100
                    res <- WalletLayer.pwlEstimateFees layer
                           `applyEstimateFees`
                           fixtureEstimateFeesRq
                    let minReasonableFee, maxReasonableFee :: Word64
                        minReasonableFee = 100000
                        maxReasonableFee = 400000
                    let isReasonableFee (STB (Coin fee)) =
                          fee >= minReasonableFee &&
                          fee <= maxReasonableFee
                    STB res `shouldSatisfy` isReasonableFee
