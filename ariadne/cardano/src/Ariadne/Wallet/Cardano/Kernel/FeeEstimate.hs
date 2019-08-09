-- | Calculate fee estimates for new transaction
module  Ariadne.Wallet.Cardano.Kernel.FeeEstimate
        ( transactionInputs
        , cardanoFee
        ) where

import qualified Text.Show (show)

import Crypto.Random (MonadRandom(..))
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Fmt (pretty)
import Formatting (sformat)
import qualified Formatting as F
import qualified Formatting.Buildable as Buildable
import System.Random.MWC (GenIO, asGenIO, initialize, uniformVector)

import Pos.Chain.Txp (TxOut(..), TxOutAux(..), Utxo)
import Pos.Core
  (Address(..), Coin(..), coinToInteger, sumCoins, unsafeIntegerToCoin)
import qualified Pos.Core as Core
import Pos.Crypto (hash)

import Ariadne.Wallet.Cardano.Kernel.CoinSelection.FromGeneric
  (CoinSelFinalResult(..), CoinSelectionOptions(..),
  ExpenseRegulation(SenderPaysFee), InputGrouping(PreferGrouping),
  dummyAddrAttrSize, dummyTxAttrSize, estimateCardanoFee, estimateMaxTxInputs)
import Ariadne.Wallet.Cardano.Kernel.CoinSelection.Generic (CoinSelHardErr(..))

import qualified Ariadne.Wallet.Cardano.Kernel.CoinSelection.FromGeneric as CoinSelection


data TxInputsException = TxInputsCoinSelErr CoinSelHardErr

instance Buildable.Buildable TxInputsException where
    build (TxInputsCoinSelErr e) = case e of
        (CoinSelHardErrOutputCannotCoverFee _ val) ->
            "Payment to receiver insufficient to cover fee. Fee: " <>
            Buildable.build val
        (CoinSelHardErrOutputIsRedeemAddress _) ->
            "Attempt to pay into a redeem-only address"
        (CoinSelHardErrMaxInputsReached inputs) ->
            "When trying to construct a transaction, the max number of allowed inputs was reached."
            <> " Inputs: " <> Buildable.build inputs
        (CoinSelHardErrCannotCoverFee) ->
          "UTxO exhausted whilst trying to pick inputs to cover remaining fee"
        (CoinSelHardErrUtxoExhausted bal val) ->
           "UTxO exhausted during input selection."
           <> " Balance: " <> Buildable.build bal
           <> " Fee: " <> Buildable.build val
        (CoinSelHardErrUtxoDepleted) ->
           "UTxO depleted using input selection"
        (CoinSelHardErrAddressNotOwned _ addr) ->
           "This wallet does not \"own\" the input address "
           <> Buildable.build addr

instance Show TxInputsException where
    show = pretty

instance Exception TxInputsException

-- | Special monad used to process the payments, which randomness is derived
-- from a fixed seed obtained from hashing the payees. This guarantees that
-- when we estimate the fees and later create a transaction, the coin selection
-- will always yield the same value, making the process externally-predicatable.
newtype PayMonad a = PayMonad {
      buildPayment :: ReaderT Env IO a
    } deriving ( Functor , Applicative , Monad , MonadIO, MonadReader Env)

-- | This 'Env' datatype is necessary to convince GHC that indeed we have
-- a 'MonadReader' instance defined on 'GenIO' for the 'PayMonad'.
newtype Env = Env { getEnv :: GenIO }

-- | \"Invalid\" 'MonadRandom' instance for 'PayMonad' which generates
-- randomness using the hash of 'NonEmpty (Address, Coin)' as fixed seed,
-- plus an internal counter used to shift the bits of such hash.
-- This ensures that the coin selection algorithm runs in a random environment
-- which is yet deterministically reproduceable by feeding the same set of
-- payees.
instance MonadRandom PayMonad where
    getRandomBytes len = do
        gen <- asks getEnv
        randomBytes <- liftIO (asGenIO (flip uniformVector len) gen)
        return $ ByteArray.convert (B.pack $ V.toList randomBytes)

-- | Creates inputs for new transaction
transactionInputs :: Utxo
                  -- ^ Available utxo
                  -> NonEmpty (Address, Coin)
                  -- ^ The payees
                  -> IO Int
transactionInputs availableUtxo payees = do
    let options = CoinSelectionOptions cardanoFee PreferGrouping SenderPaysFee (Core.mkCoin 0)
        maxInputs = estimateMaxTxInputs dummyAddrAttrSize dummyTxAttrSize 65536
    initialEnv <- newEnvironment
    -- Run coin selection.
    res <- flip runReaderT initialEnv . buildPayment $
           CoinSelection.random options
                                maxInputs
                                payees'
                                availableUtxo
    case res of
         Left e -> throwM $ TxInputsCoinSelErr e
         Right (CoinSelFinalResult inputs _ _) -> return $ length inputs
  where

    newEnvironment :: IO Env
    newEnvironment =
        let initialSeed = V.fromList . map fromIntegral
                                     . B.unpack
                                     . encodeUtf8 @Text @ByteString
                                     . sformat F.build
                                     $ hash payees
        in Env <$> initialize initialSeed

    valueToPay :: Coin
    valueToPay =
        -- safe because the argument can't be greater than 'maxBound @Coin'
        -- (because 'min')
        -- and less than 0
        -- (because we sum non-negative values).
        unsafeIntegerToCoin
        (min (coinToInteger maxBound)
             (sumCoins (map snd payees))
        )

    -- We pass only one payee, because current implementation from
    -- 'cardano-sl' fails when utxo size is less than number of
    -- outputs, even though it might be a perfectly legal case (e. g.
    -- one utxo entry with 100500 ADA and 100 outputs with 100 ADA).
    payees' :: NonEmpty TxOutAux
    payees' = one (TxOutAux (TxOut (fst (head payees)) valueToPay))

-- | An estimate of the Tx fees in Cardano based on a sensible number of defaults.
cardanoFee :: Int -> NonEmpty Coin -> Coin
cardanoFee inputs outputs = Core.mkCoin $
    estimateCardanoFee linearFeePolicy inputs (toList $ fmap Core.getCoin outputs)
    where
      linearFeePolicy = Core.TxSizeLinear (Core.Coeff 155381) (Core.Coeff 43.946)
