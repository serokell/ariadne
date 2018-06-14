-- | Part of the backend which deals with transactions.

module Ariadne.Wallet.Backend.Tx
       ( sendTx
       ) where

import Universum hiding (list)

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Buildable

import Control.Exception (Exception(displayException))
import Control.Lens (at, ix)
import Data.Default (def)
import Formatting (bprint, build, formatToString, int, (%))
import IiExtras ((:~>)(..))
import Pos.Client.KeyStorage (getSecretDefault)
import Pos.Client.Txp.Network (prepareMTx, submitTxRaw)
import Pos.Core.Txp (Tx(..), TxAux(..), TxOutAux(..))
import Pos.Crypto
  (EncryptedSecretKey, PassPhrase, SafeSigner(..), checkPassMatches, hash)
import Pos.Crypto.HD (ShouldCheckPassphrase(..), deriveHDSecretKey)
import Pos.Infra.Diffusion.Types (Diffusion)
import Pos.Launcher (HasConfigurations)
import Pos.Util (maybeThrow)
import Pos.Util.UserSecret (usWallets)
import Text.PrettyPrint.ANSI.Leijen (Doc, list, softline, string)

import Ariadne.Cardano.Face
import Ariadne.Wallet.Backend.KeyStorage
import Ariadne.Wallet.Backend.Mode ()
import Ariadne.Wallet.Face

data SendTxException
    = SendTxNoWallet !Word
    | SendTxNoAddresses !Word
    | SendTxIncorrectPassPhrase
    deriving (Show)

instance Buildable SendTxException where
    build = \case
        SendTxNoWallet walletIdx ->
            bprint ("Wallet #"%int%" does not exist") walletIdx
        SendTxNoAddresses walletIdx ->
            bprint ("Wallet #"%int%" does not have any addresses") walletIdx
        SendTxIncorrectPassPhrase ->
            "Incorrect passphrase"

instance Exception SendTxException where
    displayException = toString . prettyL

-- | Send a transaction from selected to wallet to the list of 'TxOut's.
sendTx ::
       (HasConfigurations, HasCompileInfo)
    => WalletFace
    -> CardanoFace
    -> IORef (Maybe WalletSelection)
    -> (Doc -> IO ())
    -> PassPhrase
    -> WalletReference
    -> NonEmpty TxOut
    -> IO TxId
sendTx WalletFace {..} CardanoFace {..} walletSelRef printAction pp walletRef outs = do
    let Nat runCardanoMode = cardanoRunCardanoMode
    walletIdx <- resolveWalletRef walletSelRef runCardanoMode walletRef
    runCardanoMode $ sendTxDo walletIdx =<< cardanoGetDiffusion
  where
    sendTxDo :: Word -> Diffusion CardanoMode -> CardanoMode TxId
    sendTxDo walletIdx diffusion = do
        wallets <- view usWallets <$> getSecretDefault
        wd <-
            maybeThrow
                (SendTxNoWallet walletIdx)
                (wallets ^? ix (fromIntegral walletIdx))
        maybeThrow SendTxIncorrectPassPhrase $
            checkPassMatches pp (_wdRootKey wd)
        let signersMap :: HashMap Address SafeSigner
            -- safe due to passphrase check above
            signersMap = walletSigners pp wd
        let getSigner :: Address -> Maybe SafeSigner
            getSigner addr = signersMap ^. at addr
        -- TODO: generate new change address
        ourAddresses <-
            maybeThrow
                (SendTxNoAddresses walletIdx)
                (nonEmpty $ HM.keys signersMap)
        let ourAddress = NE.head ourAddresses
        (txAux, _) <-
            prepareMTx
                getSigner
                mempty
                def
                ourAddresses
                (map TxOutAux outs)
                ourAddress
        let tx = taTx txAux
        let txId = hash tx
        liftIO $ printAction $ formatSubmitTxMsg tx
        txId <$ submitTxRaw diffusion txAux
    formatToDoc :: forall a. Buildable a => a -> Doc
    formatToDoc = string . formatToString build
    formatSubmitTxMsg :: Tx -> Doc
    formatSubmitTxMsg UnsafeTx {..} = mconcat
        [ "Submitting Tx with inputs: "
        , list . toList $ map formatToDoc _txInputs
        , ","
        , softline
        , "outputs: "
        , list . toList $ map formatToDoc _txOutputs
        , "…"
        ]

-- Assumes the passphrase is correct!
walletSigners :: PassPhrase -> WalletData -> HashMap Address SafeSigner
walletSigners pp WalletData {..} = foldMap accountSigners _wdAccounts
  where
    deriveSecretKey :: EncryptedSecretKey -> Word32 -> EncryptedSecretKey
    deriveSecretKey encSK idx =
        case deriveHDSecretKey (ShouldCheckPassphrase False) pp encSK idx of
            Nothing ->
                error
                    "walletSigners: passphrase is invalid and why was it checked at all??!!?"
            Just key -> key
    accountSigners :: AccountData -> HashMap Address SafeSigner
    accountSigners AccountData {..} =
        let accountKey :: EncryptedSecretKey
            accountKey = deriveSecretKey _wdRootKey _adPath
            step ::
                   HashMap Address SafeSigner
                -> (Word32, Address)
                -> HashMap Address SafeSigner
            step m (addrIdx, addr) =
                m &
                at addr .~
                Just (SafeSigner (deriveSecretKey accountKey addrIdx) pp)
         in foldl' step mempty _adAddresses
