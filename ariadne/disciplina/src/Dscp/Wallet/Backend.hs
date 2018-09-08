module Dscp.Wallet.Backend
       ( WalletFace(..)
       , createWalletFace
       ) where

import Control.Exception (throwIO)
import Control.Monad.Component (ComponentM, buildComponent_)

import Dscp.Core (GTx, Tx (..), TxInAcc (..), TxWitness (..), TxWitnessed (..), mkAddr, toTxId)
import Dscp.Crypto (decrypt, emptyPassPhrase, encrypt, keyGen, sign, toPublic)
import Dscp.Wallet.Face
import Dscp.Wallet.KeyStorage
import Dscp.Web
import Dscp.Witness.Web.Client
import Dscp.Witness.Web.Types

createWalletFace :: BaseUrl -> (WalletEvent -> IO ()) -> ComponentM WalletFace
createWalletFace serverAddress sendEvent = buildComponent_ "Wallet" $ do
    sendStateUpdateEvent sendEvent
    wc <- createWitnessClient serverAddress
    return WalletFace
        { walletRefreshState = sendStateUpdateEvent sendEvent
        , walletGenKeyPair = genKeyPair sendEvent
        , walletRestoreKey = restoreKey sendEvent
        , walletListKeys = listKeys
        , walletSendTx = sendTx wc sendEvent
        , walletGetBalance = getBalance wc
        , walletGetTxHistory = getTxHistory wc
        }

sendStateUpdateEvent :: (WalletEvent -> IO ()) -> IO ()
sendStateUpdateEvent sendEvent = getAccounts >>= sendEvent . WalletStateUpdateEvent

genKeyPair :: (WalletEvent -> IO ()) -> Maybe Text -> Maybe PassPhrase -> IO Account
genKeyPair sendEvent mName mPassPhrase = do
    (sk, pk) <- keyGen
    let account = Account
            { accountName = mName
            , accountSecretKey = encrypt (fromMaybe emptyPassPhrase mPassPhrase) sk
            , accountPublicKey = pk
            , accountAddress = mkAddr pk
            }
    addAccount account
    sendStateUpdateEvent sendEvent
    return account

restoreKey :: (WalletEvent -> IO ()) -> Maybe Text -> Encrypted SecretKey -> Maybe PassPhrase -> IO ()
restoreKey sendEvent mName eSecretKey mPassPhrase = do
    secretKey <- either throwIO return . decrypt (fromMaybe emptyPassPhrase mPassPhrase) $ eSecretKey
    let publicKey = toPublic secretKey
        account = Account
            { accountName = mName
            , accountSecretKey = eSecretKey
            , accountPublicKey = publicKey
            , accountAddress = mkAddr publicKey
            }
    addAccount account
    sendStateUpdateEvent sendEvent

listKeys :: IO [Account]
listKeys = getAccounts

sendTx
    :: WitnessClient
    -> (WalletEvent -> IO ())
    -> Encrypted SecretKey
    -> Maybe PassPhrase
    -> NonEmpty TxOut
    -> IO Tx
sendTx wc sendEvent eSecretKey mPassPhrase (toList -> outs) = do
    secretKey <-
        either throwIO return . decrypt (fromMaybe emptyPassPhrase mPassPhrase) $ eSecretKey
    let publicKey = toPublic secretKey
        address = mkAddr publicKey

    nonce <- aiCurrentNonce <$> wGetAccount wc address False

    let inAcc   = TxInAcc { tiaNonce = nonce, tiaAddr   = address }
        tx      = Tx      { txInAcc  = inAcc, txInValue = inValue, txOuts = outs }
        inValue = Coin $ sum $ unCoin . txOutValue <$> outs

        signature   = sign secretKey (toTxId tx, publicKey, ())
        witness     = TxWitness   { txwSig = signature, txwPk = publicKey }
        txWitnessed = TxWitnessed { twTx   = tx, twWitness = witness }

    void $ wSubmitTx wc txWitnessed
    sendStateUpdateEvent sendEvent
    return tx

getBalance :: WitnessClient -> Address -> IO (BlocksOrMempool Coin)
getBalance wc address = aiBalances <$> wGetAccount wc address False

getTxHistory :: WitnessClient -> Address -> IO [GTx]
getTxHistory wc address = map tiTx . fromMaybe [] . aiTransactions <$> wGetAccount wc address True
