module Dscp.Wallet.Face
       ( WalletFace (..)
       , Account (..)
       , WalletEvent (..)

         -- Re-exports from Dscp
       , Address
       , Coin (..)
       , Encrypted
       , PassPhrase
       , PublicKey
       , SecretKey
       , Tx
       , TxOut (..)
       ) where

import Dscp.Core (Address, Coin (..), GTx, Tx, TxOut (..))
import Dscp.Crypto (Encrypted, PassPhrase, PublicKey, SecretKey)
import Dscp.Witness.Web.Types

data WalletFace = WalletFace
    { walletRefreshState :: IO ()
    , walletGenKeyPair   :: Maybe Text -> Maybe PassPhrase -> IO Account
    , walletRestoreKey   :: Maybe Text -> Encrypted SecretKey -> Maybe PassPhrase -> IO ()
    , walletListKeys     :: IO [Account]
    , walletSendTx       :: Encrypted SecretKey -> Maybe PassPhrase -> NonEmpty TxOut -> IO Tx
    , walletGetBalance   :: Address -> IO (BlocksOrMempool Coin)
    , walletGetTxHistory :: Address -> IO [GTx]
    }

data Account = Account
    { accountName      :: Maybe Text
    , accountSecretKey :: Encrypted SecretKey
    , accountPublicKey :: PublicKey
    , accountAddress   :: Address
    }
    deriving (Eq, Show)

data WalletEvent
    = WalletStateUpdateEvent [Account]
    deriving (Eq, Show)
