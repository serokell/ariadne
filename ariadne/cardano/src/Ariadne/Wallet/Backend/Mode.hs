{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module contains instances of some type classes necessary for wallet.
--
-- Ideally it would probably be better to define our own mode instead
-- of using 'CardanoMode' (because 'CardanoMode' is not Ariadne's
-- business), but life is hard.

module Ariadne.Wallet.Backend.Mode () where

import qualified Data.ByteString as BS

import Named ((!))
import Pos.Client.Txp.Addresses (MonadAddresses(..))
import Pos.Client.Txp.Balances (MonadBalances(..), getBalanceFromUtxo)
import Pos.Client.Txp.History
  (MonadTxHistory(..), getBlockHistoryDefault, getLocalHistoryDefault,
  saveTxDefault)
import Pos.Core (Address, IsBootstrapEraAddr(..))
import Pos.Core.Configuration (HasConfiguration)
import Pos.Core.NetworkMagic (NetworkMagic)
import Pos.Crypto (PublicKey, deterministicKeyGen)
import Pos.Launcher.Configuration (HasConfigurations)
import Pos.Txp.DB.Utxo (getFilteredUtxo)

import Ariadne.Cardano.Face (CardanoMode)
import Ariadne.Wallet.Cardano.Kernel.Bip32
  (DerivationPath(..), makePubKeyHdwAddressUsingPath)
import Ariadne.Wallet.Cardano.Kernel.Bip44
  (Bip44DerivationPath(..), encodeBip44DerivationPath)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (HdAddressChain(HdChainExternal))

instance HasConfiguration => MonadBalances CardanoMode where
    getOwnUtxos = getFilteredUtxo
    getBalance = getBalanceFromUtxo

instance HasConfigurations => MonadTxHistory CardanoMode where
    getBlockHistory = getBlockHistoryDefault
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance MonadAddresses CardanoMode where
    type AddrData CardanoMode = NetworkMagic -> IO Address
    getNewAddress nm f = liftIO (f nm)
    -- [AD-236] Do not assume bootstrap era.
    getFakeChangeAddress = pure . largestHDAddressBoot

-- | Like 'largestHDAddressBoot' from 'cardano-sl', but uses different
-- derivation scheme (BIP-44).
largestHDAddressBoot :: NetworkMagic -> Address
largestHDAddressBoot nm =
    -- We cheat here a little bit using the same PublicKey for root
    -- key and address key.
    makePubKeyHdwAddressUsingPath nm (IsBootstrapEraAddr True)
        (DerivationPath derPath)
        ! #root goodPk
        ! #address goodPk
  where
    derPath = encodeBip44DerivationPath
        Bip44DerivationPath
        { bip44AccountIndex = maxBound
        , bip44AddressChain = HdChainExternal
        , bip44AddressIndex = maxBound
        }
    goodPk :: PublicKey
    goodPk = fst $ deterministicKeyGen $ BS.replicate 32 0
