{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module contains instances of some type classes necessary for wallet.
--
-- Ideally it would probably be better to define our own mode instead
-- of using 'CardanoMode' (because 'CardanoMode' is not Ariadne's
-- business), but life is hard.

module Ariadne.Wallet.Backend.Mode
       (
       ) where

import Universum

import Pos.Client.Txp.Addresses (MonadAddresses(..))
import Pos.Client.Txp.Balances (MonadBalances(..), getBalanceFromUtxo)
import Pos.Client.Txp.History
  (MonadTxHistory(..), getBlockHistoryDefault, getLocalHistoryDefault,
  saveTxDefault)
import Pos.Core (Address, largestHDAddressBoot)
import Pos.Core.Configuration (HasConfiguration)
import Pos.Launcher.Configuration (HasConfigurations)
import Pos.Txp.DB.Utxo (getFilteredUtxo)
import Pos.Util.CompileInfo (HasCompileInfo)

import Ariadne.Cardano.Face (CardanoMode)

instance HasConfiguration => MonadBalances CardanoMode where
    getOwnUtxos addrs = getFilteredUtxo addrs
    getBalance = getBalanceFromUtxo

instance (HasConfigurations, HasCompileInfo) => MonadTxHistory CardanoMode where
    getBlockHistory = getBlockHistoryDefault
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance (HasConfigurations, HasCompileInfo) =>
         MonadAddresses CardanoMode where
    type AddrData CardanoMode = Address
    getNewAddress = pure
    -- FIXME: do not assume bootstrap era.
    getFakeChangeAddress = pure largestHDAddressBoot
