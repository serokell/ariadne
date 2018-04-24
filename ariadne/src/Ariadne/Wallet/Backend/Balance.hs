module Ariadne.Wallet.Backend.Balance (getBalance) where

import Universum

import Pos.Txp.Toil.Utxo (getTotalCoinsInUtxo)
import Pos.Launcher (HasConfigurations)
import Pos.Core (Address, Coin)
import Pos.DB (MonadDBRead)
import Pos.Txp.DB.Utxo (getFilteredUtxo)

getBalance :: (HasConfigurations, MonadDBRead m) => [Address] -> m Coin
getBalance = fmap getTotalCoinsInUtxo . getFilteredUtxo
