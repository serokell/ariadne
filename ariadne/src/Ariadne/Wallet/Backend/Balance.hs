module Ariadne.Wallet.Backend.Balance (getBalance) where

import Universum

import UnliftIO (MonadUnliftIO)

import Pos.Core (Address, Coin)
import Pos.DB (MonadDBRead)
import Pos.Launcher (HasConfigurations)
import Pos.Txp.DB.Utxo (getFilteredUtxo)
import Pos.Txp.Toil.Utxo (getTotalCoinsInUtxo)

getBalance ::
       (HasConfigurations, MonadDBRead m, MonadUnliftIO m)
    => [Address]
    -> m Coin
getBalance = fmap getTotalCoinsInUtxo . getFilteredUtxo
