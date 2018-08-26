module Ariadne.Wallet.Backend.Util
       ( mkHasPass
       ) where

import Universum

import Control.Natural (type (~>))

import qualified Pos.Core as Core

import Ariadne.Cardano.Face (CardanoMode)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet (HasSpendingPassword(..))
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..))

mkHasPass
    :: (CardanoMode ~> IO)
    -> Bool
    -> IO HasSpendingPassword
mkHasPass runCardanoMode noSpendingPassword
    | noSpendingPassword = pure NoSpendingPassword
    | otherwise = HasSpendingPassword . InDb <$> runCardanoMode Core.getCurrentTimestamp
