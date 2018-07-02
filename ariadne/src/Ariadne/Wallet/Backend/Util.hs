module Ariadne.Wallet.Backend.Util
       ( mkHasPass
       ) where

import Universum

import qualified Pos.Core as Core

import IiExtras (type (~>))
import Ariadne.Cardano.Face (CardanoMode)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet (HasSpendingPassword(..))
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..))

mkHasPass
    :: (CardanoMode ~> IO)
    -> Bool
    -> IO HasSpendingPassword
mkHasPass runCardanoMode noSpendingPassword =
  if noSpendingPassword then
    pure NoSpendingPassword
  else
    HasSpendingPassword . InDb <$> runCardanoMode Core.getCurrentTimestamp
