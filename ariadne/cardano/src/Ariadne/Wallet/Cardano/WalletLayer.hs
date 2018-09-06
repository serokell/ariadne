module Ariadne.Wallet.Cardano.WalletLayer
    ( -- * Kernel
      passiveWalletLayerComponent
    , passiveWalletLayerWithDBComponent
    -- * We re-export the types since we want all the dependencies
    -- in this module, other modules shouldn't be touched.
    , module Types
    ) where

import Ariadne.Wallet.Cardano.WalletLayer.Kernel
  (passiveWalletLayerComponent, passiveWalletLayerWithDBComponent)
import Ariadne.Wallet.Cardano.WalletLayer.Types as Types
