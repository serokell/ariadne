module Ariadne.Wallet.Cardano.WalletLayer
    ( -- * Kernel
      passiveWalletLayerComponent
    , passiveWalletLayerCustomDBComponent
    -- * We re-export the types since we want all the dependencies
    -- in this module, other modules shouldn't be touched.
    , module Types
    ) where

import Ariadne.Wallet.Cardano.WalletLayer.Kernel
  (passiveWalletLayerComponent, passiveWalletLayerCustomDBComponent)
import Ariadne.Wallet.Cardano.WalletLayer.Types (PassiveWalletLayer)
import Ariadne.Wallet.Cardano.WalletLayer.Types as Types
