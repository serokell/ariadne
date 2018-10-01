module Ariadne.Wallet.Cardano.WalletLayer
    ( -- * Kernel
      passiveWalletLayerComponent
    , passiveWalletLayerInMemoryDBComponent
    -- * We re-export the types since we want all the dependencies
    -- in this module, other modules shouldn't be touched.
    , module Types
    -- * Tests use the bracket interface throughout
    , bracketKernelPassiveWallet
    ) where

import Control.Monad.Component (runComponentM)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import System.Wlog (Severity)

import Ariadne.Wallet.Cardano.Kernel (PassiveWallet)
import Ariadne.Wallet.Cardano.Kernel.Keystore (Keystore)
import Ariadne.Wallet.Cardano.WalletLayer.Kernel
  (passiveWalletLayerComponent, passiveWalletLayerInMemoryDBComponent)
import Ariadne.Wallet.Cardano.WalletLayer.Types (PassiveWalletLayer)
import Ariadne.Wallet.Cardano.WalletLayer.Types as Types

bracketKernelPassiveWallet
    :: forall m n a. (MonadIO m, MonadUnliftIO n)
    => (Severity -> Text -> IO ())
    -> Keystore
    -> (PassiveWalletLayer m -> PassiveWallet -> n a) -> n a
bracketKernelPassiveWallet logFunction keystore f =
    withRunInIO $ \runInIO ->
        runComponentM "Passive wallet layer (in-memory DB)"
            (passiveWalletLayerInMemoryDBComponent logFunction keystore)
            (\(pwl, pw) -> runInIO $ f pwl pw)
