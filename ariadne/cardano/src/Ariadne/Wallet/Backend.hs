module Ariadne.Wallet.Backend
  ( WalletFace(..)
  , WalletPreface(..)
  , createWalletBackend
  ) where

import Universum

import Control.Monad.Component (ComponentM)
import Control.Natural ((:~>)(..))
import Data.Constraint (withDict)
import System.Wlog (logMessage, usingLoggerName)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Cardano.Face
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Wallet.Backend.KeyStorage
import Ariadne.Wallet.Backend.Restore
import Ariadne.Wallet.Backend.Tx
import Ariadne.Wallet.Cardano.Kernel.Keystore
  (DeletePolicy(..), keystoreComponent)
import Ariadne.Wallet.Cardano.WalletLayer (PassiveWalletLayer(..))
import Ariadne.Wallet.Cardano.WalletLayer.Kernel (passiveWalletLayerComponent)
import Ariadne.Wallet.Face

-- | This is what we create initially, before actually creating 'WalletFace'.
data WalletPreface = WalletPreface
    { wpBListener :: !BListenerHandle
    , wpMakeWallet :: !(CardanoFace -> ((Doc -> IO ()) -> WalletFace, IO ()))
    }

createWalletBackend ::
    WalletConfig -> (WalletEvent -> IO ()) -> ComponentM WalletPreface
createWalletBackend walletConfig sendWalletEvent = do
    walletSelRef <- newIORef Nothing

    keystore <- keystoreComponent
        RemoveKeystoreIfEmpty
        (wcKeyfilePath walletConfig)
    pwl <- passiveWalletLayerComponent
        (usingLoggerName "passive-wallet" ... logMessage)
        keystore
        (wcAcidDBPath walletConfig)

    let refresh = refreshState pwl walletSelRef sendWalletEvent
        applyHook = const refresh <=< pwlApplyBlocks pwl
        rollbackHook = const refresh <=< pwlRollbackBlocks pwl

        bListenerHandle = BListenerHandle
            { bhOnApply = applyHook
            , bhOnRollback = rollbackHook
            }

        mkWallet cf@CardanoFace{..} = (mkWalletFace, initWalletAction)
          where
            NT runCardanoMode = cardanoRunCardanoMode
            withDicts :: ((HasConfigurations, HasCompileInfo) => r) -> r
            withDicts r =
                withDict cardanoConfigurations $
                withDict cardanoCompileInfo $
                r
            mkWalletFace putCommandOutput =
                withDicts $ fix $ \this -> WalletFace
                { walletNewAddress = newAddress pwl this walletSelRef
                , walletNewAccount = newAccount pwl this walletSelRef
                , walletNewWallet = newWallet pwl walletConfig this
                , walletRestore = restoreWallet pwl this runCardanoMode
                , walletRestoreFromFile = restoreFromKeyFile pwl this runCardanoMode
                , walletRename = renameSelection pwl this walletSelRef
                , walletRemove = removeSelection pwl this walletSelRef
                , walletRefreshState =
                    refreshState pwl walletSelRef sendWalletEvent
                , walletSelect = select pwl this walletSelRef
                , walletSend =
                    sendTx pwl this cf walletSelRef putCommandOutput
                , walletGetSelection =
                    (,) <$> readIORef walletSelRef <*> pwlGetDBSnapshot pwl
                , walletBalance = getBalance pwl walletSelRef
                }
            initWalletAction =
                refreshState pwl walletSelRef sendWalletEvent

        walletPreface = WalletPreface
            { wpBListener = bListenerHandle
            , wpMakeWallet = mkWallet
            }
    return walletPreface

-- TODO: make 'append' and 'rewrite' modes for wallet acid-state database.
-- If running append mode (append wallets to existing database) it should be
-- prevalidated at first (no name and key duplicates).
