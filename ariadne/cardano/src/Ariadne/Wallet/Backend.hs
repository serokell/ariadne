module Ariadne.Wallet.Backend
  ( WalletFace(..)
  , WalletPreface(..)
  , createWalletBackend
  ) where

import Universum

import Control.Monad.Component (ComponentM, buildComponent)
import Data.Acid (closeAcidState, openLocalStateFrom, query)
import Data.Constraint (withDict)
import IiExtras ((:~>)(..))
import System.Wlog (logMessage, usingLoggerName)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Cardano.Face
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Wallet.Backend.KeyStorage
import Ariadne.Wallet.Backend.Restore
import Ariadne.Wallet.Backend.Tx
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (Snapshot(..), defDB)
import Ariadne.Wallet.Cardano.Kernel.Keystore
  (DeletePolicy(..), keystoreComponent)
import Ariadne.Wallet.Cardano.WalletLayer.Kernel
  (passiveWalletLayerWithDBComponent)
import Ariadne.Wallet.Cardano.WalletLayer.Types (PassiveWalletLayer(..))
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
    acidDb <- buildComponent "Wallet DB"
        (openLocalStateFrom walletAcidDbPathPlaceholder defDB)
        closeAcidState

    -- TODO: configurable path to keyfile
    keystore <- keystoreComponent RemoveKeystoreIfEmpty "secret-mainnet.key"
    pwl <- passiveWalletLayerWithDBComponent
        (usingLoggerName "passive-wallet" ... logMessage)
        keystore
        acidDb

    let refresh = refreshState pwl walletSelRef sendWalletEvent
        applyHook = const refresh <=< _pwlApplyBlocks pwl
        rollbackHook = const refresh <=< _pwlRollbackBlocks pwl

        bListenerHandle = BListenerHandle
            { bhOnApply = applyHook
            , bhOnRollback = rollbackHook
            }

        mkWallet cf@CardanoFace{..} = (mkWalletFace, initWalletAction)
          where
            Nat runCardanoMode = cardanoRunCardanoMode
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
                    sendTx acidDb this cf walletSelRef putCommandOutput
                , walletGetSelection =
                    (,) <$> readIORef walletSelRef <*> query acidDb Snapshot
                , walletBalance = getBalance pwl walletSelRef
                }
            initWalletAction =
                refreshState pwl walletSelRef sendWalletEvent

        walletPreface = WalletPreface
            { wpBListener = bListenerHandle
            , wpMakeWallet = mkWallet
            }
    return walletPreface

-- TODO: Make it configurable
walletAcidDbPathPlaceholder :: FilePath
walletAcidDbPathPlaceholder = ".wallet-db"

-- TODO: make 'append' and 'rewrite' modes for wallet acid-state database.
-- If running append mode (append wallets to existing database) it should be
-- prevalidated at first (no name and key duplicates).
