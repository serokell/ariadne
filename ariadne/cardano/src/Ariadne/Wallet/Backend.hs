module Ariadne.Wallet.Backend
  ( WalletFace(..)
  , WalletPreface(..)
  , createWalletBackend
  ) where

import Universum

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Component (ComponentM, buildComponent)
import Data.Acid (closeAcidState, openLocalStateFrom, query)
import Data.Constraint (withDict)
import IiExtras ((:~>)(..))
import Pos.Util.Future (newInitFuture)
import Pos.Util.UserSecret (UserSecret)
import System.Wlog (logMessage, usingLoggerName)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Cardano.Face
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Wallet.Backend.KeyStorage
import Ariadne.Wallet.Backend.Restore
import Ariadne.Wallet.Backend.Tx
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (Snapshot(..), defDB)
import Ariadne.Wallet.Cardano.WalletLayer.Kernel (passiveWalletLayerComponent)
import Ariadne.Wallet.Cardano.WalletLayer.Types (PassiveWalletLayer(..))
import Ariadne.Wallet.Face

-- | This is what we create initially, before actually creating 'WalletFace'.
data WalletPreface = WalletPreface
    { wpBListener :: !BListenerHandle
    , wpAddUserSecret :: !(TVar UserSecret -> IO ())
    , wpMakeWallet :: !(CardanoFace -> ((Doc -> IO ()) -> WalletFace, IO ()))
    }

createWalletBackend ::
    WalletConfig -> (WalletEvent -> IO ()) -> ComponentM WalletPreface
createWalletBackend walletConfig sendWalletEvent = do
    walletSelRef <- newIORef Nothing
    acidDb <- buildComponent "Wallet DB"
        (openLocalStateFrom walletAcidDbPathPlaceholder defDB)
        closeAcidState

    (us :: TVar UserSecret, addUs :: TVar UserSecret -> IO ()) <- newInitFuture "UserSecret"
    PassiveWalletLayer{..} <- passiveWalletLayerComponent
        (usingLoggerName "passive-wallet" ... logMessage) us acidDb
    let refresh = refreshState acidDb walletSelRef sendWalletEvent
        applyHook = const refresh <=< _pwlApplyBlocks
        rollbackHook = const refresh <=< _pwlRollbackBlocks

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
                { walletNewAddress =
                    newAddress acidDb this walletSelRef runCardanoMode
                , walletNewAccount = newAccount acidDb this walletSelRef
                , walletNewWallet = newWallet acidDb walletConfig this runCardanoMode
                , walletRestore = restoreWallet acidDb this runCardanoMode
                , walletRestoreFromFile = restoreFromKeyFile acidDb this runCardanoMode
                , walletRename = renameSelection acidDb this walletSelRef
                , walletRemove = removeSelection acidDb this walletSelRef runCardanoMode
                , walletRefreshState =
                    refreshState acidDb walletSelRef sendWalletEvent
                , walletSelect = select acidDb this walletSelRef
                , walletSend =
                    sendTx acidDb this cf walletSelRef putCommandOutput
                , walletGetSelection =
                    (,) <$> readIORef walletSelRef <*> query acidDb Snapshot
                , walletBalance = getBalance acidDb walletSelRef
                }
            initWalletAction =
                refreshState acidDb walletSelRef sendWalletEvent

        walletPreface = WalletPreface
            { wpBListener = bListenerHandle
            , wpAddUserSecret = addUs
            , wpMakeWallet = mkWallet
            }
    return walletPreface

-- TODO: Make it configurable
walletAcidDbPathPlaceholder :: FilePath
walletAcidDbPathPlaceholder = ".wallet-db"

-- TODO: make 'append' and 'rewrite' modes for wallet acid-state database.
-- If running append mode (append wallets to existing database) it should be
-- prevalidated at first (no name and key duplicates).
