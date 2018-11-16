module Ariadne.Wallet.Backend
       ( WalletFace(..)
       , WalletPreface(..)
       , createWalletBackend
       ) where

import Control.Monad.Component (ComponentM)
import Control.Natural ((:~>)(..))
import qualified Data.ByteArray as ByteArray
import Data.Constraint (withDict)
import Pos.Crypto.Hashing (hashRaw)
import System.Wlog (logMessage, usingLoggerName)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Cardano.Face
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.UX.PasswordManager
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

createWalletBackend
    :: WalletConfig
    -> (WalletEvent -> IO ())
    -> GetPassword
    -> VoidPassword
    -> ComponentM WalletPreface
createWalletBackend walletConfig sendWalletEvent getPass voidPass = do
    keystore <- keystoreComponent
        RemoveKeystoreIfEmpty
        (wcKeyfilePath walletConfig)
    pwl <- passiveWalletLayerComponent
        (usingLoggerName "passive-wallet" ... logMessage)
        keystore
        (wcAcidDBPath walletConfig)

    let refresh = refreshState pwl sendWalletEvent
        applyHook = const refresh <=< pwlApplyBlocks pwl
        rollbackHook = const refresh <=< pwlRollbackBlocks pwl

        bListenerHandle = BListenerHandle
            { bhOnApply = applyHook
            , bhOnRollback = rollbackHook
            }

        getPass' :: WalletId -> IO PassPhrase
        getPass' = fmap (ByteArray.convert . hashRaw . encodeUtf8) . getPass

        getPassTemp :: IO PassPhrase
        getPassTemp = getPass' WalletIdTemporary

        getPassPhrase :: WalletReference -> IO PassPhrase
        getPassPhrase = getPass' . fromWalletRef

        -- | Only catches SomeWalletPassExceptions to void the password in the
        -- Password Manager and then retrow the Exception
        voidWrongPass :: WalletReference -> IO a -> IO a
        voidWrongPass walletRef action = catch action doVoidPass
          where
            doVoidPass :: SomeWalletPassException -> IO a
            doVoidPass e = do
                voidPass $ fromWalletRef walletRef
                throwM e

        -- | Ask Ui for a confirmation and wait for a result
        waitUiConfirm :: ConfirmationType -> IO Bool
        waitUiConfirm confirmType = do
            mVar <- newEmptyMVar
            sendWalletEvent $ WalletRequireConfirm mVar confirmType
            takeMVar mVar

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
                { walletNewAddress =
                    newAddress pwl this getPassPhrase voidWrongPass
                , walletNewAccount = newAccount pwl this
                , walletNewWallet = newWallet pwl walletConfig this getPassTemp waitUiConfirm
                , walletRestore = restoreWallet pwl this runCardanoMode getPassTemp
                , walletRestoreFromFile = restoreFromKeyFile pwl this runCardanoMode
                , walletRenameAccount = renameAccount pwl this
                , walletRenameWallet = renameWallet pwl this
                , walletRemoveAccount = removeAccount pwl this waitUiConfirm
                , walletRemoveWallet = removeWallet pwl this waitUiConfirm
                , walletRefreshState = refreshState pwl sendWalletEvent
                , walletSend =
                    sendTx pwl this cf putCommandOutput getPassPhrase voidWrongPass waitUiConfirm
                , walletAccountBalance = getAccountBalance pwl
                , walletWalletBalance = getWalletBalance pwl
                }
            initWalletAction =
                refreshState pwl sendWalletEvent

        walletPreface = WalletPreface
            { wpBListener = bListenerHandle
            , wpMakeWallet = mkWallet
            }
    return walletPreface

fromWalletRef :: WalletReference -> WalletId
fromWalletRef = \case
    WalletRefByUIindex wrd -> WalletIdByUiIndex wrd
    WalletRefByHdRootId hid -> WalletIdByBackend $ show hid

-- TODO: make 'append' and 'rewrite' modes for wallet acid-state database.
-- If running append mode (append wallets to existing database) it should be
-- prevalidated at first (no name and key duplicates).
