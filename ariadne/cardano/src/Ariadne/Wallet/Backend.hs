module Ariadne.Wallet.Backend
       ( WalletFace(..)
       , WalletPreface(..)
       , createWalletBackend
       ) where

import Control.Monad.Component (ComponentM)
import Control.Natural ((:~>)(..))
import qualified Data.ByteArray as ByteArray
import qualified Data.Text as T
import Data.Constraint (withDict)
import Pos.Core (sumCoins, unsafeIntegerToCoin)
import Pos.Crypto.Hashing (hashRaw)
import System.Wlog (logMessage, usingLoggerName)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Cardano.Face
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Logging (Logging)
import Ariadne.UX.PasswordManager
import Ariadne.Wallet.Backend.KeyStorage
import Ariadne.Wallet.Backend.Restore (restoreFromKeyFile, restoreWallet)
import Ariadne.Wallet.Backend.Tx
import Ariadne.Wallet.Backend.Util (allAccountIds)
import Ariadne.Wallet.Cardano.Kernel.Keystore
  (DeletePolicy(..), keystoreComponent)
import Ariadne.Wallet.Cardano.WalletLayer
  (PassiveWalletLayer(..), activeWalletLayerComponent,
  passiveWalletLayerComponent)
import Ariadne.Wallet.Cardano.WalletLayer.Kernel (passiveWalletLayerComponent)
import Ariadne.Wallet.Face

-- | This is what we create initially, before actually creating 'WalletFace'.
data WalletPreface = WalletPreface
    { wpBListener :: !BListenerHandle

    , wpMakeWallet :: !((Doc -> IO ()) -> WalletFace, IO ())
    , wpMakeWallet :: !(CardanoFace -> ((Doc -> IO ()) -> WalletFace, IO (), IO ()))
    }

createWalletBackend
    :: WalletConfig
    -> CardanoFace
    -> (WalletEvent -> IO ())
    -> GetPassword
    -> VoidPassword
    -> Logging
    -> ComponentM WalletPreface
createWalletBackend walletConfig cardanoFace sendWalletEvent getPass voidPass logging = do
    walletSelRef <- newIORef Nothing

    keystore <- keystoreComponent
        RemoveKeystoreIfEmpty
        (wcKeyfilePath walletConfig)
    (pwl, pw) <- passiveWalletLayerComponent
        (usingLoggerName "passive-wallet" ... logMessage)
        keystore
        (wcAcidDBPath walletConfig)
        (cardanoProtocolMagic cardanoFace)
    (awl, _) <- activeWalletLayerComponent pwl pw

    let refresh = refreshState pwl walletSelRef sendWalletEvent
        applyHook = const refresh <=< pwlApplyBlocks pwl
        rollbackHook = const refresh <=< pwlRollbackBlocks pwl

        bListenerHandle = BListenerHandle
            { bhOnApply = applyHook
            , bhOnRollback = rollbackHook
            }

        getPass' :: PasswordRequestType -> WalletId -> IO PassPhrase
        getPass' requestType = fmap makePassPhrase . (getPass RequestCurrentPassword requestType)

        getPassTemp :: IO PassPhrase
        getPassTemp = getPass' GetCached WalletIdTemporary

        getPassPhrase :: PasswordRequestType -> WalletReference -> IO PassPhrase
        getPassPhrase requestType = (getPass' requestType) . fromWalletRef

        makePassPhrase :: T.Text -> PassPhrase
        makePassPhrase = ByteArray.convert . hashRaw . encodeUtf8

        -- | Only catches SomeWalletPassExceptions to void the password in the
        -- Password Manager and then retrow the Exception
        voidWrongPass :: WalletReference -> IO a -> IO a
        voidWrongPass walletRef action = catch action doVoidPass
          where
            doVoidPass :: SomeWalletPassException -> IO a
            doVoidPass e = do
                voidPass $ fromWalletRef walletRef
                throwM e

        voidSelectionPass :: IO ()
        voidSelectionPass = voidPass WalletIdSelected

        -- | Ask Ui for a confirmation and wait for a result
        waitUiConfirm :: ConfirmationType -> IO Bool
        waitUiConfirm confirmType = do
            mVar <- newEmptyMVar
            sendWalletEvent $ WalletRequireConfirm mVar confirmType
            takeMVar mVar

        mkWallet = (mkWalletFace, initWalletAction)
        mkWallet cf@CardanoFace{..} = (mkWalletFace, initWalletAction, walletAction)
          where
            NT runCardanoMode = cardanoRunCardanoMode cardanoFace
            withDicts :: ((HasConfigurations, HasCompileInfo) => r) -> r
            withDicts r =
                withDict (cardanoConfigurations cardanoFace) $
                withDict (cardanoCompileInfo cardanoFace) $
                r
            mkWalletFace putCommandOutput =
                withDicts $ fix $ \this -> WalletFace
                { walletNewAddress =
                    newAddress pwl this walletSelRef (getPassPhrase GetCached) voidWrongPass
                , walletNewAccount = newAccount pwl this walletSelRef
                , walletNewWallet = newWallet pwl walletConfig this getPassTemp waitUiConfirm logging
                , walletRestore = restoreWallet pwl runCardanoMode getPassTemp
                , walletRestoreFromFile = restoreFromKeyFile pwl runCardanoMode
                , walletRename = renameSelection pwl this walletSelRef logging
                , walletRemove = removeSelection pwl this walletSelRef waitUiConfirm
                , walletRefreshState =
                    refreshState pwl walletSelRef sendWalletEvent
                , walletSelect = select pwl this walletSelRef voidSelectionPass
                , walletSend =
                    sendTx awl this cardanoFace walletSelRef putCommandOutput
                        (getPassPhrase GetCached) voidWrongPass waitUiConfirm
                , walletFee = \walletRef accRefs _ txOuts -> do
                    accountIds <- allAccountIds pwl walletSelRef walletRef accRefs
                    pwlEstimateFees pwl accountIds txOuts
                , walletChangePassword = do
                    changePassword pwl this walletSelRef
                        (makePassPhrase <$> getPass RequestNewPassword GetCached WalletIdTemporary)
                        (getPassPhrase GetPasswordFromUI)
                    mbWalletId <- fromWalletSelRef walletSelRef
                    whenJust mbWalletId voidPass
                , walletBalance = getBalance pwl walletSelRef
                , walletSumCoins = \amounts -> return $ unsafeIntegerToCoin $ sumCoins amounts
                }
            initWalletAction = refreshState pwl walletSelRef sendWalletEvent

            postInitAction = do
                checkUnknownKeys pwl waitUiConfirm
                checkWalletsWithoutSecretKey pwl waitUiConfirm

            walletAction = pwlCleanupDBPeriodically pwl
              (wcAcidDBPath walletConfig)
              (wcNumStoredArchives walletConfig)
              (wcDBCleanupPerion walletConfig)
              (usingLoggerName "acid-db" ... logMessage)


        walletPreface = WalletPreface
            { wpBListener = bListenerHandle
            , wpMakeWallet = mkWallet
            }
    return walletPreface

fromWalletRef :: WalletReference -> WalletId
fromWalletRef = \case
    WalletRefByUIindex wrd -> WalletIdByUiIndex wrd
    WalletRefSelection     -> WalletIdSelected
    WalletRefByHdRootId hid -> WalletIdByBackend $ show hid

fromWalletSelRef :: IORef (Maybe WalletSelection) -> IO (Maybe WalletId)
fromWalletSelRef walletSelRef = do
    mWalletSel <- readIORef walletSelRef
    case mWalletSel of
        Nothing -> return Nothing
        Just selection -> case selection of
            WSRoot hdrId -> return $ Just $ WalletIdByBackend $ show hdrId
            _ -> return Nothing

-- TODO: make 'append' and 'rewrite' modes for wallet acid-state database.
-- If running append mode (append wallets to existing database) it should be
-- prevalidated at first (no name and key duplicates).
