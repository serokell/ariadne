module Ariadne.Wallet.Cardano.WalletLayer.Kernel
       ( passiveWalletLayerComponent
       , passiveWalletLayerCustomDBComponent
       , activeWalletLayerComponent
       , walletDBComponent
       ) where

import qualified Universum.Unsafe as Unsafe (fromJust)

import Control.Concurrent.Async (async, uninterruptibleCancel)
import Control.Monad.Component (ComponentM, buildComponent, buildComponent_)
import Data.Acid (AcidState, closeAcidState, openLocalStateFrom)
import System.Wlog (Severity(Debug), logMessage, usingLoggerName)
import Time (KnownDivRat, Rat, Second, Time(..), threadDelay)

import Pos.Block.Types (Blund, Undo(..))
import Pos.Core (unsafeIntegerToCoin)
import Pos.Core.Chrono (OldestFirst(..))
import Pos.Core.Txp (TxOut(..))
import Pos.Crypto (ProtocolMagic, safeDeterministicKeyGen)

import qualified Ariadne.Wallet.Cardano.Kernel as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Accounts as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Actions as Actions
import qualified Ariadne.Wallet.Cardano.Kernel.Addresses as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore (lookup)

import qualified Ariadne.Wallet.Cardano.Kernel.Restore as Kernel

import qualified Ariadne.Wallet.Cardano.Kernel.Wallets as Kernel

import Ariadne.Wallet.Cardano.Kernel.Bip39 (mnemonicToSeedNoPassword)
import Ariadne.Wallet.Cardano.Kernel.Consistency
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
  (DB, dbHdWallets, defDB)
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState (cleanupAcidState)

import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read as HDRead
import Ariadne.Wallet.Cardano.Kernel.DB.Resolved (ResolvedBlock)
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Spec.Read as Spec
import Ariadne.Wallet.Cardano.Kernel.FeeEstimate (cardanoFee, transactionInputs)
import Ariadne.Wallet.Cardano.Kernel.Keystore (Keystore)

import Ariadne.Wallet.Cardano.Kernel.Types
  (AccountId(..), RawResolvedBlock(..), WalletId(..), fromRawResolvedBlock)
import Ariadne.Wallet.Cardano.Kernel.Wallets (CreateWithAddress(..))
import Ariadne.Wallet.Cardano.WalletLayer.Types
  (ActiveWalletLayer(..), PassiveWalletLayer(..))

-- | Initialize Acid State database. Is used as a helper function
-- and is exported only for tests.
walletDBComponent :: FilePath -> ComponentM (AcidState Kernel.DB)
walletDBComponent dbPath =
  buildComponent "Wallet DB"
    (openLocalStateFrom dbPath defDB)
    closeAcidState

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
passiveWalletLayerComponent
    :: forall (unit :: Rat) m. (KnownDivRat unit Second, MonadIO m)
    => (Severity -> Text -> IO ())
    -> Keystore
    -> FilePath
    -> ProtocolMagic
    -> Time unit
    -> Int
    -> ComponentM (PassiveWalletLayer m, Kernel.PassiveWallet)
passiveWalletLayerComponent logFunction keystore dbPath pm cleanupPeriod storedArchives = do
    acidDB <- walletDBComponent dbPath
    _ <- buildComponent "Wallet DB cleaner"
           (async $ runPeriodically cleanupPeriod $ cleanupAcidState acidDB dbPath storedArchives
             (usingLoggerName "acid-db" ... logMessage)) uninterruptibleCancel
    passiveWalletLayerCustomDBComponent logFunction keystore acidDB pm

passiveWalletLayerCustomDBComponent
    :: forall m. MonadIO m
    => (Severity -> Text -> IO ())
    -> Keystore
    -> AcidState DB
    -> ProtocolMagic
    -> ComponentM (PassiveWalletLayer m, Kernel.PassiveWallet)
passiveWalletLayerCustomDBComponent logFunction keystore acidDB pm = do
    w <- Kernel.passiveWalletCustomDBComponent logFunction keystore acidDB pm

    -- Create the wallet worker and its communication endpoint `invoke`.
    invoke <-
        buildComponent "WalletWorker"
            ( Actions.forkWalletWorker $ Actions.WalletActionInterp
                { Actions.applyBlocks  =  \blunds ->
                   Kernel.applyBlocks w $
                       OldestFirst (mapMaybe blundToResolvedBlock (toList (getOldestFirst blunds)))
                , Actions.switchToFork = \_ _ -> logFunction Debug "<switchToFork>"
                , Actions.emit         = logFunction Debug
                }
            )
            (\invoke -> liftIO (invoke Actions.Shutdown))
    pure (passiveWalletLayer w invoke, w)
  where
    passiveWalletLayer :: Kernel.PassiveWallet
                       -> (Actions.WalletAction Blund -> IO ())
                       -> PassiveWalletLayer m
    passiveWalletLayer wallet invoke =
        PassiveWalletLayer
            { pwlCreateWallet          = liftIO ... Kernel.createHdWallet wallet

            , pwlGetWalletIds          = liftIO $ do
                snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                pure $ HDRead.readAllHdRoots (snapshot ^. dbHdWallets)
            , pwlGetWallet             = \hdrId -> liftIO $ do
                snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                pure $ HDRead.readHdRoot hdrId (snapshot ^. dbHdWallets)
            , pwlUpdateWalletName      = \hdrId walletName -> liftIO $
                Kernel.updateHdWalletName wallet hdrId walletName
            , pwlUpdateWalletAssurance = \hdrId assurance -> liftIO $
                Kernel.updateHdWalletAssurance wallet hdrId assurance
            , pwlUpdateWalletPassword = \hdrId newPassPhrase oldPassPhrase -> liftIO $
                Kernel.updateHdWalletPassPhrase wallet hdrId newPassPhrase oldPassPhrase

            , pwlGetAccountBalance = \accountId -> liftIO $ do
                snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                account <- either throwM pure $
                    HDRead.readHdAccount accountId (snapshot ^. dbHdWallets)
                pure $ HDRead.hdAccountBalance account

            , pwlGetRootBalance = \rootId -> liftIO $ do
                snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                -- Using the unsafe function is OK here, since the case where
                -- the invariant that the balance exceeds @maxCoin@ is broken
                -- is clearly a programmer mistake.
                pure $ unsafeIntegerToCoin $
                    HDRead.hdRootBalance rootId (snapshot ^. dbHdWallets)

            , pwlDeleteWallet          = \hdrId -> liftIO $
                Kernel.deleteHdWallet wallet hdrId

            , pwlRestoreWallet = \runCardanoMode rFrom walletName -> liftIO $ do
                wallets <- Kernel.restoreWallets rFrom walletName
                traverse_
                    (\(Kernel.WalletToRestore esk hasPP templateWalletName assurance) -> do
                        utxoByAccount <- runCardanoMode $ Kernel.collectUtxo esk
                        Kernel.createHdWallet
                            wallet
                            esk
                            hasPP
                            WithoutAddress
                            assurance
                            templateWalletName
                            utxoByAccount
                    )
                    wallets

            , pwlCreateAccount         = \hdrId accName -> liftIO $ do
                let walletId = WalletIdHdSeq hdrId
                Kernel.createAccount accName walletId wallet
            , pwlGetAccounts           = \hdrId -> liftIO $ do
                snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                pure $ HDRead.readAccountsByRootId hdrId (snapshot ^. dbHdWallets)
            , pwlGetAccount            = \hdAccId -> liftIO $ do
                snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                pure $ HDRead.readHdAccount hdAccId (snapshot ^. dbHdWallets)
            , pwlUpdateAccountName     = \hdAccId accName -> liftIO $
                bimap id snd <$> Kernel.updateAccount hdAccId accName wallet
            , pwlDeleteAccount         = \hdAccId -> liftIO $
                Kernel.deleteAccount hdAccId wallet

            , pwlCreateAddress         = \pp hdAccId hdAddrChain -> liftIO $ do
                let accId = AccountIdHdSeq hdAccId
                Kernel.createAddress pp accId hdAddrChain wallet
            , pwlGetAddresses          = \hdrId -> liftIO $ do
                snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                pure $ HDRead.readAddressesByRootId hdrId (snapshot ^. dbHdWallets)

            , pwlApplyBlocks           = liftIO . invoke . Actions.ApplyBlocks
            , pwlRollbackBlocks        = liftIO . invoke . Actions.RollbackBlocks
            , pwlCreateEncryptedKey = \pp mnemonic ->
                let seed     = mnemonicToSeedNoPassword $ unwords mnemonic
                    (_, esk) = safeDeterministicKeyGen seed pp
                in pure esk
            , pwlEstimateFees          = \hdAccIds txOuts -> do
                snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                let payees = ((,) <$> txOutAddress <*> txOutValue) <$> txOuts
                availableUtxo <- concatForM hdAccIds $ \hdAccId -> liftIO $ either throwM pure $
                    Spec.queryAccountAvailableUtxo hdAccId (snapshot ^. dbHdWallets)
                ins <- liftIO $ transactionInputs availableUtxo payees
                return $ cardanoFee ins (txOutValue <$> txOuts)
            , pwlGetDBSnapshot         = liftIO $ Kernel.getWalletSnapshot wallet
            , pwlLookupKeystore        = \hdrId -> liftIO $
                Keystore.lookup (WalletIdHdSeq hdrId) (wallet ^. Kernel.walletKeystore)
            , pwlRemoveUnknownKeys     = \rootIds -> liftIO $
                Kernel.removeKeysFromKeystore wallet rootIds
            , pwlGetUnknownKeys        = getUnknownKeys wallet keystore
            , pwlGetWalletsWithoutSecretKeys =  getUnknownWallets wallet keystore

            }

    -- The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
    -- by the invariants established in the 'Blund'.
    blundToResolvedBlock :: Blund -> Maybe ResolvedBlock
    blundToResolvedBlock (b,u)
        = rightToMaybe b <&> \mainBlock ->
            fromRawResolvedBlock
            $ UnsafeRawResolvedBlock mainBlock spentOutputs'
        where
            spentOutputs' = map (map Unsafe.fromJust) $ undoTx u

-- | Initialize the active wallet.
-- The active wallet is allowed to send transactions.
activeWalletLayerComponent
    :: forall m. (MonadIO m)
    => PassiveWalletLayer m
    -> Kernel.PassiveWallet
    -> ComponentM (ActiveWalletLayer m, Kernel.ActiveWallet)
activeWalletLayerComponent pwl pw = do
    aw <- Kernel.activeWalletComponent pw
    awl <- buildComponent_
        "ActiveWalletLayer"
        (pure $ activeWalletLayer aw)
    pure (awl, aw)
  where
    activeWalletLayer aw = ActiveWalletLayer
        { walletPassiveLayer = pwl

        , awlNewPending      = \hdAccId tx ->
            liftIO $ Kernel.newPending aw hdAccId tx
        }

-- | Helper function to run action periodically.
runPeriodically :: forall (unit :: Rat) m a. (KnownDivRat unit Second, MonadIO m)
  => Time unit -- ^ time between performing action
  -> m a       -- ^ action
  -> m ()
runPeriodically delay action = forever $ do
  _ <- action
  threadDelay delay
