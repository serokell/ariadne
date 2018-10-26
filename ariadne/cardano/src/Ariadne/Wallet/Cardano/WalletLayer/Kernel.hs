module Ariadne.Wallet.Cardano.WalletLayer.Kernel
       ( passiveWalletLayerComponent
       , passiveWalletLayerCustomDBComponent
       ) where

import qualified Universum.Unsafe as Unsafe (fromJust)

import Control.Monad.Component (ComponentM, buildComponent)
import Data.Acid (AcidState, closeAcidState, openLocalStateFrom)
import System.Wlog (Severity(Debug))

import Pos.Block.Types (Blund, Undo(..))
import Pos.Core (unsafeIntegerToCoin)
import Pos.Core.Chrono (OldestFirst(..))
import Pos.Crypto (ProtocolMagic, safeDeterministicKeyGen)

import qualified Ariadne.Wallet.Cardano.Kernel as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Accounts as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Actions as Actions
import qualified Ariadne.Wallet.Cardano.Kernel.Addresses as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read as HDRead
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore (lookup)
import qualified Ariadne.Wallet.Cardano.Kernel.Wallets as Kernel

import Ariadne.Wallet.Cardano.Kernel.Bip39 (mnemonicToSeedNoPassword)
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (DB, dbHdWallets, defDB)
import Ariadne.Wallet.Cardano.Kernel.DB.Resolved (ResolvedBlock)
import Ariadne.Wallet.Cardano.Kernel.Keystore (Keystore)
import Ariadne.Wallet.Cardano.Kernel.Types
  (AccountId(..), RawResolvedBlock(..), WalletId(..), fromRawResolvedBlock)
import Ariadne.Wallet.Cardano.WalletLayer.Types (PassiveWalletLayer(..))

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
passiveWalletLayerComponent
    :: forall n. (MonadIO n)
    => (Severity -> Text -> IO ())
    -> Keystore
    -> FilePath
    -> ProtocolMagic
    -> ComponentM (PassiveWalletLayer n)
passiveWalletLayerComponent logFunction keystore dbPath pm = do
    acidDB <- buildComponent "Wallet DB"
        (openLocalStateFrom dbPath defDB)
        closeAcidState
    (pwl, _pw) <- passiveWalletLayerCustomDBComponent logFunction keystore acidDB pm
    pure pwl

passiveWalletLayerCustomDBComponent
    :: forall n. (MonadIO n)
    => (Severity -> Text -> IO ())
    -> Keystore
    -> AcidState DB
    -> ProtocolMagic
    -> ComponentM (PassiveWalletLayer n, Kernel.PassiveWallet)
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
    pure $ (passiveWalletLayer w invoke, w)
  where
    passiveWalletLayer :: Kernel.PassiveWallet
                       -> (Actions.WalletAction Blund -> IO ())
                       -> PassiveWalletLayer n
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

            , pwlGetDBSnapshot         = liftIO $ Kernel.getWalletSnapshot wallet
            , pwlLookupKeystore        = \hdrId -> liftIO $
                Keystore.lookup (WalletIdHdSeq hdrId) (wallet ^. Kernel.walletKeystore)
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
