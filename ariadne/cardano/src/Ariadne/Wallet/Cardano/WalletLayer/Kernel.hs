{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Ariadne.Wallet.Cardano.WalletLayer.Kernel
    ( passiveWalletLayerComponent
    , passiveWalletLayerWithDBComponent
    ) where

import Universum

import Control.Monad.Component (ComponentM, buildComponent)
import Data.Acid (AcidState)
import Data.Maybe (fromJust)
import System.Wlog (Severity(Debug))

import Pos.Block.Types (Blund, Undo(..))

import qualified Ariadne.Wallet.Cardano.Kernel as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Accounts as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Addresses as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Wallets as Kernel

import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (DB, dbHdWallets)
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read as HDRead
import Ariadne.Wallet.Cardano.Kernel.DB.Resolved (ResolvedBlock)
import Ariadne.Wallet.Cardano.Kernel.Keystore (Keystore)
import Ariadne.Wallet.Cardano.Kernel.Types
  (AccountId(..), RawResolvedBlock(..), WalletId(..), fromRawResolvedBlock)
import Ariadne.Wallet.Cardano.WalletLayer.Types (PassiveWalletLayer(..))

import Pos.Core.Chrono (OldestFirst(..))

import qualified Ariadne.Wallet.Cardano.Kernel.Actions as Actions

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
passiveWalletLayerComponent
    :: forall n. (MonadIO n)
    => (Severity -> Text -> IO ())
    -> Keystore
    -> ComponentM (PassiveWalletLayer n)
passiveWalletLayerComponent logFunction keystore = do
    acidDB <- Kernel.inMemoryDBComponent
    passiveWalletLayerWithDBComponent logFunction keystore acidDB

passiveWalletLayerWithDBComponent
    :: forall n. (MonadIO n)
    => (Severity -> Text -> IO ())
    -> Keystore
    -> AcidState DB
    -> ComponentM (PassiveWalletLayer n)
passiveWalletLayerWithDBComponent logFunction keystore acidDB = do
    w <- Kernel.passiveWalletWithDBComponent logFunction keystore acidDB

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
    pure $ passiveWalletLayer w invoke

  where
    passiveWalletLayer :: Kernel.PassiveWallet
                       -> (Actions.WalletAction Blund -> IO ())
                       -> PassiveWalletLayer n
    passiveWalletLayer wallet invoke =
        PassiveWalletLayer
            { _pwlCreateWallet   = liftIO ... Kernel.createHdWallet wallet

            , _pwlGetWalletIds   = liftIO $ do
                snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                pure $ HDRead.readAllHdRoots (snapshot ^. dbHdWallets)
            , _pwlGetWallet      = \hdrId -> liftIO $ do
                snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                pure $ HDRead.readHdRoot hdrId (snapshot ^. dbHdWallets)
            , _pwlUpdateWallet   = \hdrId assurance walletName -> liftIO $
                Kernel.updateHdWallet wallet hdrId assurance walletName
            , _pwlDeleteWallet   = \hdrId -> liftIO $
                Kernel.deleteHdWallet wallet hdrId

            , _pwlCreateAccount = \hdrId mbAccName -> liftIO $ do
                let walletId = WalletIdHdRnd hdrId
                Kernel.createAccount mbAccName walletId wallet
            , _pwlGetAccounts   = \hdrId -> liftIO $ do
                snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                pure $ HDRead.readAccountsByRootId hdrId (snapshot ^. dbHdWallets)
            , _pwlGetAccount    = \hdAccId -> liftIO $ do
                snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                pure $ HDRead.readHdAccount hdAccId (snapshot ^. dbHdWallets)
            , _pwlUpdateAccount  = \hdAccId accName -> liftIO $
                bimap id snd <$> Kernel.updateAccount hdAccId accName wallet
            , _pwlDeleteAccount  = \hdAccId -> liftIO $
                Kernel.deleteAccount hdAccId wallet

            , _pwlCreateAddress  = \pp hdAccId hdAddrChain -> liftIO $ do
                let accId = AccountIdHdRnd hdAccId
                Kernel.createAddress pp accId hdAddrChain wallet
            , _pwlGetAddresses   = \hdrId -> liftIO $ do
                snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                pure $ HDRead.readAddressesByRootId hdrId (snapshot ^. dbHdWallets)

            , _pwlApplyBlocks    = liftIO . invoke . Actions.ApplyBlocks
            , _pwlRollbackBlocks = liftIO . invoke . Actions.RollbackBlocks

            , _pwlGetDBSnapshot  = liftIO $ Kernel.getWalletSnapshot wallet
            }

    -- The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
    -- by the invariants established in the 'Blund'.
    blundToResolvedBlock :: Blund -> Maybe ResolvedBlock
    blundToResolvedBlock (b,u)
        = rightToJust b <&> \mainBlock ->
            fromRawResolvedBlock
            $ UnsafeRawResolvedBlock mainBlock spentOutputs'
        where
            spentOutputs' = map (map fromJust) $ undoTx u
            rightToJust   = either (const Nothing) Just
