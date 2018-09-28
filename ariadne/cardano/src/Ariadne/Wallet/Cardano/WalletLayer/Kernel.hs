{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Ariadne.Wallet.Cardano.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import Universum

import Data.Default (def)
import Data.Maybe (fromJust)
import Data.Time.Units (Second)
import System.Wlog (Severity(Debug))

import Pos.Chain.Block (Blund, Undo(..))

import qualified Ariadne.Wallet.Cardano.Kernel as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Transactions as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Wallets as Kernel
import qualified Ariadne.Wallet.Cardano.WalletLayer.Kernel.Accounts as Accounts
import qualified Ariadne.Wallet.Cardano.WalletLayer.Kernel.Addresses as Addresses
import qualified Ariadne.Wallet.Cardano.WalletLayer.Kernel.Wallets as Wallets

import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet as HD
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..))
import Ariadne.Wallet.Cardano.Kernel.DB.Resolved (ResolvedBlock)
import Ariadne.Wallet.Cardano.Kernel.Diffusion (WalletDiffusion(..))
import Ariadne.Wallet.Cardano.Kernel.Keystore (Keystore)
import Ariadne.Wallet.Cardano.Kernel.Types
  (RawResolvedBlock(..), fromRawResolvedBlock)
import Ariadne.Wallet.Cardano.WalletLayer.ExecutionTimeLimit
  (limitExecutionTimeTo)
import Ariadne.Wallet.Cardano.WalletLayer.Types
  (ActiveWalletLayer(..), EstimateFeesError(..), NewPaymentError(..),
  PassiveWalletLayer(..), WalletLayerError(..))

import Ariadne.Wallet.Cardano.Kernel.CoinSelection.FromGeneric
  (CoinSelectionOptions(..), ExpenseRegulation, InputGrouping, newOptions)

import qualified Ariadne.Wallet.Cardano.Kernel.BIP39 as BIP39
import Pos.Core (Address, Coin)
import qualified Pos.Core as Core
import Pos.Core.Chrono (OldestFirst(..))

import qualified Ariadne.Wallet.Cardano.Kernel.Actions as Actions
import Ariadne.Wallet.Cardano.Kernel.MonadDBReadAdaptor (MonadDBReadAdaptor)
import Pos.Crypto.Signing

import Ariadne.Wallet.Cardano.API.V1.Types
  (Payment(..), PaymentDistribution(..), PaymentSource(..), WalletId(..), unV1)

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (MonadIO n, MonadIO m, MonadMask m)
    => (Severity -> Text -> IO ())
    -> Keystore
    -> MonadDBReadAdaptor IO
    -> (PassiveWalletLayer n -> Kernel.PassiveWallet -> m a) -> m a
bracketPassiveWallet logFunction keystore rocksDB f =
    Kernel.bracketPassiveWallet logFunction keystore rocksDB $ \w -> do

      -- Create the wallet worker and its communication endpoint `invoke`.
      bracket (liftIO $ Actions.forkWalletWorker $ Actions.WalletActionInterp
                 { Actions.applyBlocks  =  \blunds ->
                     Kernel.applyBlocks w $
                         OldestFirst (mapMaybe blundToResolvedBlock (toList (getOldestFirst blunds)))
                 , Actions.switchToFork = \_ _ -> logFunction Debug "<switchToFork>"
                 , Actions.emit         = logFunction Debug
                 }
              ) (\invoke -> liftIO (invoke Actions.Shutdown))
              $ \invoke -> do
                  -- TODO (temporary): build a sample wallet from a backup phrase
                  _ <- liftIO $ do
                    Kernel.createHdWallet w
                                          (def @(BIP39.Mnemonic 12))
                                          emptyPassphrase
                                          assuranceLevel
                                          walletName

                  f (passiveWalletLayer w invoke) w

  where
    -- TODO consider defaults
    walletName       = HD.WalletName "(new wallet)"
    assuranceLevel   = HD.AssuranceLevelNormal

    -- | TODO(ks): Currently not implemented!
    passiveWalletLayer :: Kernel.PassiveWallet
                       -> (Actions.WalletAction Blund -> IO ())
                       -> PassiveWalletLayer n
    passiveWalletLayer wallet invoke =
        PassiveWalletLayer
            { _pwlCreateWallet   = Wallets.createWallet wallet

            , _pwlGetWalletIds   = error "Not implemented!"
            , _pwlGetWallet      = error "Not implemented!"
            , _pwlUpdateWallet   = error "Not implemented!"
            , _pwlDeleteWallet   = error "Not implemented!"

            , _pwlCreateAccount = Accounts.createAccount wallet
            , _pwlGetAccounts   =
                \walletId -> do
                    snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                    Accounts.getAccounts snapshot walletId
            , _pwlGetAccount    =
                \walletId accountIndex -> do
                    snapshot <- liftIO (Kernel.getWalletSnapshot wallet)
                    Accounts.getAccount snapshot walletId accountIndex
            , _pwlUpdateAccount  = Accounts.updateAccount wallet
            , _pwlDeleteAccount  = Accounts.deleteAccount wallet

            , _pwlCreateAddress  = Addresses.createAddress wallet
            , _pwlGetAddresses   = error "Not implemented!"

            , _pwlApplyBlocks    = liftIO . invoke . Actions.ApplyBlocks
            , _pwlRollbackBlocks = liftIO . invoke . Actions.RollbackBlocks
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

-- | Initialize the active wallet.
-- The active wallet is allowed to send transactions, as it has the full
-- 'WalletDiffusion' layer in scope.
bracketActiveWallet
    :: forall m n a. (MonadIO m, MonadMask m, MonadIO n)
    => Core.ProtocolMagic
    -> PassiveWalletLayer n
    -> Kernel.PassiveWallet
    -> WalletDiffusion
    -> (ActiveWalletLayer n -> Kernel.ActiveWallet -> m a) -> m a
bracketActiveWallet pm walletPassiveLayer passiveWallet walletDiffusion runActiveLayer =
    Kernel.bracketActiveWallet pm passiveWallet walletDiffusion $ \activeWallet -> do
        bracket
          (return (activeWalletLayer activeWallet))
          (\_ -> return ())
          (flip runActiveLayer activeWallet)
  where

    activeWalletLayer :: Kernel.ActiveWallet -> ActiveWalletLayer n
    activeWalletLayer activeWallet = ActiveWalletLayer {
          walletPassiveLayer = walletPassiveLayer

        -- | Generates a new transaction @and submit it as pending@.
        , pay = \spendingPassword grouping regulation payment -> do
              liftIO $ limitExecutionTimeTo (60 :: Second) NewPaymentTimeLimitReached $ do
                  (opts, accountId, payees) <-
                       liftIO $ setupPayment grouping
                                             regulation
                                             payment
                  res <- liftIO $ Kernel.pay activeWallet
                                             spendingPassword
                                             opts
                                             accountId
                                             payees
                  case res of
                       Left e   -> return . Left . NewPaymentError $ e
                       Right tx -> return . Right $ tx

        -- | Estimates the fees for a payment.
        , estimateFees = \spendingPassword grouping regulation payment -> do
              liftIO $ limitExecutionTimeTo (60 :: Second) EstimateFeesTimeLimitReached $ do
                  (opts, accountId, payees) <-
                      liftIO $ setupPayment grouping
                                            regulation
                                            payment
                  fees <- liftIO $ Kernel.estimateFees activeWallet
                                                       spendingPassword
                                                       opts
                                                       accountId
                                                       payees
                  case fees of
                       Left e  -> return . Left  . EstimateFeesError $ e
                       Right f -> return . Right $ f
        }


-- | Internal function setup to facilitate the creation of the necessary
-- context to perform either a new payment or the estimation of the fees.
setupPayment :: InputGrouping
             -> ExpenseRegulation
             -> Payment
             -> IO ( CoinSelectionOptions
                   , HD.HdAccountId
                   , NonEmpty (Address, Coin)
                   )
setupPayment grouping regulation payment = do

    let (WalletId wId) = psWalletId . pmtSource $ payment

    hdRootId  <- case Core.decodeTextAddress wId of
                     Left e  -> throwM (InvalidAddressConversionFailed e)
                     Right a -> return (HD.HdRootId . InDb $ a)
    let opts = (newOptions Kernel.cardanoFee) {
               csoExpenseRegulation = regulation
             , csoInputGrouping     = grouping
             }
        accountIndex   = HD.HdAccountIx (psAccountIndex . pmtSource $ payment)
        accountId = HD.HdAccountId {
                    _hdAccountIdParent = hdRootId
                  , _hdAccountIdIx     = accountIndex
                  }
        payees    =  (\(PaymentDistribution a c) -> (unV1 a, unV1 c))
                 <$> (pmtDestinations payment)

    return (opts , accountId , payees)


