module Ariadne.Wallet.Cardano.Kernel.Wallets (
      createHdWallet
    , updateHdWalletName
    , updateHdWalletAssurance
    , deleteHdWallet
    , HasNonemptyPassphrase(..)
    , mkHasPP
    , WithAddress (..)
      -- * Errors
    , CreateWalletError(..)
    -- * Internal & testing use only
    , createWalletHdSeq
    ) where


import qualified Data.Text.Buildable
import qualified Data.Text.Lazy.Builder as TB (fromText)
import Formatting (bprint, build, formatToString, (%))
import qualified Formatting as F
import qualified Text.Show

import Data.Acid.Advanced (update')

import Pos.Core (Timestamp)
import Pos.Crypto (EncryptedSecretKey, PassPhrase, emptyPassphrase)

import Ariadne.Wallet.Cardano.Kernel.Accounts (createAccount, CreateAccountError(..))
import Ariadne.Wallet.Cardano.Kernel.Addresses (createAddress, CreateAddressError (..))
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
  (CreateHdWallet(..), DeleteHdRoot(..), UpdateHdWalletAssurance(..),
  UpdateHdWalletName(..))
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (AccountName(..), AssuranceLevel, HdAddressChain(HdChainInternal), HdAccountId, HdRoot, WalletName)
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet as HD
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Create as HD
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Delete as HD
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..))
import Ariadne.Wallet.Cardano.Kernel.Internal
  (PassiveWallet, walletKeystore, wallets)
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx (PrefilteredUtxo)
import Ariadne.Wallet.Cardano.Kernel.Types (WalletId(..), AccountId(..))
import Ariadne.Wallet.Cardano.Kernel.Util (getCurrentTimestamp)

import Test.QuickCheck (Arbitrary(..))


data CreateWalletError =
      CreateWalletFailed HD.CreateHdRootError  
      -- ^ When trying to create the 'Wallet', the DB operation failed.
    | CreateAccountFailed CreateAccountError
    | CreateAddressFailed CreateAddressError
    | EmptyPassPhraseError


instance Arbitrary CreateWalletError where
    arbitrary = error "Arbitrary CreateWalletError is not implemented"

instance Buildable CreateWalletError where
    build (CreateWalletFailed dbOperation) =
        bprint ("CreateWalletUnknownHdAccount " % F.build) dbOperation
    build EmptyPassPhraseError = TB.fromText "EmptyPassPhraseError"
    build (CreateAccountFailed dbOperation) =
        bprint ("CreateWalletAcountUnknown " % F.build) dbOperation
    build (CreateAddressFailed dbOperation) =
        bprint ("CreateWalletAddressUnknown " % F.build) dbOperation


instance Show CreateWalletError where
    show = formatToString build

instance Exception CreateWalletError

newtype HasNonemptyPassphrase = HasNonemptyPassphrase Bool

newtype WithAddress = WithAddress Bool

mkHasPP :: PassPhrase -> HasNonemptyPassphrase
mkHasPP pp = HasNonemptyPassphrase $ pp /= emptyPassphrase

{-------------------------------------------------------------------------------
  Wallet Creation
-------------------------------------------------------------------------------}

-- | Creates a new HD 'Wallet'.
createHdWallet :: PassiveWallet
             -> EncryptedSecretKey
             -- ^ Wallet's encrypted secret key. Should be generated outside.
             -> HasNonemptyPassphrase
             -- ^ Whether the newly created wallet has a non-empty passphrase.
             -> WithAddress
             -- ^ Whether we need to auto create an account and address for new wallet
             -> Maybe PassPhrase
             -- ^ The passPhrase for create wallet account address
             -> AssuranceLevel
             -- ^ The 'AssuranceLevel' for this wallet, namely after how many
             -- blocks each transaction is considered 'adopted'. This translates
             -- in the frontend with a different threshold for the confirmation
             -- range (@low@, @medium@, @high@).
             -> WalletName
             -- ^ The name for this wallet.
             -> Map HdAccountId PrefilteredUtxo
             -- ^ Initial utxo for the new wallet.
             -> IO (Either CreateWalletError HdRoot)
createHdWallet pw esk hasNonemptyPP (WithAddress wA) mPP assuranceLevel walletName utxoByAccount = do
    -- STEP 1: Insert the 'EncryptedSecretKey' into the 'Keystore'
    let newRootId = HD.eskToHdRootId esk
        walletId = WalletIdHdSeq newRootId
    -- This may throw an IO exception.
    Keystore.insert walletId esk (pw ^. walletKeystore)
    -- STEP 2: Atomically generate the wallet and the initial internal structure in
    -- an acid-state transaction.
    res <- createWalletHdSeq
        pw
        hasNonemptyPP
        walletName
        assuranceLevel
        esk
        utxoByAccount
    case res of
        Left e -> do
            Keystore.delete walletId (pw ^. walletKeystore)
            return . Left $ CreateWalletFailed e
        Right hdRoot -> do
            if not wA then 
                return (Right hdRoot) 
            else case mPP of 
                Just passphrase -> addAddress pw walletId passphrase hdRoot
                Nothing -> return . Left $ EmptyPassPhraseError



-- | Creates an HD wallet where new accounts and addresses are generated
-- via sequential index derivation.
--
-- Prefilters the Utxo before passing it to the Acidstate update.
--
-- Adds an HdRoot and HdAccounts (which are discovered during prefiltering of utxo).
-- In the case of empty utxo, no HdAccounts are created.
-- Fails with CreateHdWalletError if the HdRootId already exists.
createWalletHdSeq :: PassiveWallet
                  -> HasNonemptyPassphrase
                  -- ^ Whether or not this wallet has a spending password set.
                  -> HD.WalletName
                  -> AssuranceLevel
                  -> EncryptedSecretKey
                  -> Map HdAccountId PrefilteredUtxo
                  -> IO (Either HD.CreateHdRootError HdRoot)
createWalletHdSeq pw (HasNonemptyPassphrase hasPP) name assuranceLevel esk utxoByAccount = do
    created <- InDb <$> getCurrentTimestamp
    let rootId  = HD.eskToHdRootId esk
        newRoot = HD.initHdRoot rootId
                                name
                                (hdSpendingPassword created)
                                assuranceLevel
                                created

    -- mempty means we're using autogenerated account names
    res <- update' (pw ^. wallets) $ CreateHdWallet newRoot utxoByAccount mempty
    return $ case res of
                 Left err -> Left err
                 Right () -> Right newRoot
    where

        hdSpendingPassword :: InDb Timestamp -> HD.HasSpendingPassword
        hdSpendingPassword created =
            if hasPP then HD.HasSpendingPassword created
                     else HD.NoSpendingPassword

deleteHdWallet :: PassiveWallet
               -> HD.HdRootId
               -> IO (Either HD.DeleteHdRootError ())
deleteHdWallet pw rootId = do
    -- STEP 1: Remove the HdRoot via an acid-state transaction which will
    --         also delete any associated accounts and addresses.
    res <- update' (pw ^. wallets) $ DeleteHdRoot rootId
    case res of
        Left err -> return (Left err)
        Right () -> do
            -- STEP 2: Purge the key from the keystore.
            Keystore.delete (WalletIdHdSeq rootId) (pw ^. walletKeystore)
            return $ Right ()

{-------------------------------------------------------------------------------
  Wallet update
-------------------------------------------------------------------------------}

updateHdWalletName
    :: PassiveWallet
    -> HD.HdRootId
    -> HD.WalletName
    -> IO (Either HD.UnknownHdRoot HdRoot)
updateHdWalletName pw hdRootId walletName =
    update' (pw ^. wallets) (UpdateHdWalletName hdRootId walletName)

updateHdWalletAssurance
    :: PassiveWallet
    -> HD.HdRootId
    -> HD.AssuranceLevel
    -> IO (Either HD.UnknownHdRoot HdRoot)
updateHdWalletAssurance pw hdRootId assuranceLevel =
    update' (pw ^. wallets) (UpdateHdWalletAssurance hdRootId assuranceLevel)

{-------------------------------------------------------------------------------
  Automatic creation of Wallet Account and Address
-------------------------------------------------------------------------------}

addAddress 
    :: PassiveWallet
    -> WalletId
    -> PassPhrase
    -> HdRoot
    -> IO (Either CreateWalletError HdRoot)
addAddress pw walletId passphrase hdRoot = do 
    accountRes <- createAccount (AccountName "un-named") walletId pw
    case accountRes of 
        Left err      -> return . Left $ CreateAccountFailed err
        Right account -> do
            let accountId = AccountIdHdSeq $ account ^. HD.hdAccountId
            addressRes <- createAddress passphrase accountId HdChainInternal pw
            case addressRes of
                Left err -> return . Left $ CreateAddressFailed err
                Right _  -> return (Right hdRoot)
    
