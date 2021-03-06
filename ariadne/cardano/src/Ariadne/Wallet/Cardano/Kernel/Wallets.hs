module Ariadne.Wallet.Cardano.Kernel.Wallets
       ( createHdWallet
       , updateHdWalletName
       , updateHdWalletPassPhrase
       , updateHdWalletAssurance
       , deleteHdWallet
       , removeKeysFromKeystore
       , HasNonemptyPassphrase(..)
       , mkHasPP
       , CreateWithAddress(..)
         -- * Errors
       , CreateWalletError(..)
       , PassPhraseUpdateError(..)
       -- * Internal & testing use only
       , createWalletHdSeq
       ) where

import qualified Data.Text.Buildable
import Formatting (bprint, build, formatToString, (%))
import qualified Formatting as F
import qualified Text.Show

import Data.Acid.Advanced (update')

import Pos.Core (Timestamp)
import Pos.Crypto
  (EncryptedSecretKey, PassPhrase, changeEncPassphrase, emptyPassphrase)

import Ariadne.Wallet.Cardano.Kernel.Accounts
  (CreateAccountError(..), createAccount)
import Ariadne.Wallet.Cardano.Kernel.Addresses
  (CreateAddressError(..), createAddress)
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
  (CreateHdWallet(..), DeleteHdRoot(..),
  UpdateHdWalletAssurance(..), UpdateHdWalletName(..))
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (AccountName(..), AssuranceLevel, HdAddressChain(..), HdRoot, WalletName)
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet as HD
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Create as HD
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Delete as HD
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..))
import Ariadne.Wallet.Cardano.Kernel.Internal
  (PassiveWallet, walletKeystore, wallets)
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx (UtxoByAccount)
import Ariadne.Wallet.Cardano.Kernel.Types (AccountId(..), WalletId(..))
import Ariadne.Wallet.Cardano.Kernel.Util (getCurrentTimestamp)

import Test.QuickCheck (Arbitrary(..))


data CreateWalletError =
      CreateWalletFailed HD.CreateHdRootError
      -- ^ When trying to create the 'Wallet', the DB operation failed.
    | CreateAccountFailed CreateAccountError
    | CreateAddressFailed CreateAddressError

instance Arbitrary CreateWalletError where
    arbitrary = error "Arbitrary CreateWalletError is not implemented"

instance Buildable CreateWalletError where
    build (CreateWalletFailed dbOperation) =
        bprint ("CreateWalletUnknownHdAccount " % F.build) dbOperation
    build (CreateAccountFailed dbOperation) =
        bprint ("CreateWalletAccountUnknown " % F.build) dbOperation
    build (CreateAddressFailed dbOperation) =
        bprint ("CreateWalletAddressUnknown " % F.build) dbOperation


instance Show CreateWalletError where
    show = formatToString build

instance Exception CreateWalletError

newtype HasNonemptyPassphrase = HasNonemptyPassphrase Bool

data CreateWithAddress
    = WithoutAddress
    | WithAddress !PassPhrase

mkHasPP :: PassPhrase -> HasNonemptyPassphrase
mkHasPP pp = HasNonemptyPassphrase $ pp /= emptyPassphrase

{-------------------------------------------------------------------------------
  Wallet Creation
-------------------------------------------------------------------------------}

-- | Creates a new HD 'Wallet'.
createHdWallet
    :: PassiveWallet
    -> EncryptedSecretKey
    -- ^ Wallet's encrypted secret key. Should be generated outside.
    -> HasNonemptyPassphrase
    -- ^ Whether the newly created wallet has a non-empty passphrase.
    -> CreateWithAddress
    -- ^ Whether we need to auto create an account and address for new wallet or not
    -> AssuranceLevel
    -- ^ The 'AssuranceLevel' for this wallet, namely after how many
    -- blocks each transaction is considered 'adopted'. This translates
    -- in the frontend with a different threshold for the confirmation
    -- range (@low@, @medium@, @high@).
    -> WalletName
    -- ^ The name for this wallet.
    -> UtxoByAccount
    -- ^ Initial utxo for the new wallet.
    -> IO (Either CreateWalletError HdRoot)
createHdWallet pw esk hasNonemptyPP createWithA assuranceLevel walletName utxoByAccount = do
    -- Note: upstream implementation inserted a new HdRoot into acid-state
    -- first, and then added the new key into the keystore. We do
    -- this in the reverse order because we want to maintain the
    -- invariant where having an HdRoot in the acid-state guarantees
    -- that the corresponding key is present in the keystore.
    --
    -- STEP 1: Insert the 'EncryptedSecretKey' into the 'Keystore'
    -- This may throw an IO exception, which we rewrap into Either
    -- in order to follow the general pattern of imported code.
    let hdrId = HD.eskToHdRootId esk
        walletId = WalletIdHdSeq hdrId
    res1 <- try $ Keystore.insert walletId esk (pw ^. walletKeystore)
    case res1 of
        Left Keystore.DuplicatedWalletKey ->
            pure $ Left $ CreateWalletFailed (HD.CreateHdRootExists hdrId)
        Right () -> do
            -- STEP 2: Atomically generate the wallet and the initial internal structure in
            -- an acid-state transaction.
            res2 <- createWalletHdSeq
                pw
                hasNonemptyPP
                walletName
                assuranceLevel
                esk
                utxoByAccount
            case res2 of
                Left e -> do
                    liftIO $ Keystore.delete walletId (pw ^. walletKeystore)
                    pure $ Left $ CreateWalletFailed e
                Right hdRoot -> do
                    case createWithA of
                        WithoutAddress         -> return (Right hdRoot)
                        WithAddress passphrase -> do
                            addressRes <- runExceptT $ addDefaultAddress pw walletId passphrase
                            case addressRes of
                                Left e   -> return $ Left e
                                Right () -> return (Right hdRoot)

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
                  -> UtxoByAccount
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

removeKeysFromKeystore :: PassiveWallet
                       -> [HD.HdRootId]
                       -> IO ()
removeKeysFromKeystore pw rootIds =
  let keystore = pw ^. walletKeystore
  in mapM_ (\rootId -> Keystore.delete (WalletIdHdSeq rootId) keystore) rootIds

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

data PassPhraseUpdateError
  = PassPhraseUpdateIncorrectPassPhrase
  | PassPhraseUpdateUnknownHdRoot HD.UnknownHdRoot
  deriving (Eq, Show)

instance Buildable PassPhraseUpdateError where
  build = \case
    PassPhraseUpdateIncorrectPassPhrase ->
      "Incorrect passphrase"
    PassPhraseUpdateUnknownHdRoot rootId ->
      bprint ("Unknown wallet: "%build) rootId

instance Exception PassPhraseUpdateError where
  displayException e = "An error occurred during password change: "
    <> formatToString build e

updateHdWalletPassPhrase
    :: PassiveWallet
    -> HD.HdRootId
    -> PassPhrase
    -> PassPhrase
    -> IO (Either PassPhraseUpdateError ())
updateHdWalletPassPhrase pw rootId newPassword oldPassword = do
  res <- Keystore.lookup (WalletIdHdSeq rootId) (pw ^. walletKeystore)
  case res of
    Nothing -> return $ Left $ PassPhraseUpdateUnknownHdRoot $ HD.UnknownHdRoot rootId
    Just key -> do
      mbNewEsk <- changeEncPassphrase oldPassword newPassword key
      case mbNewEsk of
        Nothing -> do
          return $ Left $ PassPhraseUpdateIncorrectPassPhrase
        Just newEsk -> do
          Keystore.update (WalletIdHdSeq rootId) newEsk (pw ^. walletKeystore)
          return $ Right ()

{-------------------------------------------------------------------------------
  Automatic creation of Wallet Account and Address
-------------------------------------------------------------------------------}

addDefaultAddress
    :: PassiveWallet
    -> WalletId
    -> PassPhrase
    -> ExceptT CreateWalletError IO ()
addDefaultAddress pw walletId passphrase = do
    account <- ExceptT
         $  first CreateAccountFailed
        <$> createAccount (AccountName "Unnamed account") walletId pw

    let accountId = AccountIdHdSeq $ account ^. HD.hdAccountId

    void $ ExceptT
         $ first CreateAddressFailed
        <$> createAddress passphrase accountId HdChainExternal pw
