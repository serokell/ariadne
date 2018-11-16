-- | Part of backend which manages keys, wallets, accounts, addresses.

module Ariadne.Wallet.Backend.KeyStorage
       ( -- * Commands/other functions
         refreshState
       , resolveWalletRef
       , resolveWalletRefThenRead
       , newAddress
       , newAccount
       , newWallet
       , addWallet
       , getAccountBalance
       , getWalletBalance
       , renameAccount
       , renameWallet
       , removeAccount
       , removeWallet
       , deriveBip44KeyPair

         -- * Exceptions
       , NoWalletSelection (..)
       , NoAccountSelection (..)

         -- * Util
       , generateMnemonic
       ) where

import qualified Universum.Unsafe as Unsafe

import Control.Exception (Exception(displayException))
import Control.Lens (ix)
import qualified Data.Text.Buildable
import Formatting (bprint, int, (%))
import Serokell.Data.Memory.Units (Byte)

-- import Pos.Core (mkCoin)
import Pos.Crypto
import Pos.Crypto.Random (secureRandomBS)
import Pos.Util (eitherToThrow, maybeThrow)

import Ariadne.Cardano.Face
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Wallet.Cardano.Kernel.Bip39
  (entropyToMnemonic, mnemonicToSeedNoPassword)
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Derivation (deriveBip44KeyPair)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx (UtxoByAccount)
import Ariadne.Wallet.Cardano.Kernel.Wallets
  (CreateWithAddress(..), HasNonemptyPassphrase, mkHasPP)
import Ariadne.Wallet.Cardano.WalletLayer (PassiveWalletLayer(..))
import Ariadne.Wallet.Face

data NoWalletSelection = NoWalletSelection
  deriving (Eq, Show)

instance Exception NoWalletSelection where
  displayException NoWalletSelection =
    "Select or specify a wallet to perform this operation."

data NoAccountSelection = NoAccountSelection
  deriving (Eq, Show)

instance Exception NoAccountSelection where
  displayException NoAccountSelection =
    "Select or specify an account to perform this operation."

data WalletDoesNotExist = WalletDoesNotExist WalletName
  deriving (Eq, Show)

instance Exception WalletDoesNotExist where
  displayException (WalletDoesNotExist (WalletName t)) =
    "The wallet " ++ show t ++ " does not exist."

data AccountDoesNotExist = AccountDoesNotExist Text
  deriving (Eq, Show)

instance Exception AccountDoesNotExist where
  displayException (AccountDoesNotExist t) =
    "The account " ++ show t ++ " does not exist."

data AddressDoesNotExist = AddressDoesNotExist Text
  deriving (Eq, Show)

instance Exception AddressDoesNotExist where
  displayException (AddressDoesNotExist t) =
    "The address " ++ show t ++ " does not exist."

data SelectIsTooDeep = SelectIsTooDeep
  deriving (Eq, Show)

instance Exception SelectIsTooDeep where
  displayException SelectIsTooDeep =
    "The selection path is too deep"

data WalletGenerationFailed = WGFailedUnconfirmedMnemonic
  deriving (Eq, Show)

instance Exception WalletGenerationFailed where
  displayException WGFailedUnconfirmedMnemonic =
    "Wallet mnemonic was not confirmed"

data AddressGenerationFailed = AGFailedIncorrectPassPhrase
  deriving (Eq, Show)

instance Exception AddressGenerationFailed where
    toException e    = case e of
        AGFailedIncorrectPassPhrase -> walletPassExceptionToException e
    fromException    = walletPassExceptionFromException
    displayException = \case
        AGFailedIncorrectPassPhrase ->
            "Address generation failed due to incorrect passphrase"

data WalletIndexOutOfRange = WalletIndexOutOfRange Word
  deriving (Eq, Show)

instance Exception WalletIndexOutOfRange where
  displayException (WalletIndexOutOfRange i) =
   "The wallet index " ++ show i ++ " is out of range."

data AccountIndexOutOfRange = AccountIndexOutOfRange Word
  deriving (Eq, Show)

instance Exception AccountIndexOutOfRange where
  displayException (AccountIndexOutOfRange i) =
   "The account index " ++ show i ++ " is out of range."

data RemoveFailed = RemoveFailedUnconfirmed
  deriving (Eq, Show)

instance Exception RemoveFailed where
  displayException RemoveFailedUnconfirmed =
    "Removal was not confirmed"

-- | Get the wallet HdRootId by ID, UI index or using current selection.
resolveWalletRef
  :: WalletReference
  -> DB
  -> IO HdRootId
resolveWalletRef walRef db = case walRef of
  WalletRefByUIindex i -> do
    -- Note: Add/remove wallets cause changes in indexation
    case walletList ^? ix (fromIntegral i) of
      Just hdRoot -> return (hdRoot ^. hdRootId)
      Nothing -> throwM $ WalletIndexOutOfRange i
  WalletRefByHdRootId hrR -> case readHdRoot hrR hdWallets of
    Right _ -> return hrR
    Left err -> throwM err

  where
    hdWallets :: HdWallets
    hdWallets = db ^. dbHdWallets

    walletList :: [HdRoot]
    walletList = toList (readAllHdRoots hdWallets)

-- | Like 'resolveWalletRef', but also reads some data from 'DB'
-- corresponding to the resolved root ID, assuming that the wallet
-- with this ID definitely exists.
resolveWalletRefThenRead
  :: WalletReference
  -> DB
  -> (HdRootId -> HdQueryErr UnknownHdRoot a)
  -> IO (HdRootId, a)
resolveWalletRefThenRead walRef db q = do
    rootId <- resolveWalletRef walRef db
    (rootId,) <$> case q rootId (db ^. dbHdWallets) of
        -- This function's assumption is that it must not happen.
        Left err -> error $ "resolveWalletRefThenRead: " <> pretty err
        Right res -> return res

resolveAccountRef
  :: AccountReference
  -> DB
  -> IO HdAccountId
resolveAccountRef accountRef walletDb = case accountRef of
    AccountRefByHdAccountId accId -> do
      checkAccId accId
      checkParentRoot accId
      return accId
    AccountRefByUIindex accIdx walRef -> do
      accounts <- toList . snd <$>
        resolveWalletRefThenRead walRef walletDb readAccountsByRootId
      acc <- maybeThrow
        (AccountIndexOutOfRange accIdx)
        (accounts ^? ix (fromIntegral accIdx))
      let accId = acc ^. hdAccountId
      checkParentRoot accId
      return accId
  where
    hdWallets = walletDb ^. dbHdWallets

    checkAccId :: HdAccountId -> IO ()
    checkAccId accId =
      void $ eitherToThrow $ readHdAccount accId hdWallets

    checkParentRoot :: HdAccountId -> IO ()
    checkParentRoot accId =
      void $ eitherToThrow $ readHdRoot (accId ^. hdAccountIdParent) hdWallets

refreshState
  :: PassiveWalletLayer IO
  -> (WalletEvent -> IO ())
  -> IO ()
refreshState pwl sendWalletEvent = do
  walletDb <- pwlGetDBSnapshot pwl
  sendWalletEvent (WalletStateSetEvent walletDb)

newAddress ::
       PassiveWalletLayer IO
    -> WalletFace
    -> (WalletReference -> IO PassPhrase)
    -> (WalletReference -> IO HdAddress -> IO HdAddress)
    -> AccountReference
    -> HdAddressChain
    -> IO Address
newAddress pwl WalletFace {..} getPassPhrase voidWrongPass accRef hdAddrChain = do
  let walletRef = case accRef of
          AccountRefByHdAccountId (HdAccountId hdRtId _) -> WalletRefByHdRootId hdRtId
          AccountRefByUIindex _ wRef -> wRef
  pp <- getPassPhrase walletRef
  walletDb <- pwlGetDBSnapshot pwl
  hdAccId <- resolveAccountRef accRef walletDb

  hdAddr <- voidWrongPass walletRef . throwLeftIO $
    pwlCreateAddress pwl pp hdAccId hdAddrChain

  (hdAddr ^. hdAddressAddress . fromDb) <$ walletRefreshState

newAccount
  :: PassiveWalletLayer IO
  -> WalletFace
  -> WalletReference
  -> Maybe AccountName
  -> IO ()
newAccount pwl WalletFace{..} walletRef mbAccountName = do
  walletDb <- pwlGetDBSnapshot pwl
  hdrId <- resolveWalletRef walletRef walletDb

  let accountName = fromMaybe (AccountName "Untitled account") mbAccountName

  throwLeftIO $ void <$>
    pwlCreateAccount pwl hdrId accountName

  walletRefreshState


data InvalidEntropySize =
    InvalidEntropySize !Byte
    deriving (Show)

instance Buildable InvalidEntropySize where
    build (InvalidEntropySize sz) =
        bprint ("Invalid size of entropy: "%int%" bytes") sz

instance Exception InvalidEntropySize where
    displayException = toString . pretty

generateMnemonic :: Byte -> IO [Text]
generateMnemonic entropySize = do
  unless (entropySize `elem` [16, 20, 24, 28, 32]) $
      throwM $ InvalidEntropySize entropySize
  mnemonic <- entropyToMnemonic <$> secureRandomBS (fromIntegral entropySize)
  return $ mnemonic ++ ["ariadne-v0"]

-- | Generate a mnemonic and a wallet from this mnemonic and add the
-- wallet to the storage.
newWallet ::
       PassiveWalletLayer IO
    -> WalletConfig
    -> WalletFace
    -> IO PassPhrase
    -> (ConfirmationType -> IO Bool)
    -> Bool
    -> Maybe WalletName
    -> Maybe Byte
    -> IO [Text]
newWallet pwl walletConfig face getPassTemp waitUiConfirm noConfirm mbWalletName mbEntropySize = do
  pp <- getPassTemp
  let entropySize = fromMaybe (wcEntropySize walletConfig) mbEntropySize
  mnemonic <- generateMnemonic entropySize
  unless noConfirm $
    unlessM (waitUiConfirm $ ConfirmMnemonic mnemonic) $
      throwM WGFailedUnconfirmedMnemonic
  let seed = mnemonicToSeedNoPassword $ unwords $ Unsafe.init mnemonic
      (_, esk) = safeDeterministicKeyGen seed pp
  mnemonic <$
    addWallet pwl face esk mbWalletName mempty (mkHasPP pp) (WithAddress pp) assurance
  where
    -- TODO(AD-251): allow selecting assurance.
    assurance = AssuranceLevelNormal

-- | Construct a wallet from given data and add it to the storage.
addWallet ::
       PassiveWalletLayer IO
    -> WalletFace
    -> EncryptedSecretKey
    -> Maybe WalletName
    -> UtxoByAccount
    -> HasNonemptyPassphrase
    -> CreateWithAddress
    -> AssuranceLevel
    -> IO ()
addWallet pwl WalletFace {..} esk mbWalletName utxoByAccount hasPP createWithA assurance = do
  let walletName = fromMaybe (WalletName "Untitled wallet") mbWalletName

  throwLeftIO $ void <$>
    pwlCreateWallet pwl esk hasPP createWithA assurance walletName utxoByAccount

  walletRefreshState

getAccountBalance
  :: PassiveWalletLayer IO
  -> AccountReference
  -> IO Coin
getAccountBalance = error "not implemented" {- do
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> return $ mkCoin 0
    Just selection ->
      case selection of
        WSRoot rootId -> pwlGetRootBalance pwl rootId
        WSAccount accountId -> pwlGetAccountBalance pwl accountId -}

getWalletBalance
  :: PassiveWalletLayer IO
  -> WalletReference
  -> IO Coin
getWalletBalance = error "not implemented" {- do
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> return $ mkCoin 0
    Just selection ->
      case selection of
        WSRoot rootId -> pwlGetRootBalance pwl rootId
        WSAccount accountId -> pwlGetAccountBalance pwl accountId -}

removeAccount
  :: PassiveWalletLayer IO
  -> WalletFace
  -> (ConfirmationType -> IO Bool)
  -> AccountReference
  -> Bool
  -> IO ()
removeAccount = error "not implemented" {- pwl WalletFace{..} walletSelRef waitUiConfirm noConfirm = do
  mWalletSel <- readIORef walletSelRef
  newSelection <- case mWalletSel of
    Nothing -> pure Nothing
    -- Throw "Nothing selected" here?
    Just selection -> do
      confirmationType <- getConfirmationRemoveType pwl selection
      unless noConfirm $
        unlessM (waitUiConfirm confirmationType) $
          throwM RemoveFailedUnconfirmed
      case selection of
        WSRoot hdrId -> do
          throwLeftIO $ void <$> pwlDeleteWallet pwl hdrId
          return Nothing
        WSAccount hdAccId -> do
          throwLeftIO $ void <$> pwlDeleteAccount pwl hdAccId
          return $ Just $ WSRoot (hdAccId ^. hdAccountIdParent)
  atomicWriteIORef walletSelRef newSelection
  walletRefreshState-}

removeWallet
  :: PassiveWalletLayer IO
  -> WalletFace
  -> (ConfirmationType -> IO Bool)
  -> WalletReference
  -> Bool
  -> IO ()
removeWallet = error "not implemented" {- pwl WalletFace{..} walletSelRef waitUiConfirm noConfirm = do
  mWalletSel <- readIORef walletSelRef
  newSelection <- case mWalletSel of
    Nothing -> pure Nothing
    -- Throw "Nothing selected" here?
    Just selection -> do
      confirmationType <- getConfirmationRemoveType pwl selection
      unless noConfirm $
        unlessM (waitUiConfirm confirmationType) $
          throwM RemoveFailedUnconfirmed
      case selection of
        WSRoot hdrId -> do
          throwLeftIO $ void <$> pwlDeleteWallet pwl hdrId
          return Nothing
        WSAccount hdAccId -> do
          throwLeftIO $ void <$> pwlDeleteAccount pwl hdAccId
          return $ Just $ WSRoot (hdAccId ^. hdAccountIdParent)
  atomicWriteIORef walletSelRef newSelection
  walletRefreshState -}

renameAccount
  :: PassiveWalletLayer IO
  -> WalletFace
  -> AccountReference
  -> Text
  -> IO ()
renameAccount = error "not implemented" {- pwl WalletFace{..} walletSelRef name = do
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> pass
    Just selection -> case selection of
      WSRoot hdrId ->
        throwLeftIO $ void <$> pwlUpdateWalletName pwl hdrId (WalletName name)

      WSAccount hdAccId ->
        throwLeftIO $ void <$> pwlUpdateAccountName pwl hdAccId (AccountName name)

  walletRefreshState -}

renameWallet
  :: PassiveWalletLayer IO
  -> WalletFace
  -> WalletReference
  -> Text
  -> IO ()
renameWallet = error "not implemented" {- pwl WalletFace{..} walletSelRef name = do
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> pass
    Just selection -> case selection of
      WSRoot hdrId ->
        throwLeftIO $ void <$> pwlUpdateWalletName pwl hdrId (WalletName name)

      WSAccount hdAccId ->
        throwLeftIO $ void <$> pwlUpdateAccountName pwl hdAccId (AccountName name)

  walletRefreshState -}

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

{-
getConfirmationRemoveType
  :: PassiveWalletLayer IO
  -> WalletSelection
  -> IO ConfirmationType
getConfirmationRemoveType pwl = \case
    WSRoot hdrId ->
          throwLeftIO (pwlGetWallet pwl hdrId)
      <&> ConfrimRemoveWallet . view hdRootName

    WSAccount hdAccId ->
          throwLeftIO (pwlGetAccount pwl hdAccId)
      <&> ConfirmRemoveAccount . view hdAccountName
-}

throwLeftIO :: (Exception e) => IO (Either e a) -> IO a
throwLeftIO ioEith = ioEith >>= eitherToThrow
