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
       , select
       , getBalance
       , renameSelection
       , removeSelection
       , checkUnknownKeys
       , checkWalletsWithoutSecretKey
       , changePassword
       , deriveBip44KeyPair

         -- * Exceptions
       , NoWalletSelection (..)
       , NoAccountSelection (..)

         -- * Util
       , generateMnemonic
       ) where
import qualified Universum.Unsafe as Unsafe (init)

import Control.Exception (Exception(displayException))
import Control.Lens (ix)

import qualified Data.Text as T
import qualified Data.Text.Buildable
import Formatting (bprint, int, sformat, (%))
import Serokell.Data.Memory.Units (Byte)

import Pos.Core (mkCoin)
import Pos.Crypto
import Pos.Crypto.Random (secureRandomBS)
import Pos.Util (eitherToThrow, maybeThrow)

import Ariadne.Cardano.Face
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Logging (Logging, logInfo)
import Ariadne.Wallet.Cardano.Kernel.Bip39 (entropyToMnemonic)
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
  :: IORef (Maybe WalletSelection)
  -> WalletReference
  -> DB
  -> IO HdRootId
resolveWalletRef walletSelRef walRef db = case walRef of
  WalletRefSelection -> do
    mWalletSelection <- readIORef walletSelRef
    case mWalletSelection of
      Nothing -> throwM NoWalletSelection
      Just selection -> return $ getHdRootId selection
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

    getHdRootId :: WalletSelection -> HdRootId
    getHdRootId (WSRoot rId) = rId
    getHdRootId (WSAccount acId) = acId ^. hdAccountIdParent

    walletList :: [HdRoot]
    walletList = toList (readAllHdRoots hdWallets)

-- | Like 'resolveWalletRef', but also reads some data from 'DB'
-- corresponding to the resolved root ID, assuming that the wallet
-- with this ID definitely exists.
resolveWalletRefThenRead
  :: IORef (Maybe WalletSelection)
  -> WalletReference
  -> DB
  -> (HdRootId -> HdQueryErr UnknownHdRoot a)
  -> IO (HdRootId, a)
resolveWalletRefThenRead walletSelRef walRef db q = do
    rootId <- resolveWalletRef walletSelRef walRef db
    (rootId,) <$> case q rootId (db ^. dbHdWallets) of
        -- This function's assumption is that it must not happen.
        Left err -> error $ "resolveWalletRefThenRead: " <> pretty err
        Right res -> return res

resolveAccountRef
  :: IORef (Maybe WalletSelection)
  -> AccountReference
  -> DB
  -> IO HdAccountId
resolveAccountRef walletSelRef accountRef walletDb = case accountRef of
    AccountRefSelection -> do
      mWalletSelection <- readIORef walletSelRef
      case mWalletSelection of
        Nothing -> throwM NoWalletSelection
        Just (WSRoot _) -> throwM NoAccountSelection
        Just (WSAccount accId) -> do
            checkParentRoot accId
            return accId
    AccountRefByHdAccountId accId -> do
      checkAccId accId
      checkParentRoot accId
      return accId
    AccountRefByUIindex accIdx walRef -> do
      accounts <- toList . snd <$>
        resolveWalletRefThenRead walletSelRef walRef walletDb readAccountsByRootId
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
  -> IORef (Maybe WalletSelection)
  -> (WalletEvent -> IO ())
  -> IO ()
refreshState pwl walletSelRef sendWalletEvent = do
  walletSel <- readIORef walletSelRef
  walletDb <- pwlGetDBSnapshot pwl
  sendWalletEvent (WalletStateSetEvent walletDb walletSel)

newAddress ::
       PassiveWalletLayer IO
    -> WalletFace
    -> IORef (Maybe WalletSelection)
    -> (WalletReference -> IO PassPhrase)
    -> (WalletReference -> IO HdAddress -> IO HdAddress)
    -> AccountReference
    -> HdAddressChain
    -> IO Address
newAddress pwl WalletFace {..} walletSelRef getPassPhrase voidWrongPass accRef hdAddrChain = do
  let walletRef = case accRef of
          AccountRefSelection -> WalletRefSelection
          AccountRefByHdAccountId (HdAccountId hdRtId _) -> WalletRefByHdRootId hdRtId
          AccountRefByUIindex _ wRef -> wRef
  pp <- getPassPhrase walletRef
  walletDb <- pwlGetDBSnapshot pwl
  hdAccId <- resolveAccountRef walletSelRef accRef walletDb
  hdAddr <- voidWrongPass walletRef . throwLeftIO $
    pwlCreateAddress pwl pp hdAccId hdAddrChain

  (hdAddr ^. hdAddressAddress . fromDb) <$ walletRefreshState

newAccount
  :: PassiveWalletLayer IO
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> WalletReference
  -> Maybe AccountName
  -> IO ()
newAccount pwl WalletFace{..} walletSelRef walletRef mbAccountName = do
  walletDb <- pwlGetDBSnapshot pwl
  hdrId <- resolveWalletRef walletSelRef walletRef walletDb

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
       HasCallStack
    => PassiveWalletLayer IO
    -> WalletConfig
    -> WalletFace
    -> IO PassPhrase
    -> (ConfirmationType -> IO Bool)
    -> Logging
    -> Bool
    -> Maybe WalletName
    -> Maybe Byte
    -> IO [Text]
newWallet pwl walletConfig face getPassTemp waitUiConfirm logging noConfirm mbWalletName mbEntropySize = do
  logInfo logging "Creating a new wallet…"
  pp <- getPassTemp
  let entropySize = fromMaybe (wcEntropySize walletConfig) mbEntropySize
  mnemonic <- generateMnemonic entropySize
  unless noConfirm $
    unlessM (waitUiConfirm $ ConfirmMnemonic mnemonic) $
      throwM WGFailedUnconfirmedMnemonic
  esk <- pwlCreateEncryptedKey pwl pp $ Unsafe.init mnemonic
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

-- | Convert path in index representation and write it to
-- 'IORef WalletSelection'.
select
  :: PassiveWalletLayer IO
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> IO ()
  -> Maybe WalletReference
  -> [Word]
  -> IO ()
select pwl WalletFace{..} walletSelRef voidSelectionPass mWalletRef uiPath = do
  walletDb <- pwlGetDBSnapshot pwl
  mbSelection <- case mWalletRef of
    Nothing -> return Nothing
    Just walletRef -> do
      -- Throw an exception if walletRef is invalid
      (rootId,accList) <- second toList <$>
        resolveWalletRefThenRead walletSelRef walletRef walletDb readAccountsByRootId
      case nonEmpty uiPath of
        Nothing -> return $ Just $ WSRoot rootId
        Just (accIdx :| acPath) -> do
          -- validate account
          hdAccount <- maybeThrow
            (AccountIndexOutOfRange accIdx)
            (accList ^? ix (fromIntegral accIdx))

          unless (null acPath) $ throwM SelectIsTooDeep
          return $ Just $ WSAccount $ hdAccount ^. hdAccountId

  voidSelectionPass
  atomicWriteIORef walletSelRef mbSelection
  walletRefreshState

getBalance
  :: PassiveWalletLayer IO
  -> IORef (Maybe WalletSelection)
  -> IO Coin
getBalance pwl walletSelRef = do
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> return $ mkCoin 0
    Just selection ->
      case selection of
        WSRoot rootId -> pwlGetRootBalance pwl rootId
        WSAccount accountId -> pwlGetAccountBalance pwl accountId

removeSelection
  :: PassiveWalletLayer IO
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> (ConfirmationType -> IO Bool)
  -> Bool
  -> IO ()
removeSelection pwl WalletFace{..} walletSelRef waitUiConfirm noConfirm = do
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
  walletRefreshState

renameSelection
  :: PassiveWalletLayer IO
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> Logging
  -> Text
  -> IO ()
renameSelection pwl WalletFace{..} walletSelRef logging name = do
  logInfo logging $ "Renaming selection to '" <> name <> "'…"
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> pass
    Just selection -> case selection of
      WSRoot hdrId ->
        throwLeftIO $ void <$> pwlUpdateWalletName pwl hdrId (WalletName name)

      WSAccount hdAccId ->
        throwLeftIO $ void <$> pwlUpdateAccountName pwl hdAccId (AccountName name)

  walletRefreshState

-- | This function removes unknown keys from key file on startup, if user confirms that.
checkUnknownKeys
  :: PassiveWalletLayer IO
  -> (ConfirmationType -> IO Bool)  -- ^ Asks user permission to delete unknown keys
  -> IO ()
checkUnknownKeys pwl confirmation = do
  unknownRootIds <- pwlGetUnknownKeys pwl
  unless (null unknownRootIds) $ do
    let unknownRootIdsText =
          T.dropEnd 1 . unlines $ sformat hashHexF . _fromDb . getHdRootId <$> unknownRootIds
    whenM (confirmation $ ConfirmDelUnknownKeys unknownRootIdsText) $
      pwlRemoveUnknownKeys pwl unknownRootIds

-- | This function removes wallets with missed secret keys, if user confirms that.
checkWalletsWithoutSecretKey
  :: PassiveWalletLayer IO
  -> (ConfirmationType -> IO Bool)  -- ^ Asks user permission to delete such wallets
  -> IO ()
checkWalletsWithoutSecretKey pwl confirmation = do
  (walletNamesWithMissedSecretKeys, rootIDsWithMissedSecretKeys) <- pwlGetWalletsWithoutSecretKeys pwl
  let walletsWithoutKeysText = unlines $ unWalletName <$> walletNamesWithMissedSecretKeys
  unless (null rootIDsWithMissedSecretKeys) $ do
    whenM (confirmation $ ConfirmDelBrokenWallets $ T.dropEnd 1 $ walletsWithoutKeysText) $
      mapM_ (pwlDeleteWallet pwl) rootIDsWithMissedSecretKeys

changePassword
  :: PassiveWalletLayer IO
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> IO PassPhrase
  -> (WalletReference -> IO PassPhrase)
  -> IO ()
changePassword pwl WalletFace{..} walletSelRef getPassTemp getPassPhrase = do
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> pass
    Just selection -> case selection of
        WSRoot hdrId -> do
          oldPassword <- getPassPhrase $ WalletRefByHdRootId hdrId
          newPassword <- getPassTemp
          throwLeftIO $ pwlUpdateWalletPassword pwl hdrId newPassword oldPassword
        _ -> pass
  walletRefreshState

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

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

throwLeftIO :: (Exception e) => IO (Either e a) -> IO a
throwLeftIO ioEith = ioEith >>= eitherToThrow
