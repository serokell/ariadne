-- | Part of backend which manages keys, wallets, accounts, addresses.

module Ariadne.Wallet.Backend.KeyStorage
       (
         -- * Commands/other functions
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
import qualified Data.Text as T
import qualified Data.Text.Buildable
import qualified Data.Vector as V (fromList, mapMaybe)
import Formatting (bprint, int, (%))
import Numeric.Natural (Natural)
import Serokell.Data.Memory.Units (Byte)

import Pos.Core (mkCoin, unsafeIntegerToCoin)
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
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx (PrefilteredUtxo)
import Ariadne.Wallet.Cardano.Kernel.Wallets (HasNonemptyPassphrase, WithAddress (..), mkHasPP)
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
  (hdrId, accList) <- second toList <$>
      resolveWalletRefThenRead walletSelRef walletRef walletDb readAccountsByRootId

  let accountName = fromMaybe (genAccountName accList) mbAccountName

  throwLeftIO $ void <$>
    pwlCreateAccount pwl hdrId accountName

  walletRefreshState
  where
    genAccountName :: [HdAccount] -> AccountName
    genAccountName accList =
      let namesVec = V.fromList $ map (unAccountName . view hdAccountName) accList
      in AccountName $ mkUntitled "Untitled account " namesVec

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
    -> Maybe WalletName
    -> Maybe Byte
    -> IO [Text]
newWallet pwl walletConfig face getPassTemp mbWalletName mbEntropySize = do
  pp <- getPassTemp
  let entropySize = fromMaybe (wcEntropySize walletConfig) mbEntropySize
  mnemonic <- generateMnemonic entropySize
  let seed = mnemonicToSeedNoPassword $ unwords $ Unsafe.init mnemonic
      (_, esk) = safeDeterministicKeyGen seed pp
  let withAA = WithAddress True
  mnemonic <$
    addWallet pwl face esk mbWalletName mempty (mkHasPP pp) withAA (Just pp) assurance
  where
    -- TODO(AD-251): allow selecting assurance.
    assurance = AssuranceLevelNormal

-- | Construct a wallet from given data and add it to the storage.
addWallet ::
       PassiveWalletLayer IO
    -> WalletFace
    -> EncryptedSecretKey
    -> Maybe WalletName
    -> Map HdAccountId PrefilteredUtxo
    -> HasNonemptyPassphrase
    -> WithAddress
    -> Maybe PassPhrase 
    -> AssuranceLevel
    -> IO ()
addWallet pwl WalletFace {..} esk mbWalletName utxoByAccount hasPP withA mPP assurance = do
  walletName <-
      fromMaybe
      (genWalletName <$> pwlGetDBSnapshot pwl)
      (pure <$> mbWalletName)

  throwLeftIO $ void <$>
    pwlCreateWallet pwl esk hasPP withA mPP assurance walletName utxoByAccount

  walletRefreshState
  where
    genWalletName :: DB -> WalletName
    genWalletName walletDb = do -- no monad
      let hdRoots = toList (walletDb ^. dbHdWallets . hdWalletsRoots)
          namesVec = V.fromList $ map (unWalletName . view hdRootName) hdRoots
      WalletName $ mkUntitled "Untitled wallet " namesVec

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
        WSRoot rootId -> do
          walletDb <- pwlGetDBSnapshot pwl
          -- Using the unsafe function is OK here, since the case where
          -- the invariant that the balance exceeds @maxCoin@ is broken
          -- is clearly a programmer mistake.
          pure $ unsafeIntegerToCoin $
            hdRootBalance rootId (walletDb ^. dbHdWallets)
        WSAccount accountId -> do
          walletDb <- pwlGetDBSnapshot pwl
          account <- either throwM pure $
            readHdAccount accountId (walletDb ^. dbHdWallets)
          pure $ hdAccountBalance account

removeSelection
  :: PassiveWalletLayer IO
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> IO ()
removeSelection pwl WalletFace{..} walletSelRef = do
  mWalletSel <- readIORef walletSelRef
  newSelection <- case mWalletSel of
    Nothing -> pure Nothing
    -- Throw "Nothing selected" here?
    Just selection -> case selection of
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
  -> Text
  -> IO ()
renameSelection pwl WalletFace{..} walletSelRef name = do
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> pass
    Just selection -> case selection of
      WSRoot hdrId ->
        throwLeftIO $ void <$> pwlUpdateWalletName pwl hdrId (WalletName name)

      WSAccount hdAccId ->
        throwLeftIO $ void <$> pwlUpdateAccountName pwl hdAccId (AccountName name)

  walletRefreshState

-- Helpers

throwLeftIO :: (Exception e) => IO (Either e a) -> IO a
throwLeftIO ioEith = ioEith >>= eitherToThrow

mkUntitled :: Text -> Vector Text -> Text
mkUntitled untitled namesVec = do -- no monad
  let untitledSuffixes = V.mapMaybe (T.stripPrefix $ untitled) namesVec
      numbers = V.mapMaybe ((readMaybe @Natural) . toString) untitledSuffixes
  if null untitledSuffixes || null numbers
      then untitled <> "0"
      else untitled <> (show $ (maximum numbers) + 1)
