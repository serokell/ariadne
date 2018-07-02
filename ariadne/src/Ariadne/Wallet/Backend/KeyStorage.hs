-- | Part of backend which manages keys, wallets, accounts, addresses.

module Ariadne.Wallet.Backend.KeyStorage
       (
         -- * Commands/other functions
         refreshState
       , resolveWalletRef
       , newAddress
       , newAccount
       , newWallet
       , addWallet
       , select
       , getSelectedAddresses
       , renameSelection
       , removeSelection
       , deriveBip44KeyPair

         -- * Exceptions
       , NoWalletSelection (..)
       , NoAccountSelection (..)
       ) where

import Universum

import Ariadne.Cardano.Face
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Wallet.Cardano.Kernel.Bip32
import Ariadne.Wallet.Cardano.Kernel.Bip44
  (Bip44DerivationPath(..), encodeBip44DerivationPath)
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (HdAccountIx(..), HdAddressChain(..), HdAddressIx(..))
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Spec
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet
import Ariadne.Wallet.Cardano.Kernel.Word31
import Ariadne.Wallet.Face

import Control.Exception (Exception(displayException))
import Control.Lens (ix, (%=))
import Control.Monad.Catch.Pure (Catch, CatchT, runCatchT)
import Data.Acid (AcidState, query, update)
import Data.Acid.Abstract (groupUpdates)
import Data.Map (findWithDefault)
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Buildable
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as V (foldr, foldr', fromList, mapMaybe, toList)
import Formatting (bprint, int, (%))
import IiExtras
import Loot.Crypto.Bip39 (entropyToMnemonic, mnemonicToSeed)
import Named ((!))
import Numeric.Natural (Natural)
import Pos.Client.KeyStorage (getSecretDefault, modifySecretDefault)
import Pos.Core (AddressHash)
import Pos.Core.Common (IsBootstrapEraAddr(..), addressHash)
import Pos.Crypto
import Pos.Util (eitherToThrow, maybeThrow)
import Pos.Util.UserSecret (usWallets)
import Serokell.Data.Memory.Units (Byte)

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
    displayException =
        \case
            AGFailedIncorrectPassPhrase ->
                "Address generation failed due to incorrect passphrase"

data DuplicatedWalletKey = DuplicatedWalletKey
  deriving (Eq, Show)

instance Exception DuplicatedWalletKey where
  displayException DuplicatedWalletKey =
    "The wallet with this root key already exists"

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

-- | Utility function that runs CatchT monad inside a StateT and rollbacks the state on failure
runCatchInState :: Functor m => StateT s (CatchT m) a -> StateT s m (Either SomeException a)
runCatchInState m = StateT $ \s ->
  runCatchT (runStateT m s) <&> \case
    Left e -> (Left e, s)
    Right (a, s') -> (Right a, s')

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
      accounts <- getAccList walRef
      acc <- maybeThrow
        (AccountIndexOutOfRange (fromIntegral accIdx))
        (accounts ^? ix (fromIntegral accIdx))
      let accId = acc ^. hdAccountId
      checkParentRoot accId
      return accId
  where
    hdWallets = walletDb ^. dbHdWallets
    getAccList walRef = do
      rootId <- resolveWalletRef walletSelRef walRef walletDb
      toList <$> (eitherToThrow $ readAccountsByRootId rootId hdWallets)

    checkAccId :: HdAccountId -> IO ()
    checkAccId accId =
      eitherToThrow $ readHdAccount accId hdWallets >> return ()

    checkParentRoot :: HdAccountId -> IO ()
    checkParentRoot accId =
      eitherToThrow $ readHdRoot (accId ^. hdAccountIdParent) hdWallets >> return ()

refreshState
  :: AcidState DB
  -> IORef (Maybe WalletSelection)
  -> (WalletEvent -> IO ())
  -> IO ()
refreshState acidDb walletSelRef sendWalletEvent = do
  walletSel <- readIORef walletSelRef
  walletDb <- query acidDb Snapshot
  sendWalletEvent (WalletStateSetEvent walletDb walletSel)

newAddress ::
       AcidState DB
    -> WalletFace
    -> IORef (Maybe WalletSelection)
    -> (CardanoMode ~> IO)
    -> AccountReference
    -> HdAddressChain
    -> PassPhrase
    -> IO ()
newAddress acidDb WalletFace {..} walletSelRef runCardanoMode accRef chain pp = do
  walletDb <- query acidDb Snapshot
  accountId <- resolveAccountRef walletSelRef accRef walletDb
  keysMap <- (^. usWallets) <$> runCardanoMode getSecretDefault

  let
    walletRootId = accountId ^. hdAccountIdParent
    pubAddrHash = _fromDb (unHdRootId walletRootId)
    addressId = HdAddressId
      { _hdAddressIdParent = accountId
      , _hdAddressIdChain = chain
      , _hdAddressIdIx = mkAddrIdx accountId walletDb
      }
    -- Wallets creation and deletion organized in a such way that
    -- an absence of a key is not possible.
    walletEsk = findWithDefault
      (error "Bug: _usWallets has no such key.")
      pubAddrHash
      keysMap
    bip44derPath = Bip44DerivationPath
      { bip44AccountIndex = accountId ^. hdAccountIdIx
      , bip44AddressChain = chain
      , bip44AddressIndex = addressId ^. hdAddressIdIx
      }

  addr <-
      case (deriveBip44KeyPair
                (IsBootstrapEraAddr True)
                pp
                walletEsk
                bip44derPath) of
          Nothing -> throwM AGFailedIncorrectPassPhrase
          Just (a, _) -> pure a
  let
    hdAddress = HdAddress
      { _hdAddressId = addressId
      , _hdAddressAddress = InDb addr
      , _hdAddressIsUsed = False
      , _hdAddressCheckpoints = one emptyAddrCheckpoint
      }
  throwLeftIO $ update acidDb (CreateHdAddress hdAddress)
  walletRefreshState
    where
      -- Using the sequential indexation as in accounts.
      -- TODO:
      -- * get random index with gap less than 20 (BIP-44)
      mkAddrIdx :: HdAccountId -> DB -> HdAddressIx
      mkAddrIdx accId walletDb = HdAddressIx $
        findFirstUnique
          (unsafeMkWord31 0)
          (addrIndexes accId (walletDb ^. dbHdWallets))

      addrIndexes :: HdAccountId -> HdWallets -> Vector Word31
      addrIndexes accId wallets =
        V.fromList (
          ( unHdAddressIx
          . _hdAddressIdIx
          . _hdAddressId) <$> (toList (getAddresses accId wallets)))

      getAddresses :: HdAccountId -> HdWallets -> IxSet HdAddress
      getAddresses accId wallets = fromRight
        (error "Bug: UnknownHdAccount")
        (readAddressesByAccountId accId wallets)


mkUntitled :: Text -> Vector Text -> Text
mkUntitled untitled namesVec =
  let
    untitledSuffixes = V.mapMaybe (T.stripPrefix $ untitled) namesVec
    numbers = V.mapMaybe ((readMaybe @Natural) . T.unpack) untitledSuffixes
  in
    if null untitledSuffixes || null numbers
      then untitled <> "0"
      else untitled <> (show $ (Universum.maximum numbers) + 1)

newAccount
  :: AcidState DB
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> Maybe AccCheckpoint
  -> WalletReference
  -> Maybe AccountName
  -> IO ()
newAccount acidDb WalletFace{..} walletSelRef mbCheckPoint walletRef mbAccountName = do
  walletDb <- query acidDb Snapshot
  rootId <- resolveWalletRef walletSelRef walletRef walletDb

  let
    wallets = walletDb ^. dbHdWallets
    namesVec = V.fromList
      (map (unAccountName . (^. hdAccountName)) $ toList $ getAccounts (walletDb ^. dbHdWallets) rootId)

  accountName <- case (unAccountName <$> mbAccountName) of
    Nothing ->
      return (mkUntitled "Untitled account " namesVec)
    Just accountName_ -> return accountName_

  let
    account = HdAccount
      { _hdAccountId = HdAccountId rootId (HdAccountIx $ accountIdx rootId wallets)
      , _hdAccountName = AccountName accountName
      , _hdAccountCheckpoints =
          one $ fromMaybe
            emptyAccCheckpoint
            mbCheckPoint
      }

  throwLeftIO $ update acidDb (CreateHdAccount account)

  walletRefreshState
  where
    accIndexes :: HdRootId -> HdWallets -> Vector Word31
    accIndexes rootId wallets =
      V.fromList (
        ( unHdAccountIx
        . _hdAccountIdIx
        . _hdAccountId) <$> (toList (getAccounts wallets rootId)))

    -- AFAIU account indexation should be sequential
    accountIdx rootId wallets = findFirstUnique (unsafeMkWord31 0) (accIndexes rootId wallets)

        -- TODO: make it global
    -- The same function as in `select`. It is unsafe -> not global.
    -- It is assumed that hdRootId is a valid one.
    -- It is, because resolveWalletRef always returns a valid HdRootId.
    getAccounts :: HdWallets -> HdRootId -> IxSet HdAccount
    getAccounts wallets rootId = fromRight
      (error "Bug: UnknownHdRoot")
      (readAccountsByRootId rootId wallets)

data InvalidEntropySize =
    InvalidEntropySize !Byte
    deriving (Show)

instance Buildable InvalidEntropySize where
    build (InvalidEntropySize sz) =
        bprint ("Invalid size of entropy: "%int%" bytes") sz

instance Exception InvalidEntropySize where
    displayException = toString . pretty

-- | Generate a mnemonic and a wallet from this mnemonic and add the
-- wallet to the storage.
newWallet ::
       AcidState DB
    -> WalletConfig
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> PassPhrase
    -> Maybe WalletName
    -> Maybe Byte
    -> IO [Text]
newWallet acidDb walletConfig face runCardanoMode pp mbWalletName mbEntropySize = do
  let entropySize = fromMaybe (wcEntropySize walletConfig) mbEntropySize
  unless (entropySize `elem` [16, 20, 24, 28, 32]) $
      throwM $ InvalidEntropySize entropySize
  entropy <- secureRandomBS (fromIntegral entropySize)
  let mnemonic = entropyToMnemonic entropy
  -- The empty string below is called a passphrase in BIP-39, but
  -- it's essentially an extra mnemonic word. We consider it an
  -- advanced feature and do not provide it for now.
  let seed = mnemonicToSeed (unwords mnemonic) ""
  let (_, esk) = safeDeterministicKeyGen seed pp
  mnemonic ++ ["ariadne-v0"] <$ addWallet acidDb face runCardanoMode esk mbWalletName mempty

-- | Construct a wallet from given data and add it to the storage.
addWallet ::
       AcidState DB
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> EncryptedSecretKey
    -> Maybe WalletName
    -> Vector HdAccount
    -> IO ()
addWallet acidDb WalletFace {..} runCardanoMode esk mbWalletName accounts = do
  let addWalletPure :: StateT UserSecret Catch ()
      addWalletPure = do
        eskList <- Map.elems <$> (use usWallets)
        let keysList = encToPublic <$> eskList
        when (encToPublic esk `elem` keysList) $
          throwM DuplicatedWalletKey
        usWallets %= Map.insert (addressHash $ encToPublic esk) esk
  runCardanoMode (modifySecretDefault (runCatchInState addWalletPure)) >>=
    eitherToThrow

  walletDb <- query acidDb Snapshot
  -- need to query ixSet to find if the name already in db
  let walletNamesList :: [WalletName]
      walletNamesList = (^. hdRootName) <$>
        toList (walletDb ^. dbHdWallets . hdWalletsRoots)
  let namesVec = V.fromList (map unWalletName $ walletNamesList)
  walletName <- case mbWalletName of
    Nothing ->
      return (WalletName $ mkUntitled "Untitled wallet " namesVec)
    Just walletName_ -> return walletName_

  -- getPOSIXTime return seconds with 10^-12 precision
  timestamp <- (InDb . round . (* 10 ^ (6 :: Integer)) <$> getPOSIXTime)
  let rootId = HdRootId $ InDb $ addressHash $ encToPublic esk

  -- FIXME: This should be passed to `addWallet` I guess.
  let hasPass = NoSpendingPassword
  let assurance = AssuranceLevelNormal

  let hdRoot = HdRoot
          { _hdRootId = rootId
          , _hdRootName = walletName
          , _hdRootHasPassword = hasPass
          , _hdRootAssurance = assurance
          , _hdRootCreatedAt = timestamp
          }
  throwLeftIO $ update acidDb (CreateHdRoot hdRoot)

  addAccounts rootId
  walletRefreshState
  where
    -- If any of accounts is invalid, all transaction fails.
    addAccounts :: HdRootId -> IO ()
    addAccounts rootId = do
      updateEvents <- V.toList <$> (forM accounts $ \acc -> do
        let accName = acc ^. hdAccountName . unAccountName
            parentRoot = acc ^. hdAccountId . hdAccountIdParent
        when (parentRoot /= rootId) $
          error "Bug: account's parent rootId does not match created wallet rootId"
        when (accName `S.member` namesSet) $
          throwM $ DuplicateAccountName (AccountName accName)
        return $ CreateHdAccount acc)
      groupUpdates acidDb updateEvents

    accNamesVec :: Vector Text
    accNamesVec = (_unAccountName . _hdAccountName) <$> accounts

    namesSet :: Set Text
    namesSet = V.foldr' S.insert S.empty accNamesVec

-- | Convert path in index representation and write it to
-- 'IORef WalletSelection'.
select
  :: AcidState DB
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> Maybe WalletReference
  -> [Word]
  -> IO ()
select acidDb WalletFace{..} walletSelRef mWalletRef uiPath = do
  walletDb <- query acidDb Snapshot
  let wallets = walletDb ^. dbHdWallets
  mbSelection <- case mWalletRef of
    Nothing -> return Nothing
    Just walletRef -> do
      -- Throw an exception if walletRef is invalid
      rootId <- resolveWalletRef walletSelRef walletRef walletDb
      case nonEmpty uiPath of
        Nothing -> return $ Just $ WSRoot rootId
        Just (accIdx :| acPath) -> do
          -- validate account
          hdAccount <- maybeThrow
            (AccountIndexOutOfRange accIdx)
            ((accList wallets rootId) ^? ix (fromIntegral accIdx))

          unless (null acPath) $ throwM SelectIsTooDeep
          return $ Just $ WSAccount $ hdAccount ^. hdAccountId

  atomicWriteIORef walletSelRef mbSelection
  walletRefreshState
  where
    -- TODO: make it global
    accList :: HdWallets -> HdRootId -> [HdAccount]
    accList wallets rootId = toList $ getAccounts wallets rootId

    getAccounts :: HdWallets -> HdRootId -> IxSet HdAccount
    getAccounts wallets rootId = fromRight
      (error "Bug: UnknownHdRoot")
      (readAccountsByRootId rootId wallets)

removeSelection
  :: AcidState DB
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> IO ()
removeSelection acidDb WalletFace{..} walletSelRef runCardanoMode = do
  mWalletSel <- readIORef walletSelRef
  newSelection <- case mWalletSel of
    Nothing -> pure Nothing
    -- Throw "Nothing selected" here?
    Just selection -> case selection of
      WSRoot hdrId -> do
        update acidDb (DeleteHdRoot hdrId)
        runCardanoMode $ modifySecretDefault (usWallets %= Map.delete (fromRootId hdrId))
        return Nothing
      WSAccount accId -> do
        throwLeftIO $ update acidDb (DeleteHdAccount accId)
        return $ Just $ WSRoot (accId ^. hdAccountIdParent)
  atomicWriteIORef walletSelRef newSelection
  walletRefreshState
  where
    fromRootId :: HdRootId -> (AddressHash PublicKey)
    fromRootId (HdRootId (InDb x)) = x


renameSelection
  :: AcidState DB
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> Text
  -> IO ()
renameSelection acidDb WalletFace{..} walletSelRef name = do
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> pure ()
    Just selection -> case selection of
      WSRoot hdrId ->
        throwLeftIO $ update acidDb (UpdateHdRootName hdrId (WalletName name))

      WSAccount accId ->
        throwLeftIO $ update acidDb (UpdateHdAccountName accId (AccountName name))

  walletRefreshState

-- Helpers

throwLeftIO :: (Exception e) => IO (Either e a) -> IO a
throwLeftIO ioEith = ioEith >>= eitherToThrow

findFirstUnique :: (Ord a, Enum a, Bounded a) => a -> Vector a -> a
findFirstUnique lastIdx paths = head
    . fromMaybe (error "Can't find a unique path!")
    . nonEmpty
    $ dropWhile (`S.member` pathsSet) [lastIdx..maxBound]
  where
    pathsSet = V.foldr S.insert S.empty paths

-- | This function derives a 3-level address using account index, change index
--   and address index. The input key should be the master key (not the key
--   that was derived from purpose and coin type)
deriveBip44KeyPair ::
       IsBootstrapEraAddr
    -> PassPhrase
    -> EncryptedSecretKey
    -> Bip44DerivationPath
    -> Maybe (Address, EncryptedSecretKey)
deriveBip44KeyPair era pp rootSK bip44DerPath =
    toPair <$>
    deriveHDSecretKeyByPath (ShouldCheckPassphrase True) pp rootSK derPath
  where
    derPath :: [Word32]
    derPath = encodeBip44DerivationPath bip44DerPath
    toPair :: EncryptedSecretKey -> (Address, EncryptedSecretKey)
    toPair addrSK =
        ( makePubKeyHdwAddressUsingPath
              era
              derPath
              ! #root (encToPublic rootSK)
              ! #address (encToPublic addrSK)
        , addrSK)
