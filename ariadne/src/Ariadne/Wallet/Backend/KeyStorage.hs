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
       , toWalletNamesList

         -- * Exceptions
       , NoWalletSelection (..)
       , NoAccountSelection (..)
       ) where

import Universum hiding ((^.))

import Control.Exception (Exception(displayException))
import Control.Lens (ix, (%=), (^.))
import Control.Monad.Catch.Pure (Catch, CatchT, runCatchT)
import Data.Acid (AcidState, query, update)
import qualified Data.IxSet.Typed as IxSet
import Data.Map (findWithDefault)
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Buildable
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as V (elem, foldr, fromList, mapMaybe)
import Formatting (bprint, int, (%))
import IiExtras
import Loot.Crypto.Bip39 (entropyToMnemonic, mnemonicToSeed)
import Named ((!))
import Numeric.Natural (Natural)

import Pos.Client.KeyStorage (getSecretDefault, modifySecretDefault)
import Pos.Core
import Pos.Core.Common (IsBootstrapEraAddr(..), addressHash)
import Pos.Crypto
import Pos.Util (eitherToThrow, maybeThrow)
import Pos.Util.UserSecret (usWallets)
import Serokell.Data.Memory.Units (Byte)

import Ariadne.Cardano.Face
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Wallet.Cardano.Kernel.Bip32
import Ariadne.Wallet.Cardano.Kernel.Bip44
  (Bip44DerivationPath(..), encodeBip44DerivationPath)
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Spec
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx (PrefilteredUtxo)
import Ariadne.Wallet.Cardano.Kernel.Word31
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
    displayException =
        \case
            AGFailedIncorrectPassPhrase ->
                "Address generation failed due to incorrect passphrase"

data DuplicateAccountName = DuplicateAccountName AccountName
  deriving (Eq, Show)

instance Exception DuplicateAccountName where
  displayException (DuplicateAccountName (AccountName t)) =
    "The account name " ++ show t ++ " already exists."

data DuplicateWalletName = DuplicateWalletName WalletName
  deriving (Eq, Show)

instance Exception DuplicateWalletName where
  displayException (DuplicateWalletName (WalletName t)) =
    "The wallet name " ++ show t ++ " already exists."

data DuplicatedWalletKey = DuplicatedWalletKey
  deriving (Eq, Show)

instance Exception DuplicatedWalletKey where
  displayException DuplicatedWalletKey =
    "The wallet with this root key already exists"

data NotARenamableItem = NotARenamableItem
  deriving (Eq, Show)

instance Exception NotARenamableItem where
 displayException NotARenamableItem =
    "This item cannot be renamed."

data NotARemovableItem = NotARemovableItem
  deriving (Eq, Show)

instance Exception NotARemovableItem where
 displayException NotARemovableItem =
    "This item cannot be removed."

data WalletIndexOutOfRange = WalletIndexOutOfRange Word
  deriving (Eq, Show)

instance Exception WalletIndexOutOfRange where
  displayException (WalletIndexOutOfRange i) =
   "The index " ++ show i ++ " is out of range."

-- | Utility function that runs CatchT monad inside a StateT and rollbacks the state on failure
runCatchInState :: Functor m => StateT s (CatchT m) a -> StateT s m (Either SomeException a)
runCatchInState m = StateT $ \s ->
  runCatchT (runStateT m s) <&> \case
    Left e -> (Left e, s)
    Right (a, s') -> (Right a, s')

-- | Get the wallet HdRootId by name or using current selection.
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
      Just WalletSelection{..} -> return $ getHdRootId wsPath
  WalletRefByName walletName -> do
    case getHdRootIdByName walletName of
      Just hdR -> return hdR
      Nothing -> throwM $ WalletDoesNotExist walletName
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

    getHdRootId :: HdPath -> HdRootId
    getHdRootId (RootPath rId) = rId
    getHdRootId (AccountPath acId) = acId ^. hdAccountIdParent
    getHdRootId (AddressPath adId) = adId ^. hdAddressIdParent . hdAccountIdParent

    -- TODO: Use IxSet filter. Need to add hdRootName to index.
    getHdRootIdByName :: WalletName -> Maybe HdRootId
    getHdRootIdByName wn = case filter (\w -> (w ^. hdRootName) == wn) walletList of
      [hdWallet] -> Just (hdWallet ^. hdRootId)
      [] -> Nothing
      -- Duplication is checked in `renameSelection` and `addWallet` routines.
      _:_:_ -> error "Bug: _hdRootName duplication"

    walletList :: [HdRoot]
    walletList = toList (readAllHdRoots hdWallets)

  -- TODO:
  -- * Check if is it possible, that address or account exist in db
  --   but has no parents?
  -- * If so, where do we need to check,
  --   if account and address parsents exist?
resolveAccountRef
  :: IORef (Maybe WalletSelection)
  -> AccountReference
  -> DB
  -> IO HdAccountId
resolveAccountRef walletSelRef accountRef walletDb = do
  accId <- case accountRef of
    AccountRefSelection -> do
      mWalletSelection <- readIORef walletSelRef
      case mWalletSelection of
        Nothing -> throwM NoWalletSelection
        Just WalletSelection {..} -> case wsPath of
          RootPath _ -> throwM NoAccountSelection
          AccountPath accId_ -> do
            checkParentRoot accId_
            return accId_
          AddressPath addrId -> do
            let accId_ = (addrId ^. hdAddressIdParent)
            checkParentRoot accId_
            return accId_
    AccountRefByHdAccountId accId_ -> do
      checkParentRoot accId_
      return accId_
    AccountRefByName accName walRef -> do
      accounts <- getAccList walRef
      -- TODO: add AccountName to IxSet index and use `getEQ`
      acc <- oneOnly accName $ filter (\acc -> acc ^. hdAccountName . unAccountName == accName) accounts
      let accId_ = acc ^. hdAccountId
      checkParentRoot accId_
      return accId_
    AccountRefByUIindex accIdx walRef -> do
      accounts <- getAccList walRef
      acc <- maybeThrow
        (AccountDoesNotExist $ pretty accIdx)
        (accounts ^? ix (fromIntegral accIdx))
      let accId_ = acc ^. hdAccountId
      checkParentRoot accId_
      return accId_
  return accId
  where
    oneOnly _ [x] = return x
    oneOnly accName [] = throwM (AccountDoesNotExist accName)
    oneOnly _ _ = error "Bug: account name duplication"

    getAccList walRef = do
      rootId <- resolveWalletRef walletSelRef walRef walletDb
      let wallets = walletDb ^. dbHdWallets
      toList <$> (throwLeft $ readAccountsByRootId rootId wallets)

    checkParentRoot :: HdAccountId -> IO ()
    checkParentRoot accId =
      throwLeft $ readHdRoot (accId ^. hdAccountIdParent) (walletDb ^. dbHdWallets) >> return ()

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
    pubAddrHash = _fromDb (_unHdRootId walletRootId)
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
          ( _unHdAddressIx
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

-- TODO: Move name check to Create.hs
newAccount
  :: AcidState DB
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> WalletReference
  -> Maybe AccountName
  -> IO ()
newAccount acidDb WalletFace{..} walletSelRef walletRef mbAccountName = do
  walletDb <- query acidDb Snapshot
  rootId <- resolveWalletRef walletSelRef walletRef walletDb

  let
    wallets = walletDb ^. dbHdWallets
    namesVec = V.fromList
      (map (_unAccountName . (^. hdAccountName)) $ toList $ getAccounts (walletDb ^. dbHdWallets) rootId)

  accountName <- case (_unAccountName <$> mbAccountName) of
    Nothing ->
      return (mkUntitled "Untitled account " namesVec)
    Just accountName_ -> do
      when (accountName_ `elem` namesVec) $ throwM $ DuplicateAccountName (AccountName accountName_)
      return accountName_

  let hdAccId = HdAccountId rootId (HdAccountIx $ accountIdx rootId wallets)
  throwLeftIO $ update acidDb (CreateHdAccount hdAccId mempty (Just (AccountName accountName)))

  walletRefreshState
  where
    accIndexes :: HdRootId -> HdWallets -> Vector Word31
    accIndexes rootId wallets =
      V.fromList (
        ( _unHdAccountIx
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
-- TODO: Move name check to Create.hs
addWallet ::
       AcidState DB
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> EncryptedSecretKey
    -> Maybe WalletName
    -> Map HdAccountId PrefilteredUtxo
    -> IO ()
addWallet acidDb WalletFace {..} runCardanoMode esk mbWalletName utxoByAccount = do
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
  let namesVec = V.fromList (map _unWalletName $ toWalletNamesList (walletDb ^. dbHdWallets))
  walletName <- case mbWalletName of
    Nothing ->
      return (WalletName $ mkUntitled "Untitled wallet " namesVec)
    Just walletName_ -> do
      -- TODO:
      -- * move duplicate check to `updateHdRootName`
      when ((_unWalletName walletName_) `V.elem` namesVec) $
        throwM $ DuplicateWalletName walletName_
      return walletName_

  -- getPOSIXTime return seconds with 10^-12 precision
  timestamp <- (InDb . round . (* 10 ^ (6 :: Integer)) <$> getPOSIXTime)
  let rootId = HdRootId $ InDb $ addressHash $ encToPublic esk

  -- FIXME: This should be passed to `addWallet` I guess.
  let hasPass = undefined
  let assurance = undefined

  let hdRoot = HdRoot
          { _hdRootId = rootId
          , _hdRootName = walletName
          , _hdRootHasPassword = hasPass
          , _hdRootAssurance = assurance
          , _hdRootCreatedAt = timestamp
          }
  -- Map.empty means we're using autogenerated account names
  throwLeftIO $ update acidDb (CreateHdWallet hdRoot utxoByAccount Map.empty)

  walletRefreshState

-- | convert path in index representation and write it to IORef WalletSelection
-- TODO:
-- * check if ui uses the same address indexation as in KeyStorage.hs (Random indexation)
-- * handle chain type (ui can represent it as new level; proper indexation should be used then)
select
  :: AcidState DB
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> Maybe WalletReference
  -> [Word]
  -> IO ()
select acidDb WalletFace{..} walletSelRef mWalletRef wsPath = do
  walletDb <- query acidDb Snapshot
  let wallets = walletDb ^. dbHdWallets
  mbPath <- case mWalletRef of
    Nothing -> return Nothing
    Just walletRef -> do
      -- Throw an exception if walletRef is invalid
      rootId <- resolveWalletRef walletSelRef walletRef walletDb
      case nonEmpty wsPath of
        Nothing -> return $ Just $ RootPath rootId
        Just (accIdx :| acPath) -> do
          -- validate account
          hdAccount <- maybeThrow
            (AccountDoesNotExist $ pretty accIdx)
            ((accList wallets rootId) ^? ix (fromIntegral accIdx))

          case nonEmpty acPath of
            Nothing -> return $ Just $ AccountPath $ hdAccount ^. hdAccountId
            Just (addrIdx :| []) -> do
              -- validate address
              hdAddr <- maybeThrow
                (AddressDoesNotExist $ pretty addrIdx)
                ((addrList wallets (hdAccount ^. hdAccountId)) ^? ix (fromIntegral addrIdx))
              return $ Just $ AddressPath $ hdAddr ^. hdAddressId
            Just (_ :| _) -> throwM SelectIsTooDeep
  atomicWriteIORef walletSelRef (WalletSelection <$> mbPath)
  walletRefreshState
  where
    addrList :: HdWallets -> HdAccountId -> [HdAddress]
    addrList wallets accId = toList $ getAddresses wallets accId

    -- TODO: make it global
    accList :: HdWallets -> HdRootId -> [HdAccount]
    accList wallets rootId = toList $ getAccounts wallets rootId

    getAccounts :: HdWallets -> HdRootId -> IxSet HdAccount
    getAccounts wallets rootId = fromRight
      (error "Bug: UnknownHdRoot")
      (readAccountsByRootId rootId wallets)

    getAddresses :: HdWallets -> HdAccountId -> IxSet HdAddress
    getAddresses wallets accountId = fromRight
      (error "Bug: UnknownHdAccount")
      (readAddressesByAccountId accountId wallets)

-- Do we need this function?
getSelectedAddresses
  :: AcidState DB
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> IO [Address]
getSelectedAddresses = undefined

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
    Just WalletSelection {..} -> case wsPath of
      RootPath hdrId -> do
        update acidDb (DeleteHdRoot hdrId)
        runCardanoMode $ modifySecretDefault (usWallets %= Map.delete (fromRootId hdrId))
        return Nothing
      AccountPath accId -> do
        throwLeftIO $ update acidDb (DeleteHdAccount accId)
        return $ Just $ WalletSelection $ RootPath (accId ^. hdAccountIdParent)
      AddressPath _ -> throwM NotARemovableItem
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
  walletDb <- query acidDb Snapshot
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> pure ()
    Just WalletSelection{..} -> case wsPath of
      RootPath hdrId -> do
        let namesList = map _unWalletName (toWalletNamesList (walletDb ^. dbHdWallets))
        when (name `elem` namesList)
          (throwM $ DuplicateWalletName (WalletName name))
        throwLeftIO $ update acidDb (UpdateHdRootName hdrId (WalletName name))

      AccountPath accId -> do
        accounts <- case
          readAccountsByRootId (accId ^. hdAccountIdParent) (walletDb ^. dbHdWallets) of
            Right accs -> return (map unwrapOrdByPrimKey (IxSet.toList $ unwrapIxSet accs))
            Left err -> throwM err
        let namesList = map (_unAccountName . _hdAccountName) accounts
        when (name `elem` namesList)
          (throwM $ DuplicateAccountName (AccountName name))
        throwLeftIO $ update acidDb (UpdateHdAccountName accId (AccountName name))

      AddressPath _ -> throwM NotARenamableItem
  walletRefreshState

-- Helpers
throwLeft :: (Exception e) => (Either e a) -> IO a
throwLeft eith = case eith of
  Left e -> throwM e
  Right r -> return r

throwLeftIO :: (Exception e) => IO (Either e a) -> IO a
throwLeftIO ioEith = ioEith >>= throwLeft

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

toWalletNamesList :: HdWallets -> [WalletName]
toWalletNamesList hdw =
  (^. hdRootName) <$> toList (hdw ^. hdWalletsRoots)
