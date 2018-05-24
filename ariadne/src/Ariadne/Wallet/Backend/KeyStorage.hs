-- | Part of backend which manages keys, wallets, accounts, addresses.

module Ariadne.Wallet.Backend.KeyStorage
       (
         -- * Commands/other functions
         resolveWalletRef
       , refreshUserSecret
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

import Universum

import Control.Exception (Exception(displayException))
import Control.Lens (ix, zoom, (%=), (.=), (<>=))
import Control.Monad.Catch.Pure (Catch, CatchT, runCatchT)
import Data.List (findIndex)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Buildable
import qualified Data.Vector as V
  (findIndex, foldr, fromList, ifilter, mapMaybe)
import Formatting (bprint, int, (%))
import IiExtras
import Loot.Crypto.Bip39 (entropyToMnemonic, mnemonicToSeed)
import Named ((!))
import Numeric.Natural (Natural)
import Serokell.Data.Memory.Units (Byte)

import Pos.Client.KeyStorage (getSecretDefault, modifySecretDefault)
import Pos.Core.Common (IsBootstrapEraAddr(..), deriveLvl2KeyPair)
import Pos.Core.Common.Address (addressHash)
import Pos.Crypto
import Pos.Crypto.Signing.Types.Safe (encToPublic)
import Pos.Util (eitherToThrow, maybeThrow)
import Pos.Util.UserSecret

import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Wallet.Cardano.Kernel.Bip32
import Ariadne.Wallet.Cardano.Kernel.Bip44
  (Bip44DerivationPath(..), encodeBip44DerivationPath)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (HdAccountIx(..), HdAddressChain(..), HdAddressIx(..))
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet (HdAccountIx (..), HdAddressIx (..), HdAddressChain (..))
import Serokell.Data.Memory.Units (Byte)
import qualified Data.Map as Map

import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet as HD
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

data WalletDoesNotExist = WalletDoesNotExist Text
  deriving (Eq, Show)

instance Exception WalletDoesNotExist where
  displayException (WalletDoesNotExist t) =
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

data DuplicateAccountName = DuplicateAccountName Text
  deriving (Eq, Show)

instance Exception DuplicateAccountName where
  displayException (DuplicateAccountName t) =
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
-- resolveWalletRef
resolveWalletRef
  :: IORef (Maybe WalletSelection)
  -> WalletReference
  -> DB
  -> IO HdRootId
resolveWalletRef walRef walletSelRef db = case walRef of
  WalletRefSelection -> do
    mWalletSelection <- readIORef walletSelRef
    case mWalletSelection of
      Nothing -> throwM NoWalletSelection
      Just WalletSelection{..} -> return $ getHdRootId wsPath
  WalletRefByName walletName -> do
    case getHdRootIdByName walletName hdWallets of
      Just hdR -> return hdR
      Nothing -> throwM $ WalletDoesNotExist walletName
  WalletRefByIndex i -> do
    -- Note: Add/remove wallets cause changes in indexation
    case (IxSet.toAscList (Proxy :: WalletName) hdWallets) ^? ix i of
      Just hdWallet -> return (hdWallet ^. hdRootId)
      Nothing -> throwM $ WalletIndexOutOfRange i
  WalletRefByHdRootId hrR -> case readHdRoot hrR hdWallets of
    Right _ -> return hrR
    Left err -> throwM err

  where
    hdWallets :: HdWallets
    hdWallets = db ^. dbHdWallets

    getHdRootId :: HdPath -> HdRootId
    getHdRootId (RootPath rId) = rid
    getHdRootId (AccountPath acId) = acId ^. hdAccountIdParent
    getHdRootId (AddressPath adId) = adId ^. hdAddressIdParent . hdAccountIdParent

    getHdRootIdByName :: WalletName -> HdWallets -> Maybe HdRootId
    getHdRootIdByName wn hdWallets = case IxSet.toList (IxSet.getEQ wn hdWallets) of
      [hdWallet] -> Just (hdWallet ^. hdRootId)
      [] -> Nothing
      [f:_] -> error "Bug: _hdRootName duplication"


  -- TODO: Finish
resolveAccountRef
  :: IORef (Maybe WalletSelection)
  -> AccountReference
  -> DB
  -> IO HdAccountId
resolveAccountRef walletSelRef accountRef acidDb = undefined
  -- mWalletSelection <- readIORef walletSelRef
  -- walletDb <- query acidDB Snapshot
  -- accId <- case accountRef of
  --   AccountRefSelection -> do
  --     mWalletSelection <- readIORef walletSelRef
  --     case mWalletSelection of
  --       Nothing -> throwM NoWalletSelection
  --       Just WalletSelection {..} -> case wsPath of
  --         RootPath hdr -> undefined
  --         AccountPath accPath -> undefined
  --         AddressPath addrPath -> undefined


refreshState
  :: AcidState DB
  -> IORef (Maybe WalletSelection)
  -> (WalletEvent -> IO ())
  -> IO ()
refreshState acidDB walletSelRef sendWalletEvent = do
  walletSel <- readIORef walletSelRef
  walletDb <- query acidDB Snapshot
  sendWalletEvent (WalletStateSetEvent walletDb walletSel)

newAddress ::
       AcidState DB
    -> WalletFace
    -> IORef (Maybe WalletSelection)
    -> AccountReference
    -> HdAddressChain
    -> PassPhrase
    -> IO ()
newAddress dbACID WalletFace {..} walletSelRef accRef chain pp = do
  walletDb <- query dbACID Snapshot

  hdAccountId <- resolveAccountRef walletSelRef walletDb accRef

  let hdAddressId = HdAccountId
      { _hdAddressIdParent = hdAccountId
      , _hdAddressIdIx = mkAddrIdx walletDb hdAccountId
      }

  addr <-
      case deriveLvl3KeyPair
                (IsBootstrapEraAddr True)
                (ShouldCheckPassphrase True)
                pp
                hdAccountId ^. hdAccountIdIx
                chain
                hdAddressId ^. hdAddressIdIx of
          Nothing -> throwM AGFailedIncorrectPassPhrase
          Just (a, _) -> pure a

  update dbACID (CreateHdAddress hdAddressId addr chain)
  walletRefreshState
    where
      -- TODO:
      -- get first unique index from account's address set (Mabe toList it and use old function)
      mkAddrIdx :: DB -> HdAccountId -> HdAddressIx
      mkAddrIdx = undefined

deriveLvl3KeyPair
    :: Bi Address'
    => IsBootstrapEraAddr
    -> ShouldCheckPassphrase
    -> PassPhrase
    -> EncryptedSecretKey -- ^ key of wallet
    -> HdAccountIx -- ^ account derivation index
    -> HdAddressChain
    -> HdAddressIx -- ^ address derivation index
    -> Maybe (Address, EncryptedSecretKey)
deriveLvl3KeyPair = error "AD-126"

mkUntitled :: Text -> Vector Text -> Text
mkUntitled untitled namesVec =
  let
    untitledSuffixes = V.mapMaybe (T.stripPrefix $ untitled) namesVec
    numbers = V.mapMaybe ((readMaybe @Natural) . T.unpack) untitledSuffixes
  in if null untitledSuffixes || null numbers
    then untitled <> "0"
    else untitled <> (show $ (Universum.maximum numbers) + 1)

newAccount
  :: AcidState DB
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> Maybe CheckPoint
  -> WalletReference
  -> Maybe AccountName
  -> IO ()
newAccount acidDB WalletFace{..} walletSelRef mbCheckPoint walletRef mbAccountName = do
  walletDb <- query acidDB Snapshot
  hdRootId <- resolveWalletRef walletSelRef walletDb walletRef

  -- need to query ixSet to find if the name already in db
  let accounts = case readAccountsByRootId hdRootId walletDb of
      Left err -> throwM err
      Right acc -> acc

  let namesVec = V.fromList (map (unAccountName . (^. hdAccountName)) accounts)

  accountName <- case mbAccountName of
    Nothing ->
      return (mkUntitled "Untitled account " namesVec)
    Just accountName_ -> do
      when (accountName_ `elem` namesVec) $ throwM $ DuplicateAccountName accountName_
      return accountName_

  update acidDB (CreateHdAccount
    hdRootId
    accountName
    (fromMaybe (mempty @CheckPoint) mbCheckPoint))

  walletRefreshState


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
newWallet acidDB walletConfig face runCardanoMode pp mbWalletName mbEntropySize = do
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
  mnemonic ++ ["ariadne-v0"] <$ addWallet acidDB face runCardanoMode esk mbWalletName mempty

-- | Construct a wallet from given data and add it to the storage.
addWallet ::
       AcidState DB
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> EncryptedSecretKey
    -> Maybe WalletName
    -> Vector HdAccount
    -> IO ()
addWallet acidDB wf@WalletFace {..} runCardanoMode esk mbWalletName accounts = do
  let addWalletPure :: StateT UserSecret Catch ()
      addWalletPure = do
        eskList <- Map.elems <$> (use usWallets)
        let keysList = encToPublic <$> eskList
        when (encToPublic esk `elem` keysList) $
          throwM DuplicatedWalletKey
        usWallets %= Map.insert (addressHash $ encToPublic esk) esk

  runCardanoMode (modifySecretDefault (runCatchInState addWalletPure)) >>=
    eitherToThrow

  walletDb <- query acidDB Snapshot
  -- need to query ixSet to find if the name already in db
  let namesList = map unWalletName $ toWalletNamesList (walletDb ^. dbHdWallets)
  walletName <- case mbWalletName of
    Nothing ->
      return (WalletName $ mkUntitled "Untitled wallet " namesList)
    Just walletName_ -> do
      -- TODO: move duplicate check to `updateHdRootName`
      when (unWalletName walletName_ `elem` namesList) $
        throwM $ DuplicateWalletName walletName_
      return walletName_

  timestamp <- (fromMicroseconds . round . (* 10 ^ 6 ) . nominalDiffTimeToSeconds) <$> getPOSIXTime
  let hdRootId = HdRootId InDb $ addressHash $ encToPublic esk

  -- FIXME: This should be passed to `addWallet` I guess.
  let hasPass = undefined
  let assurance = undefined

  update acidDB (CreateHdRoot hdRootId walletName hasPass assurance timestamp)
  forM_ accounts \acc ->
    newAccount
      acidDB
      wf
      Nothing
      (WalletRefByHdRootId hdRootId)
      (Just acc ^. hdAccountName)
      (Just head (acc ^. hdAccountCheckpoints))
    -- Drawbacks:
    -- * This will spawn a sequence of walletState events
    -- * What if exeption (e.g. duplicate accoun name) raised
    --   in the middle of accounts list?

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
select acidDB WalletFace{..} walletSelRef mWalletRef wsPath = do
  walletDb <- query acidDB Snapshot
  let wallets = walletDb ^. hdWallets
  mbPath <- case mWalletRef of
    Nothing -> return Nothing
    Just walletRef -> do
      -- Throw exception if walletRef not found
      rootId <- resolveWalletRef walletSelRef walletDb walletRef
      case nonEmpty wsPath of
        Nothing -> return $ Just $ RootPath rootId
        Just (accIdx :| acPath) -> do
          -- validate account
          hdAcc <- maybeThrow
            (AccountDoesNotExist $ pretty accIdx)
            -- TODO: embed info about convertion rules in path type: [(Word, Ordering Proxy)]
            ((IxSet.toAscList (Proxy :: AccountName) (getAccounts wallets rootId)) ^? ix (fromIntegral accIdx))

          case nonEmpty acPath of
            Nothing -> return $ Just $ AccountPath $ hdAccount ^. hdAccountId
            Just (addrIdx :| []) -> do
              -- validate address
              hdAddr <- maybeThrow
                (AddressDoesNotExist $ pretty addrIdx)
                ((IxSet.toList getAddresses wallets (hdAccount ^. hdAccountId)) ^? ix (fromIntegral accIdx))
              return $ Just $ AddressPath $ hdAddr ^. hdAddressId
            Just (_ :| _) -> throwM SelectIsTooDeep
  atomicWriteIORef walletSelRef (WalletSelection <$> mbPath)
  walletRefreshState
  where
    getAccounts :: HdWallets -> HdRootId -> IxSet HdAccount
    getAccounts wallets hdRootId = fromRight
      (error "Bug: UnknownHdRoot")
      (readAccountsByRootId wallets hdRootId)

    getAddresses :: HdWallets -> HdAccountId -> IxSet HdAddress
    getAddresses wallets hdAccountId = fromRight
      (error "Bug: UnknownHdAccount")
      (readAddressesByAccountId wallets hdAccountId)


-- Do we need this function?
getSelectedAddresses
  :: AcidState DB
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> IO [Address]
getSelectedAddresses acidDB WalletFace{..} walletSelRef = undefined

removeSelection
  :: AcidState DB
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> IO ()
removeSelection acidDB WalletFace{..} walletSelRef runCardanoMode = do
  walletDB <- query acidDB Snapshot
  mWalletSel <- readIORef walletSelRef
  newSelection <- case mWalletSel of
    Nothing -> pure ()
    Just WalletSelection {..} -> case wsPath of
      RootPath hdrId -> do
        update acidDb (DeleteHdRoot hdrId)
        runCardanoMode $ modifySecretDefault usWallets %= Map.delete (fromRootId hdrId)
        return Nothing
      AccountPath accId -> do
        update acidDb (DeleteHdAccount accId)
        return $ Just $ WalletSelection $ RootPath (accId ^. hdAccountIdParent)
      AddressPath addrId -> undefined
  atomicWriteIORef walletSelRef newSelection
  walletRefreshState
  where
    fromRootId :: HdRootId -> (Core.AddressHash Core.PublicKey)
    fromRootId (HdRootId (InDb x)) = x


renameSelection
  :: AcidState DB
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> Text
  -> IO ()
renameSelection acidDB WalletFace{..} walletSelRef runCardanoMode name = do
  walletDB <- query acidDB Snapshot
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> pure ()
    Just WalletSelection{..} -> case wsPath of
      RootPath hdrId -> do
        let namesList = map unWalletName $ toWalletNamesList (walletDb ^. dbHdWallets)
        when name `elen` namesList
          throwM $ DuplicateWalletName name
        update acidDb updateHdRootName hdrId name
      AccountPath accId -> do
        accounts <- case
          readAccountsByRootId (accId ^. hdAccountIdParent) (walletDb ^. dbHdWallets) of
            Rigth accs -> return $ IxSet.toList accs
            Left err -> throwM err
        let namesList = map unAccountName accounts
        when name `elen` namesList
          throwM $ DuplicateWalletName name
        update acidDb updateHdRootName hdrId name
      AddressPath _ -> throwM $ NotARenamableItem
  walletRefreshState

-- Helpers

findFirstUnique :: (Ord a, Enum a) => a -> Vector a -> a
findFirstUnique lastIdx paths = head
    . fromMaybe (error "Can't find a unique path!")
    . nonEmpty
    $ dropWhile (`S.member` pathsSet) [lastIdx..]
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
  (^. hdRootName) <$> IxSet.toList (hdw .^ dbHdWallets . hdWalletsRoots)
