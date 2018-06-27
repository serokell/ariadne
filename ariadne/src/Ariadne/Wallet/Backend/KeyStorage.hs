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

import Universum hiding ((^.))

import Ariadne.Cardano.Face
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (DB(..))
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Spec
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet
import Ariadne.Wallet.Face
import Control.Exception (Exception(displayException))
import Control.Lens (ix, zoom, (%=), (.=), (<>=), (^.))
import Control.Monad.Catch.Pure (Catch, CatchT, runCatchT)
import Data.Acid (AcidState, query, update)
import qualified Data.IxSet.Typed as IxSet
import Data.List (findIndex)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Buildable
import Data.Time.Clock (diffTimeToPicoseconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Units (fromMicroseconds)
import qualified Data.Vector as V
  (elem, findIndex, foldr, fromList, ifilter, mapMaybe)
import Formatting (bprint, int, (%))
import IiExtras
import Loot.Crypto.Bip39 (entropyToMnemonic, mnemonicToSeed)
import Named ((!))
import Numeric.Natural (Natural)
import Serokell.Data.Memory.Units (Byte)

import Pos.Client.KeyStorage (getSecretDefault, modifySecretDefault)
import Pos.Core
import Pos.Core.Common (IsBootstrapEraAddr(..), deriveLvl2KeyPair)
import Pos.Core.Common.Address (addressHash)
import Pos.Crypto
import Pos.Util (eitherToThrow, maybeThrow)
import Pos.Util.UserSecret

import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Wallet.Cardano.Kernel.Bip32
import Ariadne.Wallet.Cardano.Kernel.Bip44
  (Bip44DerivationPath(..), encodeBip44DerivationPath)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (HdAccountIx(..), HdAddressChain(..), HdAddressIx(..))
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (HdAccountIx(..), HdAddressChain(..), HdAddressIx(..))
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
  -- I guess we do not need to resolve Ui indexation (WalletRefByIndex) here,
  -- lets do it in `select` only.

  -- WalletRefByIndex i -> do
  --   -- Note: Add/remove wallets cause changes in indexation
  --   case (IxSet.toAscList (Proxy :: WalletName) hdWallets) ^? ix i of
  --     Just hdWallet -> return (hdWallet ^. hdRootId)
  --     Nothing -> throwM $ WalletIndexOutOfRange i
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
      [f,_] -> error "Bug: _hdRootName duplication"

    walletList :: [HdRoot]
    walletList = unwrapOrdByPrimKey <$> (IxSet.toList (unwrapIxSet (readAllHdRoots hdWallets)))

  -- TODO:
  -- * Is it possible, that address or account exist in db
  --   but has no parents?
  -- * If so, where do we need to check,
  --   if account and address parsents exist?
resolveAccountRef
  :: IORef (Maybe WalletSelection)
  -> AccountReference
  -> DB
  -> IO HdAccountId
resolveAccountRef walletSelRef accountRef walletDb = undefined
  -- mWalletSelection <- readIORef walletSelRef
  -- accId <- case accountRef of
  --   AccountRefSelection -> do
  --     mWalletSelection <- readIORef walletSelRef
  --     case mWalletSelection of
  --       Nothing -> throwM NoWalletSelection
  --       Just WalletSelection {..} -> case wsPath of
  --         RootPath _ -> throwM NoAccountSelection
  --         AccountPath accId_ -> return accId_
  --         AddressPath addrId -> return $ addrId ^. hdAddressIdParent
  --   AccountRefByHdAccountId accId -> return accId
  --   AccountRefByName accName walRef -> do
  --     rootId <- maybeThrow
  --       NoWalletSelection
  --       (readIORef walRef)
  --       let
  --         accounts = case readAccountsByRootId hdRootId walletDb of
  --           Left err -> throwM err
  --           Right acc -> acc

  --       let namesVec = V.fromList (map (unAccountName . (^. hdAccountName)) accounts)

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
    -> AccountReference
    -> HdAddressChain
    -> PassPhrase
    -> IO ()
newAddress acidDb WalletFace {..} walletSelRef accRef chain pp = do
  walletDb <- query acidDb Snapshot

  hdAccountId <- resolveAccountRef walletSelRef accRef walletDb

  let
    hdAddressId = HdAddressId
      { _hdAddressIdParent = hdAccountId
      , _hdAddressIdIx = mkAddrIdx walletDb hdAccountId
      }

  addr <-
      case (deriveLvl3KeyPair
                (IsBootstrapEraAddr True)
                (ShouldCheckPassphrase True)
                pp
                walletEsk
                (hdAccountId ^. hdAccountIdIx)
                chain
                (hdAddressId ^. hdAddressIdIx)) of
          Nothing -> throwM AGFailedIncorrectPassPhrase
          Just (a, _) -> pure a
  throwLeft_ $ update acidDb (CreateHdAddress hdAddressId (InDb addr) chain emptyCheckpoint)
  walletRefreshState
    where
      -- TODO: Get it from UserSecret
      walletEsk :: EncryptedSecretKey
      walletEsk = undefined
      -- TODO:
      -- get first unique index from account's address set (Mabe toList it and use old function)
      mkAddrIdx :: DB -> HdAccountId -> HdAddressIx
      mkAddrIdx = undefined

      emptyCheckpoint :: AddrCheckpoint
      emptyCheckpoint = undefined

deriveLvl3KeyPair
    :: IsBootstrapEraAddr
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

-- TODO: Move name check to Create.hs
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
  hdRootId <- resolveWalletRef walletSelRef walletRef walletDb

  let
    namesVec = V.fromList
      (map (unAccountName . (^. hdAccountName)) $ toAccList (walletDb ^. dbHdWallets) hdRootId)

  accountName <- case (unAccountName <$> mbAccountName) of
    Nothing ->
      return (mkUntitled "Untitled account " namesVec)
    Just accountName_ -> do
      when (accountName_ `elem` namesVec) $ throwM $ DuplicateAccountName (AccountName accountName_)
      return accountName_

  throwLeft_ $ update acidDb (CreateHdAccount
    hdRootId
    (AccountName accountName)
    (fromMaybe emptyCheckpoint mbCheckPoint))

  walletRefreshState
  where
    -- FIXME
    emptyCheckpoint = undefined

    -- The same function as in `select`. It is unsafe -> not global.
    -- It is assumed that hdRootId is a valid one.
    -- It is, because resolveWalletRef always returns a valid HdRootId.
    getAccounts :: HdWallets -> HdRootId -> IxSet HdAccount
    getAccounts wallets hdRootId = fromRight
      (error "Bug: UnknownHdRoot")
      (readAccountsByRootId hdRootId wallets)

    toAccList :: HdWallets -> HdRootId -> [HdAccount]
    toAccList wallets rootId =
      map unwrapOrdByPrimKey (toAccountsList $ getAccounts wallets rootId)

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
    -> IORef (Maybe WalletSelection)
    -> (CardanoMode ~> IO)
    -> PassPhrase
    -> Maybe WalletName
    -> Maybe Byte
    -> IO [Text]
newWallet acidDb walletConfig face walletSelRef runCardanoMode pp mbWalletName mbEntropySize = do
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
  mnemonic ++ ["ariadne-v0"] <$ addWallet acidDb face walletSelRef runCardanoMode esk mbWalletName mempty

-- | Construct a wallet from given data and add it to the storage.
-- Note: selection switches to the new wallet.
-- It is needed to create accounts, because
-- one have to pass selection to `addAccout`.

-- TODO: Move name check to Create.hs
addWallet ::
       AcidState DB
    -> WalletFace
    -> IORef (Maybe WalletSelection)
    -> (CardanoMode ~> IO)
    -> EncryptedSecretKey
    -> Maybe WalletName
    -> Vector HdAccount
    -> IO ()
addWallet acidDb wf@WalletFace {..} walletSelRef runCardanoMode esk mbWalletName accounts = do
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
  let namesVec = V.fromList (map unWalletName $ toWalletNamesList (walletDb ^. dbHdWallets))
  walletName <- case mbWalletName of
    Nothing ->
      return (WalletName $ mkUntitled "Untitled wallet " namesVec)
    Just walletName_ -> do
      -- TODO: move duplicate check to `updateHdRootName`
      when ((unWalletName walletName_) `V.elem` namesVec) $
        throwM $ DuplicateWalletName walletName_
      return walletName_

  -- getPOSIXTime return seconds with 10^-12 precision
  timestamp <- (InDb . fromMicroseconds . round . (* 10 ** (-6)) . toRational) <$> getPOSIXTime
  let hdRootId = HdRootId $ InDb $ addressHash $ encToPublic esk

  -- FIXME: This should be passed to `addWallet` I guess.
  let hasPass = undefined
  let assurance = undefined

  throwLeft_ $ update acidDb (CreateHdRoot hdRootId walletName hasPass assurance timestamp)

  -- FIXME: Make it a single acid-state transaction
  forM_ accounts (\acc ->
    newAccount
      acidDb
      wf
      walletSelRef
      (Just $ head (acc ^. hdAccountCheckpoints))
      (WalletRefByHdRootId hdRootId)
      (Just $ acc ^. hdAccountName))
      -- Drawbacks:
    -- * This will spawn a sequence of walletState events
    -- * What if exeption (e.g. duplicate accoun name) raised
    --   in the middle of accounts list
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
            -- TODO: embed info about convertion rules in path type: [(Word, Ordering Proxy)]
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
    addrList wallets accId =
      map unwrapOrdByPrimKey (toAddressList $ getAddresses wallets accId)

    accList :: HdWallets -> HdRootId -> [HdAccount]
    accList wallets rootId =
      map unwrapOrdByPrimKey (toAccountsList $ getAccounts wallets rootId)

    getAccounts :: HdWallets -> HdRootId -> IxSet HdAccount
    getAccounts wallets hdRootId = fromRight
      (error "Bug: UnknownHdRoot")
      (readAccountsByRootId hdRootId wallets)

    getAddresses :: HdWallets -> HdAccountId -> IxSet HdAddress
    getAddresses wallets hdAccountId = fromRight
      (error "Bug: UnknownHdAccount")
      (readAddressesByAccountId hdAccountId wallets)


-- Do we need this function?
getSelectedAddresses
  :: AcidState DB
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> IO [Address]
getSelectedAddresses acidDb WalletFace{..} walletSelRef = undefined

removeSelection
  :: AcidState DB
  -> WalletFace
  -> IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> IO ()
removeSelection acidDb WalletFace{..} walletSelRef runCardanoMode = do
  walletDB <- query acidDb Snapshot
  mWalletSel <- readIORef walletSelRef
  newSelection <- case mWalletSel of
    Nothing -> pure Nothing
    -- FIXME:
    -- Throw "Nothing selected" here
    Just WalletSelection {..} -> case wsPath of
      RootPath hdrId -> do
        update acidDb (DeleteHdRoot hdrId)
        runCardanoMode $ modifySecretDefault (usWallets %= Map.delete (fromRootId hdrId))
        return Nothing
      AccountPath accId -> do
        throwLeft_ $ update acidDb (DeleteHdAccount accId)
        return $ Just $ WalletSelection $ RootPath (accId ^. hdAccountIdParent)
      AddressPath addrId -> undefined
      -- TODO: Throw "Address con not be removed"
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
        let namesList = map unWalletName (toWalletNamesList (walletDb ^. dbHdWallets))
        when (name `elem` namesList)
          (throwM $ DuplicateWalletName (WalletName name))
        throwLeft_ $ update acidDb (UpdateHdRootName hdrId (WalletName name))

      AccountPath accId -> do
        accounts <- case
          readAccountsByRootId (accId ^. hdAccountIdParent) (walletDb ^. dbHdWallets) of
            Right accs -> return (map unwrapOrdByPrimKey (IxSet.toList $ unwrapIxSet accs))
            Left err -> throwM err
        let namesList = map (unAccountName . _hdAccountName) accounts
        when (name `elem` namesList)
          (throwM $ DuplicateAccountName (AccountName name))
        throwLeft_ $ update acidDb (UpdateHdAccountName accId (AccountName name))

      AddressPath _ -> throwM NotARenamableItem
  walletRefreshState

-- Helpers
throwLeft :: (IO a) -> (Either e a) -> (IO a)
throwLeft exep eith = case eith of
  Left _ -> exep
  Right r -> return r

throwLeft_ :: (Exception e) => IO (Either e a) -> IO ()
throwLeft_ ioEith = do
  eith <- ioEith
  case eith of
    Left err -> throwM err
    Right _ -> return ()

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
  ((^. hdRootName) . unwrapOrdByPrimKey) <$> (IxSet.toList $ unwrapIxSet (hdw ^. hdWalletsRoots))
