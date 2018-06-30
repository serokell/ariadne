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
import Pos.Crypto
import Pos.Util (eitherToThrow, maybeThrow)
import Pos.Util.UserSecret

import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Wallet.Cardano.Kernel.Bip32
import Ariadne.Wallet.Cardano.Kernel.Bip44
  (Bip44DerivationPath(..), encodeBip44DerivationPath)
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

-- | Utility function that runs CatchT monad inside a StateT and rollbacks the state on failure
runCatchInState :: Functor m => StateT s (CatchT m) a -> StateT s m (Either SomeException a)
runCatchInState m = StateT $ \s ->
  runCatchT (runStateT m s) <&> \case
    Left e -> (Left e, s)
    Right (a, s') -> (Right a, s')

-- | Get the wallet index by name or using current selection.
resolveWalletRef
  :: IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> WalletReference
  -> IO Word
resolveWalletRef walletSelRef runCardanoMode = \case
  WalletRefSelection -> do
    mWalletSelection <- readIORef walletSelRef
    case mWalletSelection of
      Nothing -> throwM NoWalletSelection
      Just WalletSelection{..} -> return wsWalletIndex
  WalletRefByName (WalletName name) -> do
    us <- runCardanoMode getSecretDefault
    case findIndex (\w -> w ^. wdName == name) (us ^. usWallets) of
      Just i -> return (fromIntegral i)
      Nothing -> throwM $ WalletDoesNotExist name
  WalletRefByIndex i -> return i

-- | Like 'resolveWalletRef', but for accounts.
resolveAccountRef
  :: IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> AccountReference
  -> IO (Word, Word32)
resolveAccountRef walletSelRef runCardanoMode accountRef = do
    walletIdx <- resolveWalletRef walletSelRef runCardanoMode walletRef
    (walletIdx, ) <$> resolveAccountIdx walletIdx
  where
    walletRef =
        case accountRef of
            AccountRefSelection -> WalletRefSelection
            AccountRefByIndex _ wr -> wr
            AccountRefByName _ wr -> wr
    resolveAccountIdx walletIdx =
        case accountRef of
            AccountRefSelection -> do
                mWalletSelection <- readIORef walletSelRef
                case mWalletSelection of
                    Nothing -> throwM NoWalletSelection
                    Just WalletSelection {..} ->
                        case head <$> nonEmpty wsPath of
                            Nothing -> throwM NoAccountSelection
                            Just accIdx -> return (fromIntegral accIdx)
            AccountRefByIndex accIdx _ -> return accIdx
            AccountRefByName accName _ -> do
                us <- runCardanoMode getSecretDefault
                walletData <-
                    maybeThrow
                        (WalletDoesNotExist (pretty walletIdx))
                        (us ^? usWallets . ix (fromIntegral walletIdx))
                case V.findIndex
                         (\ad -> ad ^. adName == accName)
                         (walletData ^. wdAccounts) of
                    Just i -> return (fromIntegral i)
                    Nothing -> throwM $ AccountDoesNotExist accName

refreshUserSecret
  :: IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> (WalletEvent -> IO ())
  -> IO ()
refreshUserSecret walletSelRef runCardanoMode sendWalletEvent = do
  walletSel <- readIORef walletSelRef
  us <- runCardanoMode getSecretDefault
  sendWalletEvent (WalletUserSecretSetEvent us walletSel)

newAddress ::
       WalletFace
    -> IORef (Maybe WalletSelection)
    -> (CardanoMode ~> IO)
    -> AccountReference
    -> PassPhrase
    -> IO ()
newAddress WalletFace {..} walletSelRef runCardanoMode accRef pp = do
    (walletIdx, accountIdx) <-
        resolveAccountRef walletSelRef runCardanoMode accRef
    let wIdx, accIdx :: Int
        wIdx = fromIntegral walletIdx
        accIdx = fromIntegral accountIdx
        addAddressPure :: StateT UserSecret Catch ()
        addAddressPure = do
            walletData <-
                maybeThrow (WalletDoesNotExist (pretty walletIdx)) =<<
                preuse (usWallets . ix wIdx)
            accountData <-
                maybeThrow (AccountDoesNotExist $ show (walletIdx, accountIdx)) $
                walletData ^?
                wdAccounts .
                ix accIdx
            let addrIdx = findFirstUnique (_adLastIndex accountData) (fmap fst $ accountData ^. adAddresses)
            -- FIXME: support not only bootstrap era
            addr <-
                case deriveLvl2KeyPair
                         (IsBootstrapEraAddr True)
                         (ShouldCheckPassphrase True)
                         pp
                         (walletData ^. wdRootKey)
                         accountIdx
                         addrIdx of
                    Nothing -> throwM AGFailedIncorrectPassPhrase
                    Just (a, _) -> pure a
            usWallets . ix wIdx . wdAccounts . ix accIdx . adAddresses <>=
                one (addrIdx, addr)
            usWallets . ix wIdx . wdAccounts . ix accIdx . adLastIndex .= succ addrIdx
    runCardanoMode (modifySecretDefault (runCatchInState addAddressPure)) >>=
        eitherToThrow
    walletRefreshUserSecret

mkUntitled :: Text -> Vector Text -> Text
mkUntitled untitled namesVec =
  let
    untitledSuffixes = V.mapMaybe (T.stripPrefix $ untitled) namesVec
    numbers = V.mapMaybe ((readMaybe @Natural) . T.unpack) untitledSuffixes
  in if null untitledSuffixes || null numbers
    then untitled <> "0"
    else untitled <> (show $ (Universum.maximum numbers) + 1)

newAccount
  :: WalletFace
  -> IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> WalletReference
  -> Maybe Text
  -> IO ()
newAccount WalletFace{..} walletSelRef runCardanoMode walletRef mbAccountName = do
  wsWalletIndex <- resolveWalletRef walletSelRef runCardanoMode walletRef

  let wIdx :: Int
      wIdx = fromIntegral wsWalletIndex

      addAccountPure :: StateT UserSecret Catch ()
      addAccountPure = do
        wd <-
          maybeThrow (WalletDoesNotExist (pretty wsWalletIndex)) =<<
          preuse (usWallets . ix wIdx)

        let namesVec = _adName <$> _wdAccounts wd

        accountName <- case mbAccountName of
          Nothing ->
            return (mkUntitled "Untitled account " namesVec)
          Just accountName_ -> do
            when (accountName_ `elem` namesVec) $ throwM $ DuplicateAccountName accountName_
            return accountName_
        let newIdx = findFirstUnique (_wdLastIndex wd) (map _adPath $ _wdAccounts wd)
        usWallets . ix wIdx . wdAccounts %= \account ->
          addAccountToVec accountName newIdx account
        usWallets . ix wIdx . wdLastIndex .= succ newIdx

  runCardanoMode (modifySecretDefault (runCatchInState addAccountPure)) >>=
    eitherToThrow
  walletRefreshUserSecret
  where
    addAccountToVec :: Text -> Word32 -> Vector AccountData -> Vector AccountData
    addAccountToVec accountName idx accounts =
        accounts <>
        one
            AccountData
                    { _adName = accountName
                    , _adLastIndex = firstNonHardened
                    , _adPath = idx
                    , _adAddresses = mempty
                    }

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
       WalletConfig
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> PassPhrase
    -> Maybe WalletName
    -> Maybe Byte
    -> IO [Text]
newWallet walletConfig face runCardanoMode pp mbWalletName mbEntropySize = do
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
  mnemonic ++ ["ariadne-v0"] <$ addWallet face runCardanoMode esk mbWalletName mempty

-- | Construct a wallet from given data and add it to the storage.
addWallet ::
       WalletFace
    -> (CardanoMode ~> IO)
    -> EncryptedSecretKey
    -> Maybe WalletName
    -> Vector AccountData
    -> IO ()
addWallet WalletFace {..} runCardanoMode esk mbWalletName accounts = do
  let addWalletPure :: StateT UserSecret Catch ()
      addWalletPure = do
        wdList <- use usWallets

        let namesList = _wdName <$> wdList
            keysList = encToPublic . _wdRootKey <$> wdList

        walletName <- case mbWalletName of
          Nothing ->
            return (WalletName $ mkUntitled "Untitled wallet " (V.fromList namesList))
          Just walletName_ -> do
            when (unWalletName walletName_ `elem` namesList) $
              throwM $ DuplicateWalletName walletName_
            when (encToPublic esk `elem` keysList) $
              throwM DuplicatedWalletKey
            return walletName_
        usWallets <>= one (walletData walletName)

  runCardanoMode (modifySecretDefault (runCatchInState addWalletPure)) >>=
    eitherToThrow
  walletRefreshUserSecret
  where
    walletData :: WalletName -> WalletData
    walletData (WalletName walletName) =
        WalletData
            { _wdRootKey = esk
            , _wdLastIndex = firstHardened
            , _wdName = walletName
            , _wdAccounts = accounts
            }

select
  :: WalletFace
  -> IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> Maybe WalletReference
  -> [Word]
  -> IO ()
select WalletFace{..} walletSelRef runCardanoMode mWalletRef wsPath = do
  case mWalletRef of
    Nothing -> atomicWriteIORef walletSelRef Nothing
    Just walletRef -> do
      wsWalletIndex <- resolveWalletRef walletSelRef runCardanoMode walletRef
      us <- runCardanoMode getSecretDefault
      -- validate wallet
      wallet <- maybeThrow
        (WalletDoesNotExist $ pretty wsWalletIndex)
        (us ^? usWallets . ix (fromIntegral wsWalletIndex))
      case nonEmpty wsPath of
        Nothing -> return ()
        Just (accIdx :| acPath) -> do

          -- validate account
          _account <- maybeThrow
            (AccountDoesNotExist $ pretty accIdx)
            (wallet ^? wdAccounts . ix (fromIntegral accIdx))

          case nonEmpty acPath of
            Nothing -> return ()
            Just _ -> throwM SelectIsTooDeep

      atomicWriteIORef walletSelRef $ Just $
        WalletSelection { wsPath, wsWalletIndex }
  walletRefreshUserSecret

getSelectedAddresses :: WalletFace -> IORef (Maybe WalletSelection) -> (CardanoMode ~> IO) -> IO [Address]
getSelectedAddresses WalletFace{..} walletSelRef runCardanoMode = do
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> return []
    Just WalletSelection{..} -> do
      us <- runCardanoMode getSecretDefault
      wallet <- maybeThrow
        (WalletDoesNotExist $ pretty wsWalletIndex)
        (us ^? usWallets . ix (fromIntegral wsWalletIndex))
      case nonEmpty wsPath of
        Nothing -> return (concat . map (toList . map snd . _adAddresses) $ wallet ^. wdAccounts)
        Just (accIdx :| acPath) -> do
          account <- maybeThrow
            (AccountDoesNotExist $ pretty accIdx)
            (wallet ^? wdAccounts . ix (fromIntegral accIdx))
          case nonEmpty acPath of
            Nothing -> return (toList . map snd $ account ^. adAddresses)
            Just (addrIdx :| []) -> maybeThrow
              (AddressDoesNotExist $ pretty addrIdx)
              ((:[]) <$> account ^? adAddresses . ix (fromIntegral addrIdx) . _2)
            Just (_ :| _) -> throwM SelectIsTooDeep

removeSelection :: WalletFace -> IORef (Maybe WalletSelection) -> (CardanoMode ~> IO) -> IO ()
removeSelection WalletFace{..} walletSelRef runCardanoMode = do
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> pure ()
    Just WalletSelection{..} -> do
      runCardanoMode $ modifySecretDefault $ do
        case wsPath of
          [] -> usWallets %= deleteNthList wsWalletIndex
          (accIdx:acPath) -> zoom (usWallets . ix (fromIntegral wsWalletIndex)) $
            case nonEmpty acPath of
              Nothing -> wdAccounts %= deleteNth accIdx
              Just (addrIdx :| []) -> zoom (wdAccounts . ix (fromIntegral accIdx)) $
                adAddresses %= deleteNth addrIdx
              -- Our UI does not allow for selection paths deeper than 3.
              _ -> error "removeSelection: selection path is deeper than 3"
  atomicModifyIORef' walletSelRef $ (, ()) . (\case
    Nothing -> Nothing
    Just (WalletSelection _ []) -> Nothing
    Just (WalletSelection i (x:xs)) -> Just . WalletSelection i $ NE.init (x :| xs)
    )
  walletRefreshUserSecret
  where
    deleteNth n = V.ifilter (\i _ -> i /= fromIntegral n)
    deleteNthList n xs = take (fromIntegral n) xs ++ drop (fromIntegral n + 1) xs

renameSelection :: WalletFace -> IORef (Maybe WalletSelection) -> (CardanoMode ~> IO) -> Text -> IO ()
renameSelection WalletFace{..} walletSelRef runCardanoMode name = do
  mWalletSel <- readIORef walletSelRef
  case mWalletSel of
    Nothing -> pure ()
    Just WalletSelection{..} -> do
      let
        rename :: StateT UserSecret Catch ()
        rename = do
          walletNames <- fmap (^. wdName) <$> use usWallets
          zoom (usWallets . ix (fromIntegral wsWalletIndex)) $
            case wsPath of
              [] -> if name `elem` walletNames
                then throwM $ DuplicateWalletName (WalletName name)
                else wdName .= name
              (accIdx:acPath) -> do
                accountNames <- fmap (^. adName) <$> use wdAccounts
                zoom (wdAccounts . ix (fromIntegral accIdx)) $
                  case acPath of
                    [] -> if name `elem` accountNames
                      then throwM $ DuplicateAccountName name
                      else adName .= name
                    _ -> throwM $ NotARenamableItem
      runCardanoMode (modifySecretDefault . runCatchInState $ rename) >>=
        eitherToThrow
  walletRefreshUserSecret


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
