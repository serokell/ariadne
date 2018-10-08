{-- | An opaque handle to a keystore, used to read and write 'EncryptedSecretKey'
      from/to disk.

    NOTE: This module aims to provide a stable interface with a concrete
    implementation concealed by the user of this module. The internal operations
    are currently quite inefficient, as they have to work around the legacy
    'UserSecret' storage.

--}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ariadne.Wallet.Cardano.Kernel.Keystore
       ( Keystore -- opaque
       , DeletePolicy(..)
       , DuplicatedWalletKey(..)
         -- * Constructing a keystore
       , keystoreComponent
       , bracketKeystore
       , bracketLegacyKeystore
       -- * Inserting values
       , insert
       -- * Deleting values
       , delete
       -- * Queries on a keystore
       , lookup
       -- * Conversions
       , toList
       -- * Tests handy functions
       , bracketTestKeystore
       ) where

import Prelude hiding (toList)

import Control.Concurrent (modifyMVar_, withMVar)
import Control.Monad.Component (ComponentM, buildComponent)
import qualified Data.Map as Map
import System.Directory
  (createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

import Pos.Core (AddressHash, addressHash)
import Pos.Crypto (EncryptedSecretKey, PublicKey, encToPublic)
import Pos.Util.UserSecret
  (UserSecret, getUSPath, isEmptyUserSecret, peekUserSecret, usWallets,
  writeUserSecret)
import System.Wlog (CanLog(..), HasLoggerName(..), LoggerName(..), logMessage)

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet (HdRootId(..))
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..), fromDb)
import Ariadne.Wallet.Cardano.Kernel.Types (WalletId(..))

-- Internal storage necessary to smooth out the legacy 'UserSecret' API.
data InternalStorage = InternalStorage !UserSecret

-- A 'Keystore'.
data Keystore = Keystore (MVar InternalStorage)

-- | Internal monad used to smooth out the 'WithLogger' dependency imposed
-- by 'Pos.Util.UserSecret', to not commit to any way of logging things just yet.
newtype KeystoreM a = KeystoreM { fromKeystore :: IdentityT IO a }
                    deriving (Functor, Applicative, Monad, MonadIO)

instance HasLoggerName KeystoreM where
    askLoggerName = return (LoggerName "Keystore")
    modifyLoggerName _ action = action

instance CanLog KeystoreM where
    dispatchMessage _ln sev txt = logMessage sev txt

-- | A 'DeletePolicy' is a preference the user can express on how to release
-- the 'Keystore' during its teardown.
data DeletePolicy =
      RemoveKeystoreIfEmpty
      -- ^ Completely obliterate the 'Keystore' if is empty, including the
      -- file on disk.
    | KeepKeystoreIfEmpty
      -- ^ Release the 'Keystore' without touching its file on disk, even
      -- if the latter is empty.

data DuplicatedWalletKey = DuplicatedWalletKey
  deriving (Eq, Show)

instance Exception DuplicatedWalletKey where
  displayException DuplicatedWalletKey =
    "The wallet with this root key already exists"

{-------------------------------------------------------------------------------
  Creating a keystore
-------------------------------------------------------------------------------}

-- FIXME [CBR-316] Due to the current, legacy 'InternalStorage' being
-- used, the 'Keystore' does not persist the in-memory content on disk after
-- every destructive operations, which means that in case of a node crash or
-- another catastrophic behaviour (e.g. power loss, etc) the in-memory content
-- not yet written in memory will be forever lost.

keystoreComponent :: DeletePolicy -> FilePath -> ComponentM Keystore
keystoreComponent deletePolicy fp =
    buildComponent
        "Keystore"
        (newKeystore fp)
        (releaseKeystore deletePolicy)

-- | Creates a 'Keystore' using a 'bracket' pattern, where the
-- initalisation and teardown of the resource are wrapped in 'bracket'.
bracketKeystore :: DeletePolicy
                -- ^ What to do if the keystore is empty
                -> FilePath
                -- ^ The path to the file which will be used for the 'Keystore'
                -> (Keystore -> IO a)
                -- ^ An action on the 'Keystore'.
                -> IO a
bracketKeystore deletePolicy fp withKeystore =
    bracket (newKeystore fp) (releaseKeystore deletePolicy) withKeystore

-- | Creates a new keystore.
newKeystore :: FilePath -> IO Keystore
newKeystore fp = runIdentityT $ fromKeystore $ do
    us <- peekUserSecret fp
    Keystore <$> newMVar (InternalStorage us)

-- | Creates a legacy 'Keystore' by reading the 'UserSecret' from a 'NodeContext'.
-- Hopefully this function will go in the near future.
newLegacyKeystore :: UserSecret -> IO Keystore
newLegacyKeystore us = Keystore <$> newMVar (InternalStorage us)

-- | Creates a legacy 'Keystore' using a 'bracket' pattern, where the
-- initalisation and teardown of the resource are wrapped in 'bracket'.
-- For a legacy 'Keystore' users do not get to specify a 'DeletePolicy', as
-- the release of the keystore is left for the node and the legacy code
-- themselves.
bracketLegacyKeystore :: UserSecret -> (Keystore -> IO a) -> IO a
bracketLegacyKeystore us withKeystore =
    bracket (newLegacyKeystore us)
            (\_ -> pass) -- Leave teardown to the legacy wallet
            withKeystore

bracketTestKeystore :: (Keystore -> IO a) -> IO a
bracketTestKeystore withKeystore =
    bracket newTestKeystore
            (releaseKeystore RemoveKeystoreIfEmpty)
            withKeystore

-- | Creates a 'Keystore' out of a randomly generated temporary file (i.e.
-- inside your $TMPDIR of choice).
-- We don't offer a 'bracket' style here as the teardown is irrelevant, as
-- the file is disposed automatically from being created into the
-- OS' temporary directory.
-- NOTE: This 'Keystore', as its name implies, shouldn't be using in
-- production, but only for testing, as it can even possibly contain data
-- races due to the fact its underlying file is stored in the OS' temporary
-- directory.
newTestKeystore :: IO Keystore
newTestKeystore = liftIO $ runIdentityT $ fromKeystore $ do
    fp <- liftIO $ getKeystorePath
    us <- peekUserSecret fp
    Keystore <$> newMVar (InternalStorage us)
  where
    -- | Generates a new path for the temporary 'Keystore'.
    -- Unfortunately, System.IO does not expose an API for path
    -- generation (it is implemented in `openTempFile'`), so
    -- we create the file, then close the handle and remove it.
    -- It cannot be left empty since the `instance Bi` for
    -- 'UserSecret' cannot handle empty files.
    getKeystorePath :: IO FilePath
    getKeystorePath = do
        tempDir <- getTemporaryDirectory
        let dir = tempDir </> "ariadne-test"
        createDirectoryIfMissing False dir
        (fp, hdl) <- openTempFile dir "keystore.key"
        hClose hdl
        removeFile fp
        pure fp

-- | Release the resources associated with this 'Keystore'.
releaseKeystore :: DeletePolicy -> Keystore -> IO ()
releaseKeystore dp (Keystore ks) =
    -- We are not modifying the 'MVar' content, because this function is
    -- not exported and called exactly once from the bracket de-allocation.
    withMVar ks $ \internalStorage@(InternalStorage us) -> do
        fp <- release internalStorage
        case dp of
             KeepKeystoreIfEmpty   -> pass
             RemoveKeystoreIfEmpty ->
                 when (isEmptyUserSecret us) $ removeFile fp

-- | Releases the underlying 'InternalStorage' and returns the updated
-- 'InternalStorage' and the file on disk this storage lives in.
-- 'FilePath'.
release :: InternalStorage -> IO FilePath
release (InternalStorage us) = do
    let fp = getUSPath us
    writeUserSecret us
    return fp

{-------------------------------------------------------------------------------
  Inserting things inside a keystore
-------------------------------------------------------------------------------}

-- | Insert a new 'EncryptedSecretKey' indexed by the input 'WalletId'.
insert :: WalletId
       -> EncryptedSecretKey
       -> Keystore
       -> IO ()
insert _walletId esk (Keystore ks) = do
    modifyMVar_ ks $ \(InternalStorage us) -> do
        when (Map.member (eskToKey esk) (view usWallets us)) $
            throwM DuplicatedWalletKey
        let us' = us & over usWallets (Map.insert (addressHash $ encToPublic esk) esk)
        writeUserSecret us'
        return $ InternalStorage us'
    where
      eskToKey :: EncryptedSecretKey -> AddressHash PublicKey
      eskToKey = addressHash . encToPublic

{-------------------------------------------------------------------------------
  Looking up things inside a keystore
-------------------------------------------------------------------------------}

-- | Lookup an 'EncryptedSecretKey' associated to the input 'HdRootId'.
lookup :: WalletId
       -> Keystore
       -> IO (Maybe EncryptedSecretKey)
lookup wId (Keystore ks) =
    withMVar ks $ \(InternalStorage us) -> return $ lookupKey us wId

-- | Lookup a key directly inside the 'UserSecret'.
lookupKey :: UserSecret -> WalletId -> Maybe EncryptedSecretKey
lookupKey us walletId =
    Map.lookup (walletIdToKey walletId) (us ^. usWallets)

{-------------------------------------------------------------------------------
  Deleting things from the keystore
-------------------------------------------------------------------------------}

-- | Deletes an element from the 'Keystore'. This is an idempotent operation
-- as in case a key was not present, no error would be thrown.
delete :: WalletId -> Keystore -> IO ()
delete walletId (Keystore ks) = do
    modifyMVar_ ks $ \(InternalStorage us) -> do
        let us' = us & over usWallets (Map.delete (walletIdToKey walletId))
        writeUserSecret us'
        return (InternalStorage us')

{-------------------------------------------------------------------------------
  Converting a Keystore into container types
-------------------------------------------------------------------------------}

-- | Returns all the 'EncryptedSecretKey' known to this 'Keystore'.
toList :: Keystore -> IO [(WalletId, EncryptedSecretKey)]
toList (Keystore ks) =
    withMVar ks $ \(InternalStorage us) ->
        pure $ map (first hashPubKeyToWalletId) $ toPairs $ us ^. usWallets
  where
    hashPubKeyToWalletId :: AddressHash PublicKey -> WalletId
    hashPubKeyToWalletId = WalletIdHdSeq . HdRootId . InDb

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

walletIdToKey :: WalletId -> AddressHash PublicKey
walletIdToKey (WalletIdHdSeq hdRootId) = view fromDb $ getHdRootId hdRootId
