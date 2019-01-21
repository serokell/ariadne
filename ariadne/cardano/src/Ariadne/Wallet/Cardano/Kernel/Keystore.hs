{-- | An opaque handle to a keystore, used to read and write 'EncryptedSecretKey'
      from/to disk.
--}

module Ariadne.Wallet.Cardano.Kernel.Keystore
       ( Keystore -- opaque
       , DeletePolicy(..)
       , DuplicatedWalletKey(..)
         -- * Constructing a keystore
       , keystoreComponent
       , bracketKeystore
       -- * Inserting values
       , insert
       -- * Deleting values
       , delete
       -- * Queries on a keystore
       , lookup
       -- * Updating keys
       , update
       -- * Conversions
       , toList
       , toMap
       -- * Tests handy functions
       , bracketTestKeystore
       ) where

import Prelude hiding (toList)

import Control.Concurrent (modifyMVar_, withMVar)
import Control.Monad.Component (ComponentM, buildComponent)
import qualified Data.Map as Map
import System.Directory (removeFile)

import Pos.Core (AddressHash, addressHash)
import Pos.Crypto (EncryptedSecretKey, PublicKey, encToPublic)

import Ariadne.Logging
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet (HdRootId(..))
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..))
import Ariadne.Wallet.Cardano.Kernel.Keystore.Persistence
  (getTempKeystorePath, peekInternalStorage, writeInternalStorage)
import Ariadne.Wallet.Cardano.Kernel.Keystore.Types
  (InternalStorage(..), isWallets)
import Ariadne.Wallet.Cardano.Kernel.Keystore.Util
  (eskToPublicHash, walletIdToPublicHash)
import Ariadne.Wallet.Cardano.Kernel.Types (WalletId(..))

data Keystore
  = Keystore !(MVar InternalStorage) !FilePath

{-------------------------------------------------------------------------------
  Creating a keystore
-------------------------------------------------------------------------------}

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

keystoreComponent :: Logging -> DeletePolicy -> FilePath -> ComponentM Keystore
keystoreComponent logging deletePolicy fp =
    buildComponent
        "Keystore"
        (newKeystore logging fp)
        (releaseKeystore deletePolicy)

-- | Creates a 'Keystore' using a 'bracket' pattern, where the
-- initalisation and teardown of the resource are wrapped in 'bracket'.
bracketKeystore :: Logging
                -- ^ Logging handle
                -> DeletePolicy
                -- ^ What to do if the keystore is empty
                -> FilePath
                -- ^ The path to the file which will be used for the 'Keystore'
                -> (Keystore -> IO a)
                -- ^ An action on the 'Keystore'.
                -> IO a
bracketKeystore logging deletePolicy fp withKeystore =
    bracket (newKeystore logging fp) (releaseKeystore deletePolicy) withKeystore

-- | Creates a new keystore.
newKeystore :: Logging -> FilePath -> IO Keystore
newKeystore logging fp = do
    istorage <- peekInternalStorage logging fp
    Keystore <$> newMVar istorage <*> pure fp

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
newTestKeystore = do
    fp <- getTempKeystorePath
    istorage <- peekInternalStorage noLogging fp
    Keystore <$> newMVar istorage <*> pure fp

-- | Release the resources associated with this 'Keystore'.
releaseKeystore :: DeletePolicy -> Keystore -> IO ()
releaseKeystore dp (Keystore mstorage fp) =
    -- We are not modifying the 'MVar' content, because this function is
    -- not exported and called exactly once from the bracket de-allocation.
    withMVar mstorage $ \istorage ->
        case dp of
             KeepKeystoreIfEmpty   -> pass
             RemoveKeystoreIfEmpty ->
                 when (isEmptyInternalStorage istorage) $ removeFile fp

{-------------------------------------------------------------------------------
  Inserting things inside a keystore
-------------------------------------------------------------------------------}

-- | Insert a new 'EncryptedSecretKey' indexed by the input 'WalletId'.
insert :: WalletId
       -> EncryptedSecretKey
       -> Keystore
       -> IO ()
insert _walletId esk (Keystore mstorage fp) = do
    modifyMVar_ mstorage $ \istorage -> do
        when (Map.member (eskToPublicHash esk) (view isWallets istorage)) $
            throwM DuplicatedWalletKey
        let istorage' = istorage & over isWallets (Map.insert (addressHash $ encToPublic esk) esk)
        writeInternalStorage fp istorage'
        return istorage'

{-------------------------------------------------------------------------------
  Looking up things inside a keystore
-------------------------------------------------------------------------------}

-- | Lookup an 'EncryptedSecretKey' associated to the input 'HdRootId'.
lookup :: WalletId
       -> Keystore
       -> IO (Maybe EncryptedSecretKey)
lookup wId (Keystore mstorage _) =
    withMVar mstorage $ \istorage -> return $ lookupKey istorage wId

-- | Lookup a key directly inside the 'InternalStorage'.
lookupKey :: InternalStorage -> WalletId -> Maybe EncryptedSecretKey
lookupKey istorage walletId =
    Map.lookup (walletIdToPublicHash walletId) (istorage ^. isWallets)

{-------------------------------------------------------------------------------
  Deleting things from the keystore
-------------------------------------------------------------------------------}

-- | Deletes an element from the 'Keystore'. This is an idempotent operation
-- as in case a key was not present, no error would be thrown.
delete :: WalletId -> Keystore -> IO ()
delete walletId (Keystore mstorage fp) = do
    modifyMVar_ mstorage $ \istorage -> do
        let istorage' = istorage & over isWallets (Map.delete (walletIdToPublicHash walletId))
        writeInternalStorage fp istorage'
        return istorage'

{------------------------------------------------------------------------------
  Updating values in keystore
-------------------------------------------------------------------------------}

-- | Updates EncryptedSecretyKey assotiated with WalletId
update :: WalletId -> EncryptedSecretKey -> Keystore -> IO ()
update walletId newEsk (Keystore mstorage fp) = do
    modifyMVar_ mstorage $ \istorage -> do
        let istorage' = istorage & over isWallets
              (Map.update (\_ -> Just newEsk) (walletIdToPublicHash walletId))
        writeInternalStorage fp istorage'
        return istorage'

{-------------------------------------------------------------------------------
  Converting a Keystore into container types
-------------------------------------------------------------------------------}

-- | Returns all the 'EncryptedSecretKey' known to this 'Keystore'.
toList :: Keystore -> IO [(WalletId, EncryptedSecretKey)]
toList keystore = Map.toList <$> toMap keystore

toMap :: Keystore -> IO (Map WalletId EncryptedSecretKey)
toMap (Keystore mstorage _) =
    withMVar mstorage $ \istorage ->
      pure $ Map.mapKeys hashPubKeyToWalletId $ istorage ^. isWallets

hashPubKeyToWalletId :: AddressHash PublicKey -> WalletId
hashPubKeyToWalletId = WalletIdHdSeq . HdRootId . InDb

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

-- | Whether stored InternalStorage has empty keys
isEmptyInternalStorage :: InternalStorage -> Bool
isEmptyInternalStorage istorage = null (istorage ^. isWallets)
