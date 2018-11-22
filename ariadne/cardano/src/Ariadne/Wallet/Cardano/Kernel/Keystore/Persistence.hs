-- | System file operations related to key-store InternalStorage

module Ariadne.Wallet.Cardano.Kernel.Keystore.Persistence
       ( getTempKeystorePath
       , writeInternalStorage
       , peekInternalStorage
       , readInternalStorage
       ) where

import Control.Exception.Safe (finally)
import Data.Aeson (eitherDecode, encode)
import Data.Default (Default(..))
import System.Directory
  (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, renameFile)
import System.FileLock (SharedExclusive(..), withFileLock)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose, openBinaryTempFile)

import qualified Data.ByteString.Lazy as LBS (hPut, readFile)

import Ariadne.System.FileMode (ensureModeIs600)
import Ariadne.Wallet.Cardano.Kernel.Keystore.Types
  (InternalStorage(..), StorageDecodingError(..), Versioned(..))


lockFilePath :: FilePath -> FilePath
lockFilePath = (<> ".lock")

-- | Creates empty temporary storage file for keyStore testing purposes
getTempKeystorePath :: MonadIO m => m FilePath
getTempKeystorePath = liftIO $ do
    tempDir <- liftIO getTemporaryDirectory
    let dir = tempDir </> "ariadne-test"
    let fp = dir </> "keystore.json"
    createDirectoryIfMissing False dir
    createEmptyFile fp $> fp

createEmptyFile :: FilePath -> IO ()
createEmptyFile fp = writeFile fp mempty

-- | Creates empty keystore file and initialise user secret from default value
initialiseInternalStorage :: FilePath -> IO InternalStorage
initialiseInternalStorage fp =
       createEmptyFile fp
    *> ensureModeIs600 fp
    *> def

-- | Reads InternalStorage from the given file.
-- If the file does not exist or is empty return a default InternalStorage instance
peekInternalStorage :: FilePath -> IO InternalStorage
peekInternalStorage fp = do
    exists <- doesFileExist fp
    if exists then
        readInternalStorage fp
    else
        initialiseInternalStorage fp

-- | Reads InternalStorage from file, assuming that file exists,and has mode 600.
-- If file exists but is empty return the default InternalStorage instance.
-- Throws exception in other case
readInternalStorage :: FilePath -> IO InternalStorage
readInternalStorage fp = do
    withReadLock fp $ do
        content <- LBS.readFile fp
        if null content then
            pure def
        else do
            either (throwM . StorageDecodingError . toText)
                   (\(Versioned istorage) -> pure istorage)
                   (eitherDecode content)

-- | writes InternalStorage to file
writeInternalStorage :: FilePath -> InternalStorage -> IO ()
writeInternalStorage fp istorage = do
    withFileLock (lockFilePath fp) Exclusive $ const (writeRaw fp istorage)

writeRaw :: FilePath -> InternalStorage -> IO ()
writeRaw path istorage = do
    -- On POSIX platforms, openBinaryTempFile guarantees that the file
    -- will be created with mode 600.
    -- If openBinaryTempFile throws, we want to propagate this exception,
    -- hence no handler.
    (tempPath, tempHandle) <-
        openBinaryTempFile (takeDirectory path) (takeFileName path)

    LBS.hPut tempHandle (encode $ Versioned istorage) `finally` hClose tempHandle
    renameFile tempPath path


-- | Helper for taking shared lock on file
withReadLock :: MonadIO m => FilePath -> IO a -> m a
withReadLock path = liftIO . withFileLock (lockFilePath path) Shared . const
