module Ariadne.System.FileMode
       ( ensureModeIs600
       ) where

import System.Info (os)

import qualified System.Posix.Files as PSX
import qualified System.Posix.Types as PSX (FileMode)

-- | It's common practice to make keyfiles readable/writable for user only.
-- For now keyfile is read and write only
mode600 :: PSX.FileMode
mode600 = PSX.unionFileModes PSX.ownerReadMode PSX.ownerWriteMode

-- | Return only the access part of the file mode (like owner:rw-, etc).
getAccessMode :: MonadIO m => FilePath -> m PSX.FileMode
getAccessMode path = do
    mode <- liftIO $ PSX.fileMode <$> PSX.getFileStatus path
    return $ PSX.intersectFileModes mode PSX.accessModes

-- | Set mode regardless of the current mode of the file.
setMode :: MonadIO m => PSX.FileMode -> FilePath -> m ()
setMode mode path = liftIO $ PSX.setFileMode path mode

-- | Perform an action only on POSIX systems.
whenPosix :: Monad m => m () -> m ()
whenPosix fn = case os of
  "windows" -> pass
  _         -> fn

-- | Set given mode if needed.
ensureModeIs
    :: (MonadIO m)
    => (Text -> m ()) -> PSX.FileMode -> FilePath -> m ()
ensureModeIs logWarning mode path = do
    accessMode <- getAccessMode path
    unless (accessMode == mode) $ do
        logWarning . fromString $
            "Key file at " <> path <> " has access mode " <> show accessMode <>
            " instead of 600. Fixing it automatically."
        setMode mode path

ensureModeIs600
    :: (MonadIO m)
    => (Text -> m ()) -> FilePath -> m ()
ensureModeIs600 logWarning = whenPosix . ensureModeIs logWarning mode600
