-- | Logging component which is the primary way to log something from
-- `ariadne` code.

module Ariadne.Logging
       (
         -- | Logging handle and the corresponding component.
         Logging
       , loggingComponent

         -- | Functions to actually log something
       , logDebug
       , logInfo
       , logWarning
       , logError
       ) where

import Colog.Actions (logTextHandle)
import Colog.Core (LogAction(..), Severity(..), cmap, (<&))
import Colog.Message (Message(..), fmtMessage)
import Control.Monad.Component (ComponentM, buildComponent)
import System.FilePath ((</>))
import System.IO (hClose, hFlush)

-- | An opaque type with everything needed to do logging.
newtype Logging = Logging
    { unLogging :: LogAction IO Message
    }

-- | Log a message with 'Debug' severity using 'Logging' handle.
logDebug :: (MonadIO m, HasCallStack) => Logging -> Text -> m ()
logDebug env = withFrozenCallStack $ log' env Debug

-- | Log a message with 'Info' severity using 'Logging' handle.
logInfo :: (MonadIO m, HasCallStack) => Logging -> Text -> m ()
logInfo env = withFrozenCallStack $ log' env Info

-- | Log a message with 'Warning' severity using 'Logging' handle.
logWarning :: (MonadIO m, HasCallStack) => Logging -> Text -> m ()
logWarning env = withFrozenCallStack $ log' env Warning

-- | Log a message with 'Error' severity using 'Logging' handle.
logError :: (MonadIO m, HasCallStack) => Logging -> Text -> m ()
logError env = withFrozenCallStack $ log' env Error

-- Internal helper function.
-- Maybe we should export it, but I don't like its name.
-- I'd like to call it just `log`, but we are using an old version of
-- `universum` which exports another function called `log`.
log' :: (MonadIO m, HasCallStack) => Logging -> Severity -> Text -> m ()
log' logging messageSeverity messageText =
    liftIO $ withFrozenCallStack (unLogging logging <& msg callStack)
  where
    msg messageStack = Message {..}

-- | Create a 'Logging' handle using 'ComponentM' interface for RAII.
--
-- The resulting logging behaves as follows:
-- 1. Messages are appended to a file inside the directory which is passed
-- as an argument to this function.
-- 2. Messages are formatted using 'fmtMessage' function from 'co-log'.
loggingComponent :: FilePath -> ComponentM Logging
loggingComponent logFile = fst <$> buildComponent "Logging" mkLogging snd
  where
    mkLogging :: IO (Logging, IO ())
    mkLogging = do
        hdl <- openFile (logFile </> "ariadne.log") AppendMode
        let logToFile = cmap fmtMessage (logTextHandle hdl) <> logFlush hdl
        return (Logging logToFile, hClose hdl)

-- A 'LogAction' which does not actually log anything, only
-- flushes the handle.
logFlush :: Handle -> LogAction IO a
logFlush hdl = LogAction $ const (hFlush hdl)
