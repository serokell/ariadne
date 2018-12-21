-- | Logging component which is the primary way to log something from
-- `ariadne` code.

module Ariadne.Logging
       (
         -- * Logging handle and the corresponding component.
         Logging
       , loggingComponent

         -- * Functions to actually log something
       , logDebug
       , logInfo
       , logWarning
       , logError

         -- * Temporary logging
       , temporaryLog
       ) where

import Colog.Actions (logTextHandle)
import Colog.Core (LogAction(..), Severity(..), cmapM, (<&))
import Colog.Message
  (FieldMap, Message(..), RichMessage, defaultFieldMap, fmtRichMessageDefault,
  upgradeMessageAction)
import Control.Monad.Component (ComponentM, buildComponent)
import System.FilePath ((</>))
import System.IO (hClose, hFlush)
import System.IO.Unsafe (unsafePerformIO)

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
loggingComponent logDir =
    fst <$> buildComponent "Logging" (mkLogging logFile) snd
  where
    logFile = logDir </> "ariadne.log"

-- Private function, returns Logging handle and an action to release it.
mkLogging :: FilePath -> IO (Logging, IO ())
mkLogging logFile = do
    hdl <- openFile logFile AppendMode
    let logToFile :: LogAction IO (RichMessage IO)
        logToFile =
            cmapM fmtRichMessageDefault (logTextHandle hdl) <>
            logFlush hdl
    let logToFile' :: LogAction IO Message
        logToFile' = upgradeMessageAction fieldMap logToFile
    return (Logging logToFile', hClose hdl)
  where
    fieldMap :: FieldMap IO
    fieldMap = defaultFieldMap

-- A 'LogAction' which does not actually log anything, only
-- flushes the handle.
logFlush :: Handle -> LogAction IO a
logFlush hdl = LogAction $ const (hFlush hdl)

----------------------------------------------------------------------------
-- Temporary hacky logging
----------------------------------------------------------------------------

-- | Sometimes one needs to quickly add logging to some place,
-- e. g. to debug something. Propagating 'Logging' there might be
-- cumbersome and tedious. Doing it for temporary logging which is not
-- supposed to be merged to the integration branch does not make much
-- sense. This function can be used directly to log something from 'IO'.
-- However, it is not intended for long-term usage and its usage emits
-- a warning. The reasons are at least the following:
--
-- 1. It's not configurable (making it configurable would require adding
-- more constraints or arguments).
-- 2. It uses a global variable.
-- 3. Having more than one way to log something is confusing.
{-# WARNING temporaryLog "'temporaryLog' remains in code" #-}
temporaryLog :: MonadIO m => Text -> m ()
temporaryLog = logDebug globalLogging
{-# SPECIALIZE temporaryLog :: Text -> IO () #-}

globalLogging :: Logging
{-# NOINLINE globalLogging #-}
globalLogging = unsafePerformIO (fst <$> mkLogging "ariadne-tmp.log")
