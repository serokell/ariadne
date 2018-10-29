{-# LANGUAGE RankNTypes #-}

-- | Some utilities for working with acid-state
module Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState
       ( -- * Acid-state updates with support for errors
         Update'
       , runUpdate'
       , runUpdateNoErrors
       , mapUpdateErrors
         -- * Zooming
       , zoom
       , zoomDef
       , zoomCreate
       , zoomAll
         -- ** Convenience re-exports
       , throwError
       , cleanupAcidState
       , runPeriodically
       ) where

import Data.Acid (AcidState(..), Update)
import Control.Monad.Except (Except, runExcept,throwError , withExcept)
import Formatting (sformat, shown, (%))
import System.Wlog (Severity(..))
import System.Directory (getModificationTime, listDirectory, removeFile)
import System.FilePath ((</>))
import Time(Time(..), KnownDivRat, Rat, Second, threadDelay)

import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet (Indexable, IxSet)
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet as IxSet

{-------------------------------------------------------------------------------
  Acid-state updates with support for errors (and zooming, see below)
-------------------------------------------------------------------------------}

type Update' st e = StateT st (Except e)

runUpdate' :: forall e st a. Update' st e a -> Update st (Either e a)
runUpdate' upd = do
    st <- get
    case upd' st of
      Left  e        -> return (Left e)
      Right (a, st') -> put st' >> return (Right a)
  where
    upd' :: st -> Either e (a, st)
    upd' = runExcept . runStateT upd

runUpdateNoErrors :: Update' st Void a -> Update st a
runUpdateNoErrors = fmap mustBeRight . runUpdate'

mapUpdateErrors :: (e -> e') -> Update' st e a -> Update' st e' a
mapUpdateErrors f upd = StateT $ withExcept f . runStateT upd

{-------------------------------------------------------------------------------
  Zooming
-------------------------------------------------------------------------------}

-- | Run an update on part of the state.
zoom :: Lens' st st' -> Update' st' e a -> Update' st e a
zoom l upd = StateT $ \large -> do
    let update small' = large & l .~ small'
        small         = large ^. l
    fmap update <$> runStateT upd small

-- | Run an update on part of the state.
--
-- If the specified part does not exist, run the default action.
zoomDef :: Update' st  e a      -- ^ Run when lens returns 'Nothing'
        -> Lens' st (Maybe st') -- ^ Index the state
        -> Update' st' e a      -- ^ Action to run on the smaller state
        -> Update' st  e a
zoomDef def l upd = StateT $ \large -> do
    let update small' = large & l .~ Just small'
        mSmall        = large ^. l
    case mSmall of
      Nothing    -> runStateT def large
      Just small -> fmap update <$> runStateT upd small


-- | Run an update on part of the state.
--
-- If the specified part does not exist, use the default provided,
-- then only apply the update.
zoomCreate :: st'                   -- ^ Default state
           -> Lens' st (Maybe st')  -- ^ Index the state
           -> Update' st' e a       -- ^ Action to run on the smaller state
           -> Update' st  e a
zoomCreate def l upd = StateT $ \large -> do
    let update small' = large & l .~ Just small'
        small         = fromMaybe def (large ^. l)
    fmap update <$> runStateT upd small

-- | Run an update on /all/ parts of the state.
--
-- This is used for system initiated actions which should not fail (such as
-- 'applyBlock', which is why the action we run must be a pure function.
zoomAll :: Indexable st'
        => Lens' st (IxSet st') -> (st' -> st') -> Update' st e ()
zoomAll l upd = StateT $ \large -> do
    let update ixset' = large & l .~ ixset'
        ixset         = large ^. l
    return $ ((), update $ IxSet.omap upd ixset)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mustBeRight :: Either Void b -> b
mustBeRight (Left  a) = absurd a
mustBeRight (Right b) = b

type MonadAcidCleanup m =
    ( MonadIO m
    , MonadMask m
    )


-- | Helper function to run action periodically.
runPeriodically :: forall (unit :: Rat) m a. (KnownDivRat unit Second, MonadIO m)
  => Time unit -- ^ time between performing action
  -> m a       -- ^ action
  -> m ()
runPeriodically delay action = forever $ do
  _ <- action
  threadDelay delay


-- | Creates checkpoint of the current state of Acid DB,
-- archive previous checkpoints and remove too old archives.
-- This is needed to avoid storing all logs permanently.
cleanupAcidState ::
       forall m st. (MonadAcidCleanup m)
    => AcidState st
    -> FilePath
    -> Int
    -> (Severity -> Text -> m ())
    -> m ()
cleanupAcidState db path numberOfStoredArchives writeLog = perform
  where
    perform = cleanupAction `catchAny` handler

    cleanupAction = do
        writeLog Debug "Starting cleanup"
        -- checkpoint/archive
        liftIO $ createCheckpoint db >> createArchive db
        writeLog Debug "Created checkpoint/archived"
        -- cleanup old archive data
        void $ flip catchAny (\e -> putStrLn @Text $ "Got error while cleaning up archive: " <> show e) $ do
            removed <- liftIO $ cleanupOldAcidArchives path numberOfStoredArchives
            writeLog Debug $ "Removed " <> pretty removed <> " old archive files"
        pass

    handler :: SomeException -> m ()
    handler e = do
        let report = do
                writeLog Error $ sformat ("acidCleanupWorker failed with error: "%shown%
                                           " restarting in 1m") e
                threadDelay @Second 60
        report `finally` perform

-- Returns how many files were deleted
cleanupOldAcidArchives ::
  FilePath  -- ^ path with stored files
  -> Int    -- how many files should be stored
  -> IO Int -- how many files have been deleted
cleanupOldAcidArchives dbPath numOfSaved = do
  let archiveDir = dbPath </> "Archive"
  archiveCheckpoints <- map (archiveDir </>) <$> listDirectory archiveDir
      -- same files, but newest first
  newestFirst <-
    map fst . reverse . sortWith snd <$>
      mapM (\f -> (f,) <$> liftIO (getModificationTime f)) archiveCheckpoints
  let oldFiles = drop numOfSaved newestFirst
  forM_ oldFiles removeFile
  print $ length oldFiles
  pure $ length oldFiles
