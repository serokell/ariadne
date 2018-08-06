module Ariadne.UI.Qt
  ( UiFace(..)
  , createAriadneUI
  ) where

import Universum

import Control.Concurrent
import Control.Monad.Extra (loopM)

import Ariadne.UI.Qt.Face (UiEvent(..), UiFace(..), UiHistoryFace, UiLangFace)

import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Event as Event
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication

import Control.Concurrent.STM.TBQueue

import Ariadne.UI.Qt.MainWindow
import Ariadne.UI.Qt.UI

type UiAction = UiLangFace -> IO ()

type UiEventBQueue = TBQueue UiEvent

createAriadneUI :: UiHistoryFace -> IO (UiFace, UiAction)
createAriadneUI historyFace = do
  eventQueue <- mkEventBQueue
  dispatcherIORef :: IORef (Maybe QObject.QObject) <- newIORef Nothing
  return (mkUiFace eventQueue dispatcherIORef, runUIEventLoop eventQueue dispatcherIORef historyFace)

runUIEventLoop :: UiEventBQueue -> IORef (Maybe QObject.QObject) -> UiHistoryFace -> UiAction
runUIEventLoop eventIORef dispatcherIORef historyFace langFace =
  runInBoundThread $ withScopedPtr (getArgs >>= QApplication.new) $ \_ -> do
    eventDispatcher <- QObject.new
    writeIORef dispatcherIORef $ Just eventDispatcher
    mainWindow <- initMainWindow langFace historyFace
    void $ Event.onEvent eventDispatcher $
      \(_ :: QEvent.QEvent) -> handleAppEvent langFace eventIORef mainWindow >> return True

    QCoreApplication.exec

mkEventBQueue :: IO (UiEventBQueue)
mkEventBQueue = newTBQueueIO 100

postEventToQt :: QObject.QObject -> IO ()
postEventToQt eventDispatcher = QEvent.new QEvent.None >>= QCoreApplication.postEvent eventDispatcher

-- Create the API for interacting with the UI thread.
mkUiFace :: UiEventBQueue -> IORef (Maybe QObject.QObject) -> UiFace
mkUiFace eventBQueue dispatcherIORef =
  UiFace
    { putUiEvent = \event -> do
        -- Cardano backend can initialize and start sending events
        -- before Qt thread is started, populationg dispatcherIORef.
        -- Therefore we need to cache some events for later processing.
        atomically $ writeTBQueue eventBQueue event
        whenJustM (readIORef dispatcherIORef) postEventToQt
    }

qtEventSubLoop :: UiEventBQueue -> (UiEvent -> IO ()) -> Int -> IO (Either Int ())
qtEventSubLoop _ _ 0 = return $ Right ()
qtEventSubLoop eventBQueue handler depth = do
  maybeEvent <- atomically $ tryReadTBQueue eventBQueue
  next <- case maybeEvent of
            Nothing -> return $ Right ()
            Just event -> handler event >> (return $ Left $ depth - 1)
  -- This method is missing from qtah. If we notice lags here, add it to qtah and uncomment
  -- QCoreApplication.processEvents
  return next

handleAppEvent :: UiLangFace -> UiEventBQueue -> MainWindow -> IO ()
handleAppEvent langFace eventBQueue mainWindow = loopM (qtEventSubLoop eventBQueue doHandleOneEvent) 5
  where
    doHandleOneEvent event = runUI (handleMainWindowEvent langFace event) mainWindow
