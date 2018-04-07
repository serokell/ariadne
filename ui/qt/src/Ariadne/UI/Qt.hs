module Ariadne.UI.Qt
  ( UiFace(..)
  , createAriadneUI
  ) where

import Universum

import Control.Monad.Extra (loopM)

import Ariadne.UI.Qt.Face (UiEvent(..), UiFace(..), UiLangFace)

import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Event as Event
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

import Control.Concurrent.STM.TBQueue

import Ariadne.UI.Qt.MainWindow
import Ariadne.UI.Qt.MainWindow.Events
import Ariadne.UI.Qt.MainWindow.Ui

type UiAction = UiLangFace -> IO ()

type UiEventBQueue = TBQueue UiEvent

createAriadneUI :: IO (UiFace, UiAction)
createAriadneUI = do
  eventQueue <- mkEventBQueue
  dispatcherIORef :: IORef (Maybe QObject.QObject) <- newIORef Nothing
  return (mkUiFace eventQueue dispatcherIORef, runUIEventLoop eventQueue dispatcherIORef)

runUIEventLoop :: UiEventBQueue -> IORef (Maybe QObject.QObject) -> UiLangFace -> IO ()
runUIEventLoop eventIORef dispatcherIORef langFace =
  withScopedPtr (getArgs >>= QApplication.new) $ \_ -> do
    eventDispatcher <- QObject.new
    writeIORef dispatcherIORef $ Just eventDispatcher
    windowRec@MainWindow{mwMainWindow = mwMainWindow} <- mkMainWindow langFace
    void $ Event.onEvent eventDispatcher $
        \(_ :: QEvent.QEvent) -> handleAppEvent eventIORef windowRec >> return True

    QWidget.show $ mwMainWindow

    QCoreApplication.exec

mkEventBQueue :: IO (UiEventBQueue)
mkEventBQueue = newTBQueueIO 100

postEventToQt :: QObject.QObject -> IO ()
postEventToQt eventDispatcher = QEvent.new QEvent.None >>= QCoreApplication.postEvent eventDispatcher

-- Create the API for interacting with the UI thread.
mkUiFace :: UiEventBQueue -> IORef (Maybe QObject.QObject) -> UiFace
mkUiFace eventBQueue dispatcherIORef =
  UiFace
    { putUiEvent = \event -> whenJustM (readIORef dispatcherIORef) $ \eventDispatcher -> do
        postEventToQt eventDispatcher
        atomically $ writeTBQueue eventBQueue event
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

handleAppEvent :: UiEventBQueue -> MainWindow -> IO ()
handleAppEvent eventBQueue window = loopM (qtEventSubLoop eventBQueue doHandleOneEvent) 5
    where
        doHandleOneEvent event = runUI (uiEventHandler event) window
