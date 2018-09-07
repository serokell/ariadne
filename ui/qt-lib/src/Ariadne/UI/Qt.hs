module Ariadne.UI.Qt
  ( UiFace(..)
  , createAriadneUI
  ) where

import Control.Concurrent
import Control.Monad.Component (ComponentM, buildComponent_)
import Control.Monad.Extra (loopM)

import Ariadne.UI.Qt.Face
  (UiEvent(..), UiFace(..), UiHistoryFace, UiLangFace, UiWalletFace)

import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Event as Event
import qualified Graphics.UI.Qtah.Gui.QFontDatabase as QFontDatabase
import qualified Graphics.UI.Qtah.Gui.QIcon as QIcon
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication

import Control.Concurrent.STM.TBQueue

import Ariadne.UI.Qt.MainWindow
import Ariadne.UI.Qt.StyleSheet
import Ariadne.UI.Qt.UI
import Ariadne.UX.PasswordManager

type UiAction = UiLangFace -> IO ()

type UiEventBQueue = TBQueue UiEvent

createAriadneUI
  :: UiWalletFace
  -> UiHistoryFace
  -> PutPassword
  -> ComponentM (UiFace, UiAction)
createAriadneUI uiWalletFace historyFace putPass = buildComponent_ "UI-Qt" $ do
  eventQueue <- mkEventBQueue
  dispatcherIORef :: IORef (Maybe QObject.QObject) <- newIORef Nothing
  return
    ( mkUiFace eventQueue dispatcherIORef
    , runUIEventLoop eventQueue dispatcherIORef uiWalletFace historyFace putPass
    )

fonts :: [Text]
fonts =
  [ "MuseoSansCyrl_100.otf"
  , "MuseoSansCyrl_300.otf"
  , "MuseoSansCyrl_500.otf"
  , "MuseoSansCyrl_700.otf"
  , "MuseoSansCyrl_900.otf"
  , "MuseoSansCyrl_italic_100.ttf"
  , "MuseoSansCyrl_italic_300.ttf"
  ]

runUIEventLoop
  :: UiEventBQueue
  -> IORef (Maybe QObject.QObject)
  -> UiWalletFace
  -> UiHistoryFace
  -> PutPassword
  -> UiAction
runUIEventLoop eventIORef dispatcherIORef uiWalletFace historyFace putPass langFace =
  runInBoundThread $ withScopedPtr (getArgs >>= QApplication.new) $ \app -> do
    QApplication.setStyleSheet app $ toString styleSheet
    QApplication.setWindowIcon app =<< QIcon.newWithFile (":/images/yarn-ic.png" :: String)

    forM_ fonts $ \font -> QFontDatabase.addApplicationFont $ toString $ ":/museo/" <> font

    eventDispatcher <- QObject.new
    writeIORef dispatcherIORef $ Just eventDispatcher
    mainWindow <- initMainWindow langFace uiWalletFace historyFace
    void $ Event.onEvent eventDispatcher $ \(_ :: QEvent.QEvent) ->
        handleAppEvent langFace putPass eventIORef mainWindow >> return True

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

{-# ANN qtEventSubLoop ("HLint: ignore Redundant return" :: Text) #-}
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

handleAppEvent
  :: UiLangFace
  -> PutPassword
  -> UiEventBQueue
  -> MainWindow
  -> IO ()
handleAppEvent langFace putPass eventBQueue mainWindow =
    loopM (qtEventSubLoop eventBQueue doHandleOneEvent) 5
  where
    doHandleOneEvent event = runUI (handleMainWindowEvent langFace putPass event) mainWindow
