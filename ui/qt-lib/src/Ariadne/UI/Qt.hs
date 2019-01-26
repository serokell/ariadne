module Ariadne.UI.Qt
       ( UiFace(..)
       , createAriadneUI
       ) where

import Control.Concurrent
import Control.Monad.Component (ComponentM, buildComponent_)
import Control.Monad.Extra (loopM)

import Ariadne.UI.Qt.Face
  (UiEvent(..), UiFace(..), UiHistoryFace, UiLangFace, UiSettings(..),
  UiWalletFace)

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
  -> Bool
  -> ComponentM (UiFace, UiAction)
createAriadneUI uiWalletFace historyFace putPass noConfirm = buildComponent_ "UI-Qt" $ do
  eventQueue <- mkEventBQueue
  dispatcherIORef :: IORef (Maybe QObject.QObject) <- newIORef Nothing
  settings <- newIORef $ UiSettings { uiNoConfirm = noConfirm }
  return
    ( mkUiFace eventQueue dispatcherIORef settings
    , runUIEventLoop eventQueue dispatcherIORef uiWalletFace historyFace putPass settings
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
  -> IORef UiSettings
  -> UiAction
runUIEventLoop eventIORef dispatcherIORef uiWalletFace historyFace putPass settings langFace =
  runInBoundThread $ withScopedPtr (getArgs >>= QApplication.new) $ \app -> do
    QApplication.setStyleSheet app $ toString styleSheet
    QApplication.setWindowIcon app =<< QIcon.newWithFile (":/images/yarn-ic.png" :: String)

    forM_ fonts $ \font -> QFontDatabase.addApplicationFont $ toString $ ":/museo/" <> font

    eventDispatcher <- QObject.new
    writeIORef dispatcherIORef $ Just eventDispatcher
    mainWindow <- initMainWindow langFace uiWalletFace historyFace settings
    void $ Event.onEvent eventDispatcher $ \(_ :: QEvent.QEvent) ->
        handleAppEvent langFace putPass eventIORef mainWindow >> return True

    QCoreApplication.exec

mkEventBQueue :: IO (UiEventBQueue)
mkEventBQueue = newTBQueueIO 100

postEventToQt :: QObject.QObject -> IO ()
postEventToQt eventDispatcher = QEvent.new QEvent.None >>= QCoreApplication.postEvent eventDispatcher

-- Create the API for interacting with the UI thread.
mkUiFace :: UiEventBQueue -> IORef (Maybe QObject.QObject) -> IORef UiSettings -> UiFace
mkUiFace eventBQueue dispatcherIORef settings =
  UiFace
    { putUiEvent = \event -> do
        -- Cardano backend can initialize and start sending events
        -- before Qt thread is started, populating dispatcherIORef.
        -- Therefore we need to cache some events for later processing.
        atomically $ writeTBQueue eventBQueue event
        whenJustM (readIORef dispatcherIORef) postEventToQt
    , uiSettings = settings
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
