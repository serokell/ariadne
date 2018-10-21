module Ariadne.UX.PasswordManager
    ( WalletId (..)
    , PutPassword
    , GetPassword
    , VoidPassword
    , RequestPasswordToUi
    , PasswordManager (..)
    , createPasswordManager
    ) where

import Control.Concurrent.Async
import Control.Monad.Component (ComponentM, buildComponent_)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Time

import qualified Control.Concurrent.Event as CE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

type PutPassword         = WalletId -> T.Text -> Maybe CE.Event -> IO ()
type GetPassword         = WalletId -> IO T.Text
type VoidPassword        = WalletId -> IO ()
type RequestPasswordToUi = WalletId -> CE.Event -> IO ()

data WalletId
    = WalletIdTemporary
    -- ^ Temporary ID: a password saved with this ID will be deleted the
    -- first time it's read
    | WalletIdSelected
    -- ^ identifies the wallet currently selected
    | WalletIdByUiIndex !Word
    -- ^ id based on the Ui index
    -- TODO: [AD-294] Remove knowledge of UI indices from Wallet backend
    | WalletIdByBackend !Text
    deriving (Eq, Ord)

data PasswordManager = PasswordManager
    { putPassword        :: PutPassword
    , getPasswordWithUI  :: RequestPasswordToUi -> GetPassword
    , voidPassword       :: VoidPassword
    }

createPasswordManager :: ComponentM PasswordManager
createPasswordManager = buildComponent_ "Password Manager" $ do
    passMapVar <- newIORef (Map.empty :: Map.Map WalletId (T.Text, Time Second))

    let passLifeTime = minute 5
        getCurrentSecs = sec . realToFrac <$> getPOSIXTime

        putPassword :: PutPassword
        putPassword walletId password mEvent = do
            currentTime <- getCurrentSecs
            oldVal <- atomicModifyIORef' passMapVar
                (lookupInsert walletId (password, currentTime))
            whenJust mEvent CE.set
            -- only start an async countDown if there was no value before
            whenNothing_ oldVal $
                void . async . countDown walletId $ toUnit @Second passLifeTime

        countDown :: WalletId -> Time Second -> IO ()
        countDown walletId waitTime = do
            threadDelay waitTime
            passMap <- readIORef passMapVar
            case Map.lookup walletId passMap of
                Just (_, oldTime) -> do
                    currentTime <- getCurrentSecs
                    case currentTime -%- (oldTime +:+ passLifeTime) of
                        (LT, t) -> countDown walletId $ toUnit @Second t
                        _ -> voidPassword walletId
                Nothing -> pass

        voidPassword :: VoidPassword
        voidPassword walletId = atomicModify_ passMapVar $ Map.delete walletId

        getPasswordWithUI :: RequestPasswordToUi -> GetPassword
        getPasswordWithUI requestToUI walletId = do
            passMap <- readIORef passMapVar
            case Map.lookup walletId passMap of
                Just (password, _) -> do
                    case walletId of
                        -- clear the password if it was temporary
                        WalletIdTemporary -> voidPassword WalletIdTemporary
                        -- otherwise update the time
                        _ -> do
                            currentTime <- getCurrentSecs
                            atomicModify_ passMapVar $
                                Map.insert walletId (password, currentTime)
                    return password
                Nothing -> do
                    -- needs to launch an ui event and wait for it to be done
                    event <- CE.new
                    requestToUI walletId event
                    CE.wait event
                    -- should be set, try to read the password again
                    getPasswordWithUI requestToUI walletId

    return PasswordManager{..}

atomicModify_ :: IORef a -> (a -> a) -> IO ()
atomicModify_ mVar func = atomicModifyIORef' mVar (\val -> (func val, ()))

lookupInsert :: Ord k => k -> a -> Map.Map k a -> (Map.Map k a, Maybe a)
lookupInsert key val = swap . Map.insertLookupWithKey (\_ a _ -> a) key val
