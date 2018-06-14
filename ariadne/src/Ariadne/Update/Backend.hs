module Ariadne.Update.Backend where

import Universum

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (Exception(..), throwIO, tryAny)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import Data.Version (Version(..), parseVersion, showVersion)
import Network.HTTP.Client
  (Manager, Request(..), httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Paths_ariadne (version)
import System.Wlog (logDebug, logWarning, usingLoggerName)
import Text.ParserCombinators.ReadP (readP_to_S)

import Ariadne.Config.Update (UpdateConfig(..))

data FailedToParseResponse = FailedToParseResponse
  deriving (Eq, Show)

instance Exception FailedToParseResponse where
  displayException _ = "Failed to parse server response"

updateCheckLoop :: UpdateConfig -> Request -> Manager -> (Version -> IO ()) -> IO ()
updateCheckLoop uc@UpdateConfig{..} req man notifyUpdate = do
  let
    getVersionParse =
        fmap fst . fmap head . nonEmpty .
        sortBy (\(_, s) (_, s') -> compare s s') -- sort by the most greedily parsed
    readVersion = maybe (throwIO FailedToParseResponse) pure . getVersionParse . readP_to_S parseVersion
  (mVer :: Either SomeException Version) <- tryAny $ httpLbs req man >>= readVersion . BS.unpack . BS.init . responseBody
  case mVer of
    Left e ->
      usingLoggerName "ariadne" . logWarning . fromString $ "Failed to check for an update: " <> displayException e
    Right ver -> do
      usingLoggerName "ariadne" . logDebug . fromString $ "Fetched version from the server: " <> showVersion ver
      when (ver > version) (notifyUpdate ver)
  threadDelay (ucCheckDelay * 1000000)
  updateCheckLoop uc req man notifyUpdate

runUpdateCheck :: UpdateConfig -> (Version -> IO ()) -> IO ()
runUpdateCheck uc@UpdateConfig{..} notifyUpdate = do
  man <- newTlsManager
  req <- parseRequest $ T.unpack ucVersionCheckUrl ++ "/version"
  updateCheckLoop uc req man notifyUpdate
