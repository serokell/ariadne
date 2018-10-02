module Ariadne.Update.Backend where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (throwIO)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Version (Version(..), parseVersion, showVersion)
import Network.HTTP.Client
  (Manager, Request(..), httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Wlog (logDebug, logWarning, usingLoggerName)
import Text.ParserCombinators.ReadP (readP_to_S)

import Ariadne.Config.Update (UpdateConfig(..))
import Ariadne.Version (currentAriadneVersion)

data FailedToParseResponse = FailedToParseResponse
  deriving (Eq, Show)

instance Exception FailedToParseResponse where
  displayException _ = "Failed to parse server response"

updateCheckLoop :: UpdateConfig -> Request -> Manager -> (Version -> Text -> IO ()) -> IO ()
updateCheckLoop uc@UpdateConfig{..} req man notifyUpdate = do
  let
    getVersionParse =
        fmap (fst . head) . nonEmpty .
        sortBy (\(_, s) (_, s') -> compare s s') -- sort by the most greedily parsed
    readVersion = maybe (throwIO FailedToParseResponse) pure . getVersionParse . readP_to_S parseVersion
  (mVer :: Either SomeException Version) <- tryAny $ httpLbs req man >>= readVersion . BS.unpack . BS.init . responseBody
  case mVer of
    Left e ->
      usingLoggerName "ariadne" . logWarning . fromString $ "Failed to check for an update: " <> displayException e
    Right ver -> do
      usingLoggerName "ariadne" . logDebug . fromString $ "Fetched version from the server: " <> showVersion ver
      when (ver > currentAriadneVersion) (notifyUpdate ver ucUpdateUrl)
  threadDelay (ucCheckDelay * 1000000)
  updateCheckLoop uc req man notifyUpdate

runUpdateCheck :: UpdateConfig -> (Version -> Text -> IO ()) -> IO ()
runUpdateCheck uc@UpdateConfig{..} notifyUpdate = do
  man <- newTlsManager
  req <- parseRequest $ toString ucVersionCheckUrl
  updateCheckLoop uc req man notifyUpdate
