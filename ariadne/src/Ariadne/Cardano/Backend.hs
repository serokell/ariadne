module Ariadne.Cardano.Backend (createCardanoBackend) where

import           Universum

import           Data.Maybe (fromJust)
import           Mockable (runProduction)
import           Options.Applicative hiding (action)

import           Pos.Communication.Protocol (toAction)
import           Pos.Binary ()
import           Pos.Client.CLI (NodeArgs (..))
import qualified Pos.Client.CLI as CLI
import           Pos.Launcher
import           Pos.Update (updateTriggerWorker)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)

import           Ariadne.Cardano.Face
import           Ariadne.Util

createCardanoBackend :: IO (CardanoMode :~> IO, IO ())
createCardanoBackend = do
  cardanoContextVar <- newEmptyMVar
  return
    ( Nat (runCardanoMode cardanoContextVar)
    , runCardanoNode cardanoContextVar )

runCardanoMode :: MVar CardanoContext -> (CardanoMode ~> IO)
runCardanoMode cardanoContextVar act = do
  cardanoContext <- readMVar cardanoContextVar
  runProduction $ runReaderT act cardanoContext

runCardanoNode :: MVar CardanoContext -> IO ()
runCardanoNode cardanoContextVar = withCompileInfo $(retrieveCompileTimeInfo) $ do
  let (Success commonArgs) =
        execParserPure defaultPrefs (info CLI.commonNodeArgsParser briefDesc)
          [ "--system-start", "0"
          , "--log-config", "log-config.yaml"
          , "--no-ntp"
          , "--configuration-file", "cardano-config.yaml"
          ]
      disableConsoleLog = \lp -> lp { lpConsoleLog = Just False }
      loggingParams = disableConsoleLog $
        CLI.loggingParams "ariadne" commonArgs
      nodeArgs = CLI.NodeArgs { behaviorConfigPath = Nothing }
      extractionWorker =
        ( [toAction $ \_sendActions -> ask >>= putMVar cardanoContextVar]
        , mempty )
  loggerBracket loggingParams. logException "ariadne" $ runProduction $
    withConfigurations (CLI.configurationOptions . CLI.commonArgs $ commonArgs) $ do
      nodeParams <- CLI.getNodeParams "ariadne" commonArgs nodeArgs
      let vssSK = fromJust $ npUserSecret nodeParams ^. usVss
      let sscParams = CLI.gtSscParams commonArgs vssSK (npBehaviorConfig nodeParams)
      runNodeReal nodeParams sscParams (updateTriggerWorker <> extractionWorker)
