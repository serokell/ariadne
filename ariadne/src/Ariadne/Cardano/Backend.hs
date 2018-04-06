module Ariadne.Cardano.Backend (createCardanoBackend) where

import Universum

import Data.Maybe (fromJust)
import IiExtras
import Mockable (runProduction)
import Options.Applicative hiding (action)

import Pos.Binary ()
import Pos.Client.CLI (NodeArgs(..))
import qualified Pos.Client.CLI as CLI
import Pos.Client.CLI.Util (readLoggerConfig)
import Pos.Communication.Protocol (OutSpecs, WorkerSpec, toAction, worker)
import Pos.Core (epochOrSlotG, headerHash)
import qualified Pos.DB.BlockIndex as DB
import Pos.Launcher
import Pos.Slotting (MonadSlots(getCurrentSlot, getCurrentSlotInaccurate))
import Pos.Update (updateTriggerWorker)
import Pos.Util (logException, sleep)
import Pos.Util.CompileInfo (retrieveCompileTimeInfo, withCompileInfo)
import Pos.Util.UserSecret (usVss)

import System.Wlog
  (consoleActionB, maybeLogsDirB, removeAllHandlers, setupLogging, showTidB,
  showTimeB)

import Ariadne.Cardano.Face

createCardanoBackend :: IO (CardanoMode :~> IO, (CardanoEvent -> IO ()) -> IO ())
createCardanoBackend = do
  cardanoContextVar <- newEmptyMVar
  return
    ( Nat (runCardanoMode cardanoContextVar)
    , runCardanoNode cardanoContextVar )

runCardanoMode :: MVar CardanoContext -> (CardanoMode ~> IO)
runCardanoMode cardanoContextVar act = do
  cardanoContext <- readMVar cardanoContextVar
  runProduction $ runReaderT act cardanoContext

runCardanoNode :: MVar CardanoContext -> (CardanoEvent -> IO ()) -> IO ()
runCardanoNode cardanoContextVar sendCardanoEvent = withCompileInfo $(retrieveCompileTimeInfo) $ do
  let (Success commonArgs) =
        execParserPure defaultPrefs (info CLI.commonNodeArgsParser briefDesc)
          [ "--db-path", "db-mainnet"
          , "--log-config", "config/cardano/log-config.yaml"
          , "--no-ntp"
          , "--configuration-file", "config/cardano/cardano-config.yaml"
          , "--topology", "config/cardano/topology.yaml"
          , "--logs-prefix", "logs/mainnet"
          , "--node-id", "node0"
          , "--keyfile", "secret-mainnet.key"
          , "--configuration-key", "mainnet_full"
          ]
      loggingParams = CLI.loggingParams "ariadne" commonArgs
      setupLoggers = setupLogging Nothing =<< getLoggerConfig loggingParams
      getLoggerConfig LoggingParams{..} = do
          let cfgBuilder = showTidB
                        <> showTimeB
                        <> maybeLogsDirB lpHandlerPrefix
                        <> consoleActionB (\_ message -> sendCardanoEvent $ CardanoLogEvent message)
          cfg <- readLoggerConfig lpConfigPath
          pure $ cfg <> cfgBuilder
      nodeArgs = CLI.NodeArgs { behaviorConfigPath = Nothing }
      extractionWorker =
        ( [toAction $ \_sendActions -> ask >>= putMVar cardanoContextVar]
        , mempty )
  bracket_ setupLoggers removeAllHandlers . logException "ariadne" $ runProduction $
    withConfigurations (CLI.configurationOptions . CLI.commonArgs $ commonArgs) $ do
      nodeParams <- CLI.getNodeParams "ariadne" commonArgs nodeArgs
      let vssSK = fromJust $ npUserSecret nodeParams ^. usVss
      let sscParams = CLI.gtSscParams commonArgs vssSK (npBehaviorConfig nodeParams)
      let workers = updateTriggerWorker
                 <> extractionWorker
                 <> statusPollingWorker sendCardanoEvent
      runNodeReal nodeParams sscParams workers

statusPollingWorker
  :: (HasConfigurations)
  => (CardanoEvent -> IO ())
  -> ([WorkerSpec CardanoMode], OutSpecs)
statusPollingWorker sendCardanoEvent = first pure $ worker mempty $ \_ ->
  forever $ do
    currentSlot <- getCurrentSlot
    currentSlotInaccurate <- getCurrentSlotInaccurate
    tipHeader <- DB.getTipHeader
    liftIO $ sendCardanoEvent $ CardanoStatusUpdateEvent CardanoStatusUpdate
      { tipHeaderHash = headerHash tipHeader
      , tipEpochOrSlot = tipHeader ^. epochOrSlotG
      , currentSlot = fromMaybe currentSlotInaccurate currentSlot
      , isInaccurate = isNothing currentSlot
      }
    sleep 1.0
