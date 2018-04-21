module Ariadne.Cardano.Backend (createCardanoBackend) where

import Universum

import Ariadne.Config.Cardano (CardanoConfig(..))
import Control.Natural
import Data.Constraint (Dict(..))
import Data.Maybe (fromJust)
import Mockable (runProduction)
import Pos.Binary ()
import Pos.Client.CLI (NodeArgs(..))
import qualified Pos.Client.CLI as CLI
import Pos.Client.CLI.Util (readLoggerConfig)
import Pos.Communication.Protocol
  (OutSpecs, SendActions, WorkerSpec, toAction, worker)
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

createCardanoBackend :: CardanoConfig -> IO (CardanoFace, (CardanoEvent -> IO ()) -> IO ())
createCardanoBackend cardanoConfig = do
  let commonArgs = getCardanoConfig cardanoConfig
  cardanoContextVar <- newEmptyMVar
  sendActionsVar <- newEmptyMVar
  runProduction $
      withCompileInfo $(retrieveCompileTimeInfo) $
      withConfigurations (CLI.configurationOptions . CLI.commonArgs $ commonArgs) $
      return (CardanoFace
          { cardanoRunCardanoMode = NT (runCardanoMode cardanoContextVar)
          , cardanoConfigurations = Dict
          , cardanoCompileInfo = Dict
          , cardanoGetSendActions = getSendActions sendActionsVar
          }
          , runCardanoNode cardanoContextVar sendActionsVar commonArgs)

runCardanoMode :: MVar CardanoContext -> (CardanoMode ~> IO)
runCardanoMode cardanoContextVar act = do
  cardanoContext <- readMVar cardanoContextVar
  runProduction $ runReaderT act cardanoContext

runCardanoNode ::
       (HasConfigurations, HasCompileInfo)
    => MVar CardanoContext
    -> MVar (SendActions CardanoMode)
    -> CLI.CommonNodeArgs
    -> (CardanoEvent -> IO ())
    -> IO ()
runCardanoNode cardanoContextVar sendActionsVar commonArgs sendCardanoEvent = do
  let loggingParams = CLI.loggingParams "ariadne" commonArgs
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
        ( [toAction $ \sendActions -> do
              ask >>= putMVar cardanoContextVar
              putMVar sendActionsVar sendActions]
        , mempty )
  bracket_ setupLoggers removeAllHandlers . logException "ariadne" $ runProduction $ do
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

getSendActions ::
       MVar (SendActions CardanoMode) -> CardanoMode (SendActions CardanoMode)
getSendActions = readMVar
