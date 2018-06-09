module Ariadne.Cardano.Backend (createCardanoBackend) where

import Universum

import Ariadne.Config.Cardano (CardanoConfig(..))
import Data.Constraint (Dict(..))
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import IiExtras
import Mockable (runProduction)
import Pos.Binary ()
import Pos.Client.CLI (NodeArgs(..))
import qualified Pos.Client.CLI as CLI
import Pos.Client.CLI.Util (readLoggerConfig)
import Pos.Communication.Types.Protocol (OutSpecs)
import Pos.Communication.Util (toAction)
import Pos.Core (epochOrSlotG, flattenSlotId, flattenEpochOrSlot, headerHash)
import qualified Pos.DB.BlockIndex as DB
import Pos.Diffusion.Types (Diffusion)
import Pos.Launcher
import Pos.Slotting (MonadSlots(getCurrentSlot, getCurrentSlotInaccurate))
import Pos.Update.Worker (updateTriggerWorker)
import Pos.Util (logException, sleep)
import Pos.Util.CompileInfo (retrieveCompileTimeInfo, withCompileInfo)
import Pos.Util.UserSecret (usVss)
import Pos.Worker.Types (WorkerSpec, worker)
import System.Wlog
  (consoleActionB, maybeLogsDirB, removeAllHandlers, setupLogging, showTidB,
  showTimeB)

import Ariadne.Cardano.Face

createCardanoBackend :: CardanoConfig -> IO (CardanoFace, (CardanoEvent -> IO ()) -> IO ())
createCardanoBackend cardanoConfig = do
  let commonArgs = getCardanoConfig cardanoConfig
  cardanoContextVar <- newEmptyMVar
  diffusionVar <- newEmptyMVar
  runProduction $
      withCompileInfo $(retrieveCompileTimeInfo) $
      withConfigurations (CLI.configurationOptions . CLI.commonArgs $ commonArgs) $ \_ntpConf ->
      return (CardanoFace
          { cardanoRunCardanoMode = Nat (runCardanoMode cardanoContextVar)
          , cardanoConfigurations = Dict
          , cardanoCompileInfo = Dict
          , cardanoGetDiffusion = getDiffusion diffusionVar
          }
          , runCardanoNode cardanoContextVar diffusionVar commonArgs)

runCardanoMode :: MVar CardanoContext -> (CardanoMode ~> IO)
runCardanoMode cardanoContextVar act = do
  cardanoContext <- readMVar cardanoContextVar
  runProduction $ runReaderT act cardanoContext

runCardanoNode ::
       (HasConfigurations, HasCompileInfo)
    => MVar CardanoContext
    -> MVar (Diffusion CardanoMode)
    -> CLI.CommonNodeArgs
    -> (CardanoEvent -> IO ())
    -> IO ()
runCardanoNode cardanoContextVar diffusionVar commonArgs sendCardanoEvent = do
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
              putMVar diffusionVar sendActions]
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
statusPollingWorker sendCardanoEvent = first pure $ worker mempty $ \_ -> do
  initialTipHeader <- DB.getTipHeader
  let initialSlotIdx = fromIntegral . flattenEpochOrSlot $ initialTipHeader ^. epochOrSlotG
  forever $ do
    currentSlot <- getCurrentSlot
    currentSlotInaccurate <- getCurrentSlotInaccurate
    tipHeader <- DB.getTipHeader
    let
      tipSlotIdx = fromIntegral . flattenEpochOrSlot $ tipHeader ^. epochOrSlotG
      currentSlotIdx = fromIntegral . flattenSlotId $ currentSlotInaccurate
      syncProgress = (tipSlotIdx - initialSlotIdx) % (currentSlotIdx - initialSlotIdx)
    liftIO $ sendCardanoEvent $ CardanoStatusUpdateEvent CardanoStatusUpdate
      { tipHeaderHash = headerHash tipHeader
      , tipEpochOrSlot = tipHeader ^. epochOrSlotG
      , currentSlot = fromMaybe currentSlotInaccurate currentSlot
      , isInaccurate = isNothing currentSlot
      , syncProgress = maybe (Just syncProgress) (const Nothing) currentSlot
      }
    sleep 1.0

getDiffusion ::
       MVar (Diffusion CardanoMode) -> CardanoMode (Diffusion CardanoMode)
getDiffusion = readMVar
