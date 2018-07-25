module Ariadne.Cardano.Backend (createCardanoBackend) where

import Universum

import Ariadne.Config.Cardano (CardanoConfig(..), CommonNodeArgs(..), NodeParams(..)
  , getNodeParams, loggingParams, toCardanoNodeParams, toCardanoCommonNodeArgs)
import Data.Constraint (Dict(..))
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import IiExtras
import Mockable (runProduction)
import Pos.Client.CLI (NodeArgs(..))
import qualified Pos.Client.CLI as CLI
import Pos.Client.CLI.Util (readLoggerConfig)
import Pos.Core (headerHash)
import qualified Pos.DB.BlockIndex as DB
import Pos.Infra.Diffusion.Types (Diffusion)
import Pos.Infra.Slotting (MonadSlots(getCurrentSlot, getCurrentSlotInaccurate)
    , epochOrSlotG, flattenEpochOrSlot, flattenSlotId)
import Pos.Launcher (LoggingParams (..), runNodeReal, withConfigurations)
import Pos.Update.Worker (updateTriggerWorker)
import Pos.Util (logException, sleep)
import Pos.Util.CompileInfo (retrieveCompileTimeInfo, withCompileInfo)
import Pos.Util.UserSecret (usVss)
import System.Wlog (consoleActionB, maybeLogsDirB, removeAllHandlers
  , setupLogging, showTidB, showTimeB, usingLoggerName)

import Ariadne.Cardano.Face

createCardanoBackend :: CardanoConfig -> IO (CardanoFace, (CardanoEvent -> IO ()) -> IO ())
createCardanoBackend cardanoConfig = do
  let commonArgs' = getCardanoConfig cardanoConfig
  cardanoContextVar <- newEmptyMVar
  diffusionVar <- newEmptyMVar
  runProduction $
      withCompileInfo $(retrieveCompileTimeInfo) $
      withConfigurations (CLI.configurationOptions . CLI.commonArgs . toCardanoCommonNodeArgs $ commonArgs') $ \_ntpConf ->
      return (CardanoFace
          { cardanoRunCardanoMode = Nat (runCardanoMode cardanoContextVar)
          , cardanoConfigurations = Dict
          , cardanoCompileInfo = Dict
          , cardanoGetDiffusion = getDiffusion diffusionVar
          }
          , runCardanoNode cardanoContextVar diffusionVar commonArgs')

runCardanoMode :: MVar CardanoContext -> (CardanoMode ~> IO)
runCardanoMode cardanoContextVar act = do
  cardanoContext <- readMVar cardanoContextVar
  runProduction $ runReaderT act cardanoContext

runCardanoNode ::
       (HasConfigurations, HasCompileInfo)
    => MVar CardanoContext
    -> MVar (Diffusion CardanoMode)
    -> CommonNodeArgs
    -> (CardanoEvent -> IO ())
    -> IO ()
runCardanoNode cardanoContextVar diffusionVar commonArgs sendCardanoEvent = do
  let loggingParams' = loggingParams "ariadne" commonArgs
      setupLoggers = setupLogging Nothing =<< getLoggerConfig loggingParams'
      getLoggerConfig LoggingParams{..} = do
          let cfgBuilder = showTidB
                        <> showTimeB
                        <> maybeLogsDirB lpHandlerPrefix
                        <> consoleActionB (\_ message -> sendCardanoEvent $ CardanoLogEvent message)
          cfg <- readLoggerConfig lpConfigPath
          pure $ cfg <> cfgBuilder
      nodeArgs = CLI.NodeArgs { behaviorConfigPath = Nothing }
      extractionWorker diffusion = do
          ask >>= putMVar cardanoContextVar
          putMVar diffusionVar diffusion
  bracket_ setupLoggers removeAllHandlers . logException "ariadne" $ do
      nodeParams <- usingLoggerName ("ariadne" <> "cardano" <> "init") $
          getNodeParams "ariadne" commonArgs nodeArgs
      let vssSK = fromJust $ npUserSecret nodeParams ^. usVss
      let sscParams = CLI.gtSscParams (toCardanoCommonNodeArgs commonArgs) vssSK (npBehaviorConfig nodeParams)
      let workers =
              [ updateTriggerWorker
              , extractionWorker
              , statusPollingWorker sendCardanoEvent
              ]
      runNodeReal (toCardanoNodeParams nodeParams) sscParams workers

statusPollingWorker ::
       (HasConfigurations)
    => (CardanoEvent -> IO ())
    -> Diffusion CardanoMode
    -> CardanoMode ()
statusPollingWorker sendCardanoEvent _diffusion = do
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
