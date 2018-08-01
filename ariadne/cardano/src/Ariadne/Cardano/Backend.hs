module Ariadne.Cardano.Backend (createCardanoBackend) where

import Universum

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Trans.Reader (withReaderT)
import Data.Constraint (Dict(..))
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import IiExtras
import Mockable (runProduction)
import Mockable (Production(..))
import Pos.Binary ()
import Pos.Client.CLI (NodeArgs(..))
import qualified Pos.Client.CLI as CLI
import Pos.Client.CLI.Util (readLoggerConfig)
import Pos.Core (epochOrSlotG, flattenEpochOrSlot, flattenSlotId, headerHash)
import qualified Pos.DB.BlockIndex as DB
import Pos.DB.DB (initNodeDBs)
import Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import Pos.Infra.Slotting (MonadSlots(getCurrentSlot, getCurrentSlotInaccurate))
import Pos.Launcher
import Pos.Launcher.Resource (NodeResources(..), bracketNodeResources)
import Pos.Txp (txpGlobalSettings)
import Pos.Update.Worker (updateTriggerWorker)
import Pos.Util (logException, sleep)
import Pos.Util.CompileInfo (retrieveCompileTimeInfo, withCompileInfo)
import Pos.Util.UserSecret (UserSecret, usVss, userSecret)
import System.Wlog
  (consoleActionB, maybeLogsDirB, removeAllHandlers, setupLogging, showTidB,
  showTimeB, usingLoggerName)

import Ariadne.Cardano.Face
import Ariadne.Config.Cardano (CardanoConfig(..), cardanoConfigToCommonNodeArgs)

createCardanoBackend ::
       CardanoConfig
    -> BListenerHandle
    -> (TVar UserSecret -> IO ())
    -> IO (CardanoFace, (CardanoEvent -> IO ()) -> IO ())
createCardanoBackend cardanoConfig bHandle addUs = do
  let commonNodeArgs = cardanoConfigToCommonNodeArgs cardanoConfig
  cardanoContextVar <- newEmptyMVar
  diffusionVar <- newEmptyMVar
  runProduction $
      withCompileInfo $(retrieveCompileTimeInfo) $
      withConfigurations (CLI.configurationOptions . CLI.commonArgs $ commonNodeArgs) $ \_ntpConf ->
      return (CardanoFace
          { cardanoRunCardanoMode = Nat (runCardanoMode cardanoContextVar)
          , cardanoConfigurations = Dict
          , cardanoCompileInfo = Dict
          , cardanoGetDiffusion = getDiffusion diffusionVar
          }
          , runCardanoNode bHandle addUs cardanoContextVar diffusionVar commonNodeArgs)

runCardanoMode :: MVar CardanoContext -> (CardanoMode ~> IO)
runCardanoMode cardanoContextVar (CardanoMode act) = do
  cardanoContext <- readMVar cardanoContextVar
  runProduction $ runReaderT act cardanoContext

runCardanoNode ::
       (HasConfigurations, HasCompileInfo)
    => BListenerHandle
    -> (TVar UserSecret -> IO ())
    -> MVar CardanoContext
    -> MVar (Diffusion CardanoMode)
    -> CLI.CommonNodeArgs
    -> (CardanoEvent -> IO ())
    -> IO ()
runCardanoNode bHandle addUs cardanoContextVar diffusionVar commonArgs sendCardanoEvent = do
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
      extractionWorker diffusion = do
          ask >>= putMVar cardanoContextVar
          putMVar diffusionVar diffusion
  bracket_ setupLoggers removeAllHandlers . logException "ariadne" $ do
      nodeParams <- usingLoggerName ("ariadne" <> "cardano" <> "init") $
          CLI.getNodeParams "ariadne" commonArgs nodeArgs
      let vssSK = fromJust $ npUserSecret nodeParams ^. usVss
      let sscParams = CLI.gtSscParams commonArgs vssSK (npBehaviorConfig nodeParams)
      let workers =
              [ updateTriggerWorker
              , extractionWorker
              , statusPollingWorker sendCardanoEvent
              ]
      let
        realModeToCardanoMode m = CardanoMode $ withReaderT ccRealModeContext m
        cardanoModeToRealMode (CardanoMode m) = withReaderT (CardanoContext bHandle) m
        convertMode f diff =
            cardanoModeToRealMode $ f (hoistDiffusion realModeToCardanoMode diff)
        runMode = bracketNodeResources nodeParams sscParams txpGlobalSettings initNodeDBs
            $ \nr@NodeResources{..} -> Production . runRealMode nr . convertMode $ \diff -> do
                ctx <- ask
                liftIO . addUs $ ctx ^. userSecret
                runNode nr workers diff
      runProduction runMode

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
