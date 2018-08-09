module Ariadne.Cardano.Backend (createCardanoBackend) where

import Universum

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Trans.Reader (withReaderT)
import Data.Constraint (Dict(..))
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import IiExtras
import Mockable (Production(..), runProduction)
import Pos.Binary ()
import Pos.Client.CLI.Util (readLoggerConfig)
import Pos.Core
  (ProtocolMagic, epochOrSlotG, flattenEpochOrSlot, flattenSlotId, headerHash)
import Pos.Core.Configuration (epochSlots)
import qualified Pos.DB.BlockIndex as DB
import Pos.DB.DB (initNodeDBs)
import Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import Pos.Infra.Slotting (MonadSlots(getCurrentSlot, getCurrentSlotInaccurate))
import Pos.Launcher
import Pos.Launcher.Configuration (withConfigurations)
import Pos.Launcher.Resource (NodeResources(..), bracketNodeResources)
import Pos.Txp (txpGlobalSettings)
import Pos.Update.Worker (updateTriggerWorker)
import Pos.Util (logException, sleep)
import Pos.Util.CompileInfo (retrieveCompileTimeInfo, withCompileInfo)
import Pos.Util.UserSecret (UserSecret, usVss, userSecret)
import Pos.WorkMode (RealMode)
import System.Wlog
  (consoleActionB, maybeLogsDirB, removeAllHandlers, setupLogging, showTidB,
  showTimeB)

import Ariadne.Cardano.Face
import Ariadne.Config.Cardano
  (CardanoConfig(..), getNodeParams, gtSscParams, mkLoggingParams)

createCardanoBackend ::
       CardanoConfig
    -> BListenerHandle
    -> (TVar UserSecret -> IO ())
    -> IO (CardanoFace, (CardanoEvent -> IO ()) -> IO ())
createCardanoBackend cardanoConfig bHandle addUs = do
  cardanoContextVar <- newEmptyMVar
  diffusionVar <- newEmptyMVar
  let confOpts = ccConfigurationOptions cardanoConfig
  runProduction $
      withCompileInfo $(retrieveCompileTimeInfo) $
      -- We don't use asset lock feature, because we don't create
      -- blocks, we leave these concerns to core nodes owners.
      withConfigurations Nothing confOpts $ \_ntpConf protocolMagic ->
      return (CardanoFace
          { cardanoRunCardanoMode = Nat (runCardanoMode cardanoContextVar)
          , cardanoConfigurations = Dict
          , cardanoCompileInfo = Dict
          , cardanoGetDiffusion = getDiffusion diffusionVar
          , cardanoProtocolMagic = protocolMagic
          }
          , runCardanoNode protocolMagic bHandle addUs cardanoContextVar diffusionVar cardanoConfig)

runCardanoMode :: MVar CardanoContext -> (CardanoMode ~> IO)
runCardanoMode cardanoContextVar (CardanoMode act) = do
  cardanoContext <- readMVar cardanoContextVar
  runProduction $ runReaderT act cardanoContext

runCardanoNode ::
       (HasConfigurations, HasCompileInfo)
    => ProtocolMagic
    -> BListenerHandle
    -> (TVar UserSecret -> IO ())
    -> MVar CardanoContext
    -> MVar (Diffusion CardanoMode)
    -> CardanoConfig
    -> (CardanoEvent -> IO ())
    -> IO ()
runCardanoNode protocolMagic bHandle addUs cardanoContextVar diffusionVar
    cardanoConfig sendCardanoEvent = do
  let loggingParams = mkLoggingParams cardanoConfig
      setupLoggers = setupLogging Nothing =<< getLoggerConfig loggingParams
      getLoggerConfig LoggingParams{..} = do
          let cfgBuilder = showTidB
                        <> showTimeB
                        <> maybeLogsDirB lpHandlerPrefix
                        <> consoleActionB (\_ message -> sendCardanoEvent $ CardanoLogEvent message)
          cfg <- readLoggerConfig lpConfigPath
          pure $ cfg <> cfgBuilder
      extractionWorker diffusion = do
          ask >>= putMVar cardanoContextVar
          putMVar diffusionVar diffusion
  bracket_ setupLoggers removeAllHandlers . logException "ariadne" $ do
      nodeParams <- getNodeParams cardanoConfig
      let vssSK = fromJust $ npUserSecret nodeParams ^. usVss
      let sscParams = gtSscParams vssSK (npBehaviorConfig nodeParams)
      let workers =
              [ updateTriggerWorker
              , extractionWorker
              , statusPollingWorker sendCardanoEvent
              ]
      let
        realModeToCardanoMode :: RealMode () a -> CardanoMode a
        realModeToCardanoMode m = CardanoMode $ withReaderT ccRealModeContext m

        cardanoModeToRealMode :: CardanoMode a -> RealMode () a
        cardanoModeToRealMode (CardanoMode m) = withReaderT (CardanoContext bHandle) m

        convertMode :: (Diffusion CardanoMode -> CardanoMode a) -> Diffusion (RealMode ()) -> RealMode () a
        convertMode f diff =
            cardanoModeToRealMode $ f (hoistDiffusion realModeToCardanoMode cardanoModeToRealMode diff)

        txpSettings = txpGlobalSettings protocolMagic
        initDBs = initNodeDBs protocolMagic epochSlots
        runMode = bracketNodeResources nodeParams sscParams txpSettings initDBs
            $ \nr@NodeResources{..} ->
                Production .
                runRealMode protocolMagic nr .
                convertMode $ \diff -> do
                    ctx <- ask
                    liftIO . addUs $ ctx ^. userSecret
                    runNode protocolMagic nr workers diff
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
