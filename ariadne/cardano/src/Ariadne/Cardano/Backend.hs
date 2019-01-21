module Ariadne.Cardano.Backend
       ( CardanoBackend (..)
       , createCardanoBackend
       ) where

import qualified Universum.Unsafe as Unsafe (fromJust)

import Control.Monad.Component (ComponentM, buildComponent_)
import Control.Monad.Trans.Reader (withReaderT)
import Control.Natural ((:~>)(..), type (~>))
import qualified Data.ByteString as BS
import Data.Constraint (Dict(..))
import Data.Ratio ((%))
import Pos.Chain.Block (headerHash)
import Pos.Context (NodeContext(..))
import Pos.Core (epochOrSlotG, flattenEpochOrSlot, flattenSlotId)
import Pos.Crypto (ProtocolMagic)
import qualified Pos.DB.BlockIndex as DB
import Pos.DB.DB (initNodeDBs)
import Pos.DB.Txp (txpGlobalSettings)
import Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import Pos.Infra.Slotting (MonadSlots(getCurrentSlot, getCurrentSlotInaccurate))
import Pos.Launcher
  (ConfigurationOptions(cfoFilePath), LoggingParams(..), NodeParams(..),
  runNode, runRealMode)
import qualified Pos.Launcher.Configuration as Launcher.Configuration
import Pos.Launcher.Resource (NodeResources(..), bracketNodeResources)
import Pos.Util (logException, sleep)
import Pos.Util.CompileInfo (withCompileInfo)
import Pos.Util.UserSecret (usVss)
import Pos.Util.Wlog
  (WithLogger, parseLoggerConfig, removeAllHandlers, setupLogging')
import Pos.Worker.Update (updateTriggerWorker)
import Pos.WorkMode (RealMode)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))

import Ariadne.Cardano.Face
import Ariadne.Config.Cardano
  (CardanoConfig(..), defaultConfigurationYaml, defaultGenesisJson,
  defaultLoggerConfig, getNodeParams, gtSscParams, mkLoggingParams)

-- | Everything Cardano-related that we create initially.
data CardanoBackend = CardanoBackend
    { cbFace :: !CardanoFace
    , cbMkAction :: !(BListenerHandle -> (CardanoEvent -> IO ()) -> IO ())
    }

createCardanoBackend :: CardanoConfig -> ComponentM CardanoBackend
createCardanoBackend cardanoConfig = buildComponent_ "Cardano" $ do
  cardanoContextVar <- newEmptyMVar
  diffusionVar <- newEmptyMVar
  let confOpts = ccConfigurationOptions cardanoConfig
  withCompileInfo $
      withConfigurations confOpts $ \protocolMagic ->
      let face = CardanoFace
              { cardanoRunCardanoMode = NT (runCardanoMode cardanoContextVar)
              , cardanoConfigurations = Dict
              , cardanoCompileInfo = Dict
              , cardanoGetDiffusion = getDiffusion diffusionVar
              , cardanoProtocolMagic = protocolMagic
              }
          mkAction bHandle = runCardanoNode protocolMagic bHandle cardanoContextVar
              diffusionVar cardanoConfig
      in return $ CardanoBackend face mkAction

runCardanoMode :: MVar CardanoContext -> (CardanoMode ~> IO)
runCardanoMode cardanoContextVar (CardanoMode act) = do
  cardanoContext <- readMVar cardanoContextVar
  runReaderT act cardanoContext

runCardanoNode ::
       (HasConfigurations, HasCompileInfo)
    => ProtocolMagic
    -> BListenerHandle
    -> MVar CardanoContext
    -> MVar (Diffusion CardanoMode)
    -> CardanoConfig
    -> (CardanoEvent -> IO ())
    -> IO ()
runCardanoNode protocolMagic bHandle cardanoContextVar diffusionVar
    cardanoConfig sendCardanoEvent = do
  let loggingParams = mkLoggingParams cardanoConfig
      getLoggerConfig LoggingParams{..} = do
          let consoleLogAction _ message =
                  sendCardanoEvent $ CardanoLogEvent message

          let cfgBuilder = undefined  -- showTidB
                        -- <> showTimeB
                        -- <> maybeLogsDirB lpHandlerPrefix
                        -- <> consoleActionB consoleLogAction

          cfg <- maybe (pure defaultLoggerConfig) parseLoggerConfig lpConfigPath
          pure $ cfg <> cfgBuilder
      extractionWorker diffusion = do
          ask >>= putMVar cardanoContextVar
          putMVar diffusionVar diffusion
  loggerConfig <- getLoggerConfig loggingParams
  let setupLoggers = setupLogging' "ariadne" loggerConfig
  bracket setupLoggers removeAllHandlers $ \loggingHandler -> logException "ariadne" $ do
      nodeParams <- getNodeParams cardanoConfig
      let vssSK = Unsafe.fromJust $ npUserSecret nodeParams ^. usVss
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

        -- It's needed because 'bracketNodeResources' uses its own
        -- logic to create a logging config, which differs from what
        -- we do above. We want 'ncLoggerConfig' to be the same config
        -- as the one passed to 'setupLogging'.
        setProperLogConfig :: NodeResources __ -> NodeResources __
        setProperLogConfig nr =
            nr {nrContext = (nrContext nr) {ncLoggerConfig = loggerConfig}}
        txpSettings = txpGlobalSettings protocolMagic
        initDBs = initNodeDBs undefined
      bracketNodeResources nodeParams sscParams txpSettings initDBs
          $ \(setProperLogConfig -> nr@NodeResources{..}) ->
            runRealMode protocolMagic nr .
            convertMode $ \diff -> runNode protocolMagic nr workers diff

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

----------------------------------------------------------------------------
-- Provide default configuration
----------------------------------------------------------------------------

data GenesisDataFileExists =
    GenesisDataFileExists !FilePath deriving (Show)

instance Exception GenesisDataFileExists where
    displayException (GenesisDataFileExists path) =
        "There already is some file in a path (" <> show path <>
        ") where we wanted to write genesis data"

-- Version of 'withConfigurations' from 'Pos.Launcher' which omits
-- stuff we are not interested in (like NTP configuration and asset
-- lock) and uses default configuration when the one from
-- 'cfoFilePath' does not exit.
withConfigurations
    :: (WithLogger m, MonadThrow m, MonadIO m)
    => ConfigurationOptions
    -> (HasConfigurations => ProtocolMagic -> m r)
    -> m r
withConfigurations cfo act = do
    liftIO $ ensureConfigurationExists (cfoFilePath cfo)
    -- We don't use asset lock feature, because we don't create
    -- blocks, we leave these concerns to core nodes owners.
    Launcher.Configuration.withConfigurations Nothing cfo (\_ntpConf -> act)
  where
    -- Quite simple, but works in cases we care about. We do not check
    -- what is in embedded 'ByteString's, just assume it is what we expect.
    -- Anyway, if you find it a bit hacky I somewhat agree.

    genesisDataPath :: FilePath -> FilePath
    genesisDataPath confPath = takeDirectory confPath </> "mainnet-genesis.json"

    ensureConfigurationExists :: FilePath -> IO ()
    ensureConfigurationExists confPath =
        unlessM (doesFileExist confPath) $ writeStaticConfiguration confPath

    writeStaticConfiguration :: FilePath -> IO ()
    writeStaticConfiguration confPath = do
        let gdp = genesisDataPath confPath
        whenM (doesFileExist gdp) $ throwM $ GenesisDataFileExists gdp
        createDirectoryIfMissing True (takeDirectory confPath)
        BS.writeFile confPath defaultConfigurationYaml
        BS.writeFile gdp defaultGenesisJson
