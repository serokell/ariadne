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
import qualified Pos.Chain.Genesis as Genesis
import Pos.Chain.Txp (TxpConfiguration)
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
  withCompileInfo $ withConfigurations confOpts $
    \genesisConfig txpConfig ->
    let face = CardanoFace
            { cardanoRunCardanoMode = NT (runCardanoMode cardanoContextVar)
            , cardanoConfigurations = Dict
            , cardanoCompileInfo = Dict
            , cardanoGetDiffusion = getDiffusion diffusionVar
            , cardanoGenesisConfig = genesisConfig
            }
        mkAction bHandle = runCardanoNode genesisConfig txpConfig bHandle
          cardanoContextVar diffusionVar cardanoConfig
    in return $ CardanoBackend face mkAction

runCardanoMode :: MVar CardanoContext -> (CardanoMode ~> IO)
runCardanoMode cardanoContextVar (CardanoMode act) = do
  cardanoContext <- readMVar cardanoContextVar
  runReaderT act cardanoContext

runCardanoNode ::
       (HasConfigurations, HasCompileInfo)
    => Genesis.Config
    -> TxpConfiguration
    -> BListenerHandle
    -> MVar CardanoContext
    -> MVar (Diffusion CardanoMode)
    -> CardanoConfig
    -> (CardanoEvent -> IO ())
    -> IO ()
runCardanoNode genesisConfig txpConfig bHandle cardanoContextVar diffusionVar
    cardanoConfig sendCardanoEvent = do
  -- undefined
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
              [ ("Update trigger", updateTriggerWorker)
              , ("Extraction", extractionWorker)
              , ("Status polling",
                 statusPollingWorker genesisConfig sendCardanoEvent)
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
        txpSettings = txpGlobalSettings genesisConfig txpConfig
        initDBs = initNodeDBs genesisConfig
      bracketNodeResources genesisConfig nodeParams sscParams txpSettings initDBs
          $ \(setProperLogConfig -> nr@NodeResources{..}) ->
            runRealMode genesisConfig txpConfig nr .
            convertMode $ runNode genesisConfig txpConfig nr workers

statusPollingWorker ::
       (HasConfigurations)
    => Genesis.Config
    -> (CardanoEvent -> IO ())
    -> Diffusion CardanoMode
    -> CardanoMode ()
statusPollingWorker genesis sendCardanoEvent _diffusion = do
  initialTipHeader <- DB.getTipHeader
  let epochSlots = Genesis.configEpochSlots genesis
  let initialSlotIdx =
        fromIntegral . flattenEpochOrSlot epochSlots $
        initialTipHeader ^. epochOrSlotG
  forever $ do
    currentSlot <- getCurrentSlot epochSlots
    currentSlotInaccurate <- getCurrentSlotInaccurate epochSlots
    tipHeader <- DB.getTipHeader
    let
      tipSlotIdx = fromIntegral . flattenEpochOrSlot epochSlots $ tipHeader ^. epochOrSlotG
      currentSlotIdx = fromIntegral . flattenSlotId epochSlots $ currentSlotInaccurate
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
    -> (HasConfigurations => Genesis.Config -> TxpConfiguration -> m r)
    -> m r
withConfigurations cfo act = do
    liftIO $ ensureConfigurationExists (cfoFilePath cfo)
    -- We don't use asset lock feature, because we don't create
    -- blocks, we leave these concerns to core nodes owners.
    Launcher.Configuration.withConfigurations Nothing Nothing False cfo
        (\genesisConf _walletConf txpConf _ntpConf -> act genesisConf txpConf)
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
