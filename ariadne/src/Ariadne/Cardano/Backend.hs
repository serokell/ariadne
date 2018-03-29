module Ariadne.Cardano.Backend (createCardanoBackend) where

import Universum hiding (atomically)

import Unsafe (unsafeFromJust)
import Ariadne.Cardano.Mode (AuxxContext (..), AuxxMode ())
import Mockable (Production (..))
import System.Wlog (logInfo)
import qualified Pos.Client.CLI as CLI
import qualified System.IO.Temp as Temp
import qualified Network.Transport.TCP as TCP (TCPAddr (..))
import Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo, retrieveCompileTimeInfo)
import Pos.Txp (txpGlobalSettings)
import Pos.Launcher (NodeParams (..), bracketNodeResources, npUserSecret, elimRealMode, runNode, loggerBracket, lpConsoleLog, withConfigurations)
import Pos.DB.DB (initNodeDBs)
import Pos.Util (logException)
import Pos.Util.UserSecret (usVss)
import Pos.WorkMode (EmptyMempoolExt, RealMode)
import Pos.Communication (ActionSpec (..), NodeId)
import Pos.Logic.Full (logicLayerFull)
import Pos.Diffusion.Full (diffusionLayerFull)
import Pos.Diffusion.Types (DiffusionLayer (..))
import JsonLog (jsonLog)
import Pos.Diffusion.Transport.TCP (bracketTransportTCP)
import Pos.Configuration (networkConnectionTimeout)
import Pos.Network.Types (NetworkConfig (..), Topology (..), topologyEnqueuePolicy, topologyDequeuePolicy, topologyFailurePolicy)
import Pos.Update (lastKnownBlockVersion)
import Pos.Logic.Types (LogicLayer (..))
import Options.Applicative hiding (action)
import Formatting

data AuxxOptions = AuxxOptions
    { aoCommonNodeArgs :: !CLI.CommonNodeArgs  -- ^ Common CLI args for nodes
    , aoPeers          :: ![NodeId]
    -- ^ Peers with which we want to communicate
    --   TODO: we also have topology, so it can be redundant.
    }

-- TODO: Replace 'IO AuxxContext' with 'AuxxMode ~> IO', name it CardanoFace and
-- move to Cardano/Face.hs
createCardanoBackend :: IO (IO AuxxContext, IO ())
createCardanoBackend = do
  auxxContextVar <- newEmptyMVar
  return (readMVar auxxContextVar, runCardano auxxContextVar)

runCardano :: MVar AuxxContext -> IO ()
runCardano auxxContextVar = withCompileInfo $(retrieveCompileTimeInfo) $ do
    -- temporary mess
    let (Success commonArgs) = execParserPure defaultPrefs (info CLI.commonNodeArgsParser briefDesc) ["--system-start", "0", "--log-config", "log-config.yaml", "--no-ntp", "--configuration-file",  "auxx.yaml"]
    let opts = AuxxOptions commonArgs []
    let disableConsoleLog = \lp -> lp { lpConsoleLog = Just False }
        loggingParams = disableConsoleLog $
            CLI.loggingParams "ariadne" (aoCommonNodeArgs opts)
    loggerBracket loggingParams. logException "ariadne" $ do
        runProduction (action auxxContextVar opts)

correctNodeParams :: AuxxOptions -> NodeParams -> Production (NodeParams, Bool)
correctNodeParams AuxxOptions {..} np = do
    (dbPath, isTempDbUsed) <- case npDbPathM np of
        Nothing -> do
            tempDir <- liftIO $ Temp.getCanonicalTemporaryDirectory
            dbPath <- liftIO $ Temp.createTempDirectory tempDir "nodedb"
            logInfo $ sformat ("Temporary db created: "%shown) dbPath
            return (dbPath, True)
        Just dbPath -> do
            logInfo $ sformat ("Supplied db used: "%shown) dbPath
            return (dbPath, False)
    let np' = np
            { npNetworkConfig = networkConfig
            , npRebuildDb = npRebuildDb np || isTempDbUsed
            , npDbPathM = Just dbPath }
    return (np', isTempDbUsed)
  where
    topology = TopologyAuxx aoPeers
    networkConfig =
        NetworkConfig
        { ncDefaultPort = 3000
        , ncSelfName = Nothing
        , ncEnqueuePolicy = topologyEnqueuePolicy topology
        , ncDequeuePolicy = topologyDequeuePolicy topology
        , ncFailurePolicy = topologyFailurePolicy topology
        , ncTopology = topology
        , ncTcpAddr = TCP.Unaddressable
        }

action :: HasCompileInfo => MVar AuxxContext -> AuxxOptions -> Production ()
action auxxContextVar opts@AuxxOptions{..} = withConfigurations (CLI.configurationOptions . CLI.commonArgs $ cArgs) $ do
    (nodeParams, tempDbUsed) <- correctNodeParams opts =<< CLI.getNodeParams "ariadne" cArgs nArgs
    let toRealMode :: AuxxMode a -> RealMode EmptyMempoolExt a
        toRealMode auxxAction = do
            realModeContext <- ask
            let auxxContext =
                    AuxxContext
                    { acRealModeContext = realModeContext
                    , acTempDbUsed = tempDbUsed }
            lift $ runReaderT auxxAction auxxContext
    let vssSK = unsafeFromJust $ npUserSecret nodeParams ^. usVss
    let sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)
    bracketNodeResources nodeParams sscParams txpGlobalSettings initNodeDBs $ \nr ->
        elimRealMode nr $ toRealMode $ do
            auxxContext <- ask
            putMVar auxxContextVar auxxContext
            logicLayerFull jsonLog $ \logicLayer ->
                bracketTransportTCP networkConnectionTimeout (ncTcpAddr (npNetworkConfig nodeParams)) $ \transport ->
                    diffusionLayerFull (npNetworkConfig nodeParams) lastKnownBlockVersion transport Nothing $ \withLogic -> do
                        diffusionLayer <- withLogic (logic logicLayer)
                        let (ActionSpec auxxModeAction, _) = runNode nr (mempty, mempty)
                        runLogicLayer logicLayer (runDiffusionLayer diffusionLayer (auxxModeAction (diffusion diffusionLayer)))
  where
    cArgs = aoCommonNodeArgs
    nArgs = CLI.NodeArgs { behaviorConfigPath = Nothing }
