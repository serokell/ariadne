module Ariadne.AuxxBackend (createAuxxBackend) where

import Universum hiding (atomically)

import Control.Concurrent.STM
import Unsafe (unsafeFromJust)
import qualified Lang
import Mode (AuxxContext (..), AuxxMode ())
import Mockable (Production (..))
import Command (createCommandProcs)
import Control.Exception.Safe (handleAsync)
import System.Wlog (logInfo)
import qualified Pos.Client.CLI as CLI
import qualified System.IO.Temp as Temp
import qualified Network.Transport.TCP as TCP (TCPAddr (..))
import Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo, retrieveCompileTimeInfo)
import Pos.Txp (txpGlobalSettings)
import Pos.Launcher (HasConfigurations, NodeParams (..), bracketNodeResources, npUserSecret, elimRealMode, runNode, loggerBracket, lpConsoleLog, withConfigurations)
import Pos.DB.DB (initNodeDBs)
import Pos.Util (logException)
import Pos.Util.UserSecret (usVss)
import Pos.Worker.Types (WorkerSpec, worker)
import Pos.WorkMode (EmptyMempoolExt, RealMode)
import Pos.Communication (ActionSpec (..), OutSpecs (..))
import Pos.Logic.Full (logicLayerFull)
import Pos.Diffusion.Full (diffusionLayerFull)
import Pos.Diffusion.Types (DiffusionLayer (..))
import AuxxOptions (AuxxOptions (..), AuxxStartMode(..), AuxxAction(..))
import JsonLog (jsonLog)
import Pos.Diffusion.Transport.TCP (bracketTransportTCP)
import Pos.Configuration (networkConnectionTimeout)
import Pos.Network.Types (NetworkConfig (..), Topology (..), topologyEnqueuePolicy, topologyDequeuePolicy, topologyFailurePolicy)
import Pos.Update (lastKnownBlockVersion)
import Pos.Logic.Types (LogicLayer (..))
import Data.Constraint (Dict (..))
import Data.Unique
import Options.Applicative hiding (action)
import Formatting

import Ariadne.Face

type CommandQueue = TBQueue (Lang.Expr Lang.Name, CommandId)

auxxPlugin :: (HasConfigurations, HasCompileInfo)
    => CommandQueue
    -> UiFace
    -> (WorkerSpec AuxxMode, OutSpecs)
auxxPlugin commandQueue uiFace = worker mempty $ \sendActions -> do
    let commandProcs = createCommandProcs (Just Dict) (const (return ())) (Just sendActions)
    forever $ do
        (expr, uid) <- liftIO . atomically $ readTBQueue commandQueue
        res <- handleAsync (\e -> return $ CommandException e) $
            case Lang.resolveCommandProcs commandProcs expr of
                Left e -> return $ CommandProcError e
                Right expr -> either CommandEvalError CommandSuccess <$> Lang.evaluate expr
        liftIO $ putUiEvent uiFace $ AuxxResultEvent uid res

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


action :: HasCompileInfo => CommandQueue -> UiFace -> AuxxOptions -> Production ()
action commandQueue uiFace opts@AuxxOptions{..} = withConfigurations (CLI.configurationOptions . CLI.commonArgs $ cArgs) $ do
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
        elimRealMode nr $ toRealMode $
            logicLayerFull jsonLog $ \logicLayer ->
                bracketTransportTCP networkConnectionTimeout (ncTcpAddr (npNetworkConfig nodeParams)) $ \transport ->
                    diffusionLayerFull (npNetworkConfig nodeParams) lastKnownBlockVersion transport Nothing $ \withLogic -> do
                        diffusionLayer <- withLogic (logic logicLayer)
                        let singlePlugin (plug, outs) = ([plug], outs)
                        let (ActionSpec auxxModeAction, _) = runNode nr (singlePlugin $ auxxPlugin commandQueue uiFace)
                        runLogicLayer logicLayer (runDiffusionLayer diffusionLayer (auxxModeAction (diffusion diffusionLayer)))
  where
    cArgs = aoCommonNodeArgs
    nArgs = CLI.NodeArgs { behaviorConfigPath = Nothing }

runAuxx :: CommandQueue -> UiFace -> IO ()
runAuxx commandQueue uiFace = withCompileInfo $(retrieveCompileTimeInfo) $ do
    -- temporary mess
    let (Success commonArgs) = execParserPure defaultPrefs (info CLI.commonNodeArgsParser briefDesc) ["--system-start", "0", "--log-config", "log-config.yaml", "--no-ntp", "--configuration-file",  "auxx.yaml"]
    let opts = AuxxOptions Repl commonArgs [] Automatic
    let disableConsoleLog = \lp -> lp { lpConsoleLog = Just False }
        loggingParams = disableConsoleLog $
            CLI.loggingParams "ariadne" (aoCommonNodeArgs opts)
    loggerBracket loggingParams. logException "ariadne" $ do
        runProduction (action commandQueue uiFace opts)

createAuxxBackend :: IO (AuxxFace, UiFace -> IO ())
createAuxxBackend = do
  commandQueue <- newTBQueueIO 100
  let
    putCommand expr = do
      uid <- CommandId <$> newUnique
      atomically $ writeTBQueue commandQueue (expr, uid)
      return uid

  return (AuxxFace putCommand, runAuxx commandQueue)
