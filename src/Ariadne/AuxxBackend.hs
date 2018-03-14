module Ariadne.AuxxBackend (createAuxxBackend) where

import Universum hiding (atomically)

import Control.Concurrent (threadDelay)
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

data WithCommandAction = WithCommandAction
    { withCommand :: (Lang.Expr Lang.Name -> AuxxMode CommandResult) -> AuxxMode ()
    , printAction :: Text -> AuxxMode ()
    }

auxxPlugin :: (HasConfigurations, HasCompileInfo)
    => WithCommandAction
    -> (WorkerSpec AuxxMode, OutSpecs)
auxxPlugin WithCommandAction{..} = worker mempty $ \sendActions -> do
    let commandProcs = createCommandProcs (Just Dict) printAction (Just sendActions)
    forever $ withCommand $ \cmd -> case Lang.resolveCommandProcs commandProcs cmd of
        Left e -> return $ CommandProcError e
        Right expr -> either CommandEvalError CommandSuccess <$> Lang.evaluate expr

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


action :: HasCompileInfo => WithCommandAction -> AuxxOptions -> Production ()
action commandAction opts@AuxxOptions{..} = withConfigurations (CLI.configurationOptions . CLI.commonArgs $ cArgs) $ do
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
                        let (ActionSpec auxxModeAction, _) = runNode nr (singlePlugin $ auxxPlugin commandAction)
                        runLogicLayer logicLayer (runDiffusionLayer diffusionLayer (auxxModeAction (diffusion diffusionLayer)))
  where
    cArgs = aoCommonNodeArgs
    nArgs = CLI.NodeArgs { behaviorConfigPath = Nothing }

runAuxx :: WithCommandAction -> IO ()
runAuxx commandAction = withCompileInfo $(retrieveCompileTimeInfo) $ do
    -- temporary mess
    let (Success commonArgs) = execParserPure defaultPrefs (info CLI.commonNodeArgsParser briefDesc) ["--system-start", "0", "--log-config", "log-config.yaml", "--no-ntp"]
    let opts = AuxxOptions Repl commonArgs [] Automatic
    let disableConsoleLog = \lp -> lp { lpConsoleLog = Just False }
        loggingParams = disableConsoleLog $
            CLI.loggingParams "ariadne" (aoCommonNodeArgs opts)
    loggerBracket loggingParams. logException "ariadne" $ do
        runProduction (action commandAction opts)

createAuxxBackend :: IO (AuxxFace, UiFace -> IO ())
createAuxxBackend = do
  commandQueue <- newTBQueueIO 100
  let
    withCommand' uiFace cont = do
      (expr, uid) <- liftIO . atomically $ readTBQueue commandQueue
      res <- handleAsync
          (\e -> return $ CommandException e)
          (cont expr)
      liftIO $ putUiEvent uiFace $ AuxxResultEvent uid res
    putCommand expr = do
      uid <- CommandId <$> newUnique
      atomically $ writeTBQueue commandQueue (expr, uid)
      return uid

  return (AuxxFace putCommand, \uiFace -> runAuxx $ WithCommandAction (withCommand' uiFace) (const (return ())))
