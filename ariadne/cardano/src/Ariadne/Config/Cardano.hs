module Ariadne.Config.Cardano
       (
         -- * Config type
         CardanoConfig (..)

       -- * Defaults
       , defaultLoggerConfig
       , defaultConfigurationYaml
       , defaultGenesisJson
       , defaultCardanoConfig

       -- * Helpers
       , cardanoFieldModifier

       -- * Construction of various stuff
       , mkLoggingParams
       , getNodeParams
       , gtSscParams

       -- * Lenses
       , ccDbPathL
       , ccRebuildDBL
       , ccKeyfilePathL
       , ccNetworkTopologyL
       , ccNetworkNodeIdL
       , ccNetworkPortL
       , ccLogConfigL
       , ccLogPrefixL
       , ccConfigurationOptionsL
       , ccEnableMetricsL
       , ccEkgParamsL


       -- * Exports for tests
       , defaultTopology
       ) where

import Universum

import Control.Lens (makeLensesWith)
import Data.Default (def)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Functor.Contravariant (Contravariant(..))
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.Time.Units (Microsecond, Second, convertUnit, fromMicroseconds)
import qualified Data.Yaml as Yaml
import qualified Dhall as D
import Dhall.Core (Expr(..))
import qualified Dhall.Core as Core
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)
import IiExtras (postfixLFields)
import Pos.Behavior (BehaviorConfig(..))
import Pos.Client.CLI.NodeOptions (CommonNodeArgs(..))
import Pos.Client.CLI.Options (CommonArgs(..))
import Pos.Client.CLI.Secrets (prepareUserSecret)
import Pos.Core.Configuration (HasConfiguration)
import Pos.Core.Slotting (Timestamp(..))
import Pos.Crypto (VssKeyPair(..))
import Pos.Infra.Network.CLI (NetworkConfigOpts(..), intNetworkConfigOpts')
import Pos.Infra.Network.Types (NodeName(..))
import qualified Pos.Infra.Network.Yaml as Network.Yaml
import Pos.Infra.Statistics (EkgParams(..))
import Pos.Infra.Util.TimeWarp (NetworkAddress)
import Pos.Launcher
  (BaseParams(..), ConfigurationOptions(..), LoggingParams(..), NodeParams(..))
import Pos.Ssc (SscParams(..))
import Pos.Update (UpdateParams(..))
import Pos.Util.UserSecret (peekUserSecret)
import System.FilePath ((</>))
import System.Wlog (LoggerConfig, usingLoggerName)

import Ariadne.Cardano.Orphans ()
import Ariadne.Config.DhallUtil

----------------------------------------------------------------------------
-- Type definition
----------------------------------------------------------------------------

-- | Configuration of the Cardano component. Similar to
-- CommonNodeArgs, but more flat and has less stuff.
data CardanoConfig = CardanoConfig
    { ccDbPath :: !(Maybe FilePath)
    , ccRebuildDB :: !Bool
    , ccKeyfilePath :: !FilePath
    , ccNetworkTopology :: !(Maybe FilePath)
    , ccNetworkNodeId :: !(Maybe NodeName)
    , ccNetworkPort :: !Word16
    , ccLogConfig :: !(Maybe FilePath)
    , ccLogPrefix :: !(Maybe FilePath)
    , ccConfigurationOptions :: !ConfigurationOptions
    , ccEnableMetrics :: !Bool
    , ccEkgParams :: !(Maybe EkgParams)
    } deriving (Eq, Show)

makeLensesWith postfixLFields ''CardanoConfig

----------------------------------------------------------------------------
-- Default values
----------------------------------------------------------------------------

parseStaticValue :: Yaml.FromJSON a => Text -> ByteString -> a
parseStaticValue name bs =
    case Yaml.decodeEither bs of
        Left err -> error $ "Static " <> name <> " is broken: " <> toText err
        Right x -> x

defaultTopology :: Network.Yaml.Topology
defaultTopology = parseStaticValue "topology" defBS
  where
    defBS =
        $(makeRelativeToProject "config/topology.yaml" >>= embedFile)

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = parseStaticValue "logging config" defBS
  where
    defBS =
      $(makeRelativeToProject "config/log-config.yaml" >>= embedFile)

defaultConfigurationYaml :: ByteString
defaultConfigurationYaml =
    $(makeRelativeToProject "config/configuration.yaml" >>= embedFile)

defaultGenesisJson :: ByteString
defaultGenesisJson =
    $(makeRelativeToProject "config/mainnet-genesis.json" >>= embedFile)

defaultCommonNodeArgs :: CommonNodeArgs
defaultCommonNodeArgs =
    CommonNodeArgs
        { dbPath = Just "db-mainnet"
        , rebuildDB = False
        , cnaAssetLockPath = Nothing
        , devGenesisSecretI = Nothing
        , keyfilePath = "secret-mainnet.key"
        , networkConfigOpts = NetworkConfigOpts
            { ncoTopology = Nothing
            , ncoKademlia = Nothing
            , ncoSelf = Just (NodeName "node0")
            , ncoPort = 3000,
              ncoPolicies = Nothing
            , ncoBindAddress = Nothing
            , ncoExternalAddress = Nothing
            }
        , jlPath = Nothing
        , commonArgs = CommonArgs
            { logConfig = Nothing
            , logPrefix = Just "logs/mainnet"
            , reportServers = []
            , updateServers = []
            , configurationOptions = ConfigurationOptions
                { cfoFilePath = "cardano-configuration.yaml"
                , cfoKey = "mainnet_full"
                , cfoSystemStart = Nothing
                , cfoSeed = Nothing
                }
            }
        , updateLatestPath = "update-installer.exe"
        , updateWithPackage = False
        , route53Params = Nothing
        , enableMetrics = False
        , ekgParams = Nothing
        , statsdParams = Nothing
        , cnaDumpGenesisDataPath = Nothing
        , cnaDumpConfiguration = False
        }

defaultCardanoConfig :: FilePath -> CardanoConfig
defaultCardanoConfig dataDir =
    CardanoConfig
        { ccDbPath = (dataDir </> ) <$> dbPath defaultCommonNodeArgs
        , ccRebuildDB = rebuildDB defaultCommonNodeArgs
        , ccKeyfilePath = dataDir </> keyfilePath defaultCommonNodeArgs
        , ccNetworkTopology = ncoTopology nco
        , ccNetworkNodeId = ncoSelf nco
        , ccNetworkPort = ncoPort nco
        , ccLogConfig = logConfig ca
        , ccLogPrefix = (dataDir </>) <$> logPrefix ca
        , ccConfigurationOptions = configurationOptions ca
        , ccEnableMetrics = enableMetrics defaultCommonNodeArgs
        , ccEkgParams = ekgParams defaultCommonNodeArgs
        }
  where
    nco :: NetworkConfigOpts
    nco = networkConfigOpts defaultCommonNodeArgs
    ca :: CommonArgs
    ca = commonArgs defaultCommonNodeArgs

----------------------------------------------------------------------------
-- Getting NodeParams and SscParams
----------------------------------------------------------------------------

mkLoggingParams :: CardanoConfig -> LoggingParams
mkLoggingParams CardanoConfig{..} =
    LoggingParams
    { lpHandlerPrefix = ccLogPrefix
    , lpConfigPath    = ccLogConfig
    , lpDefaultName   = "ariadne"
    , lpConsoleLog    = Nothing -- overridden in Backend.hs
    }

gtSscParams :: VssKeyPair -> BehaviorConfig -> SscParams
gtSscParams vssSK BehaviorConfig{..} =
    SscParams
    { spSscEnabled = True
    , spVssKeyPair = vssSK
    , spBehavior = bcSscBehavior
    }

getNodeParams :: HasConfiguration => CardanoConfig -> IO NodeParams
-- Vanilla pattern-matching is used to be sure nothing is forgotten.
getNodeParams conf@(CardanoConfig
    ccDbPath
    ccRebuildDB
    ccKeyfilePath
    ccNetworkTopology
    ccNetworkNodeId
    ccNetworkPort
    _ccLogConfig  -- is used by mkLoggingParams
    _ccLogPrefix  -- is used by mkLoggingParams
    _ccConfigurationOptions  -- should not be used
    ccEnableMetrics
    ccEkgParams
    ) = usingLoggerName ("ariadne" <> "cardano" <> "init") $ do

    let defaultCommonArgs = commonArgs defaultCommonNodeArgs

        nco :: NetworkConfigOpts
        nco = (networkConfigOpts defaultCommonNodeArgs)
            { ncoTopology = ccNetworkTopology
            , ncoSelf = ccNetworkNodeId
            , ncoPort = ccNetworkPort
            }

        baseParams :: BaseParams
        baseParams = BaseParams { bpLoggingParams = mkLoggingParams conf }

    networkConfig <- intNetworkConfigOpts' defaultTopology nco
    -- 'defaultCommonNodeArgs' is passed to 'prepareUserSecret' because we do
    -- not care what will be put into 'UserSecret'
    -- (it is essential only for core nodes)
    (primarySK, userSecret) <-
        prepareUserSecret defaultCommonNodeArgs =<< peekUserSecret ccKeyfilePath
    pure NodeParams
        { npDbPathM = ccDbPath
        , npRebuildDb = ccRebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npBaseParams = baseParams
        , npJLFile = jlPath defaultCommonNodeArgs
        , npReportServers = reportServers defaultCommonArgs
        , npUpdateParams = UpdateParams
            { upUpdatePath    = updateLatestPath defaultCommonNodeArgs
            , upUpdateWithPkg = updateWithPackage defaultCommonNodeArgs
            , upUpdateServers = updateServers defaultCommonArgs
            }
        , npRoute53Params = route53Params defaultCommonNodeArgs
        , npEnableMetrics = ccEnableMetrics
        , npEkgParams = ccEkgParams
        , npStatsdParams = statsdParams defaultCommonNodeArgs
        , npAssetLockPath = cnaAssetLockPath defaultCommonNodeArgs
        , npBehaviorConfig = def
        , npNetworkConfig = networkConfig
        }

----------------------------------------------------------------------------
-- Dhall
----------------------------------------------------------------------------

instance D.Interpret CardanoConfig where
  autoWith _ = interpretCardanoConfig

instance D.Inject CardanoConfig where
  injectWith _ = injectCardanoConfig

parseFieldCardano ::
       Map.InsOrdHashMap D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldCardano = parseField cardanoFieldModifier

cardanoFieldModifier :: D.Text -> D.Text
cardanoFieldModifier = f
  where
    f "cfoFilePath" = "configuration-file"
    f "cfoKey" = "configuration-key"
    f "cfoSystemStart" = "system-start"
    f "cfoSeed" = "configuration-seed"

    f "ekgHost" = "host"
    f "ekgPort" = "port"

    f "ccDbPath" = "db-path"
    f "ccRebuildDB" = "rebuild-db"
    f "ccKeyfilePath" = "keyfile"
    f "ccNetworkTopology" = "topology"
    f "ccNetworkNodeId" = "node-id"
    f "ccNetworkPort" = "default-port"
    f "ccLogConfig" = "log-config"
    f "ccLogPrefix" = "log-prefix"
    f "ccConfigurationOptions" = "configuration-options"
    f "ccEnableMetrics" = "metrics"
    f "ccEkgParams" = "ekg-params"
    f x = x

-- interprets
interpretNodeName :: D.Type NodeName
interpretNodeName = NodeName <$> D.strictText

interpretNetworkAddress :: D.Type NetworkAddress
interpretNetworkAddress = D.Type extractOut expectedOut
  where
    extractOut (RecordLit fields) =
      (,) <$> ( Map.lookup "IP" fields >>= D.extract interpretBytestringUTF8)
          <*> ( Map.lookup "PORT" fields >>= D.extract interpretWord16)
    extractOut _ = Nothing

    expectedOut =
        Record
            (Map.fromList
                [ ("IP", D.expected interpretBytestringUTF8)
                , ("PORT", D.expected interpretWord16)
                ]
            )

-- Assume that natural number in config represents time in seconds
interpretTimestampSec :: D.Type Timestamp
interpretTimestampSec = (Timestamp . sec . fromIntegral) <$> D.natural
  where
    sec :: Int -> Microsecond
    sec = fromMicroseconds . fromIntegral . (*) 1000000

interpretConfigurationOptions :: D.Type ConfigurationOptions
interpretConfigurationOptions = D.Type extractOut expectedOut
  where
    extractOut (RecordLit fields) = do
      cfoFilePath    <- defalultIfNothing (cfoFilePath def)
          ((parseFieldCardano fields "cfoFilePath") (D.maybe interpretFilePath))
      cfoKey         <- defalultIfNothing (cfoKey def)
          ((parseFieldCardano fields "cfoKey") (D.maybe D.strictText))
      cfoSystemStart <- parseFieldCardano fields "cfoSystemStart" (D.maybe interpretTimestampSec)
      cfoSeed        <- parseFieldCardano fields "cfoSeed" D.auto
      return ConfigurationOptions{..}
    extractOut _ = Nothing

    expectedOut = Record $ Map.fromList
        [ (cardanoFieldModifier "cfoFilePath", D.expected (D.maybe interpretFilePath))
        , (cardanoFieldModifier "cfoKey", D.expected (D.auto :: D.Type (Maybe Text)))
        , (cardanoFieldModifier "cfoSystemStart", D.expected (D.maybe interpretTimestampSec))
        , (cardanoFieldModifier "cfoSeed", D.expected (D.auto :: D.Type (Maybe Integer)))
        ]

interpretEkgParams :: D.Type EkgParams
interpretEkgParams = fmap fromNetworkAddress interpretNetworkAddress
  where
    fromNetworkAddress :: NetworkAddress -> EkgParams
    fromNetworkAddress (host, port) = EkgParams {ekgHost = host, ekgPort = fromIntegral port}

interpretCardanoConfig :: D.Type CardanoConfig
interpretCardanoConfig = D.Type extractOut expectedOut
  where
    extractOut (RecordLit fields) = do
      ccDbPath <- parseFieldCardano fields "ccDbPath" (D.maybe interpretFilePath)
      ccRebuildDB <- parseFieldCardano fields "ccRebuildDB" D.auto
      ccKeyfilePath <- defalultIfNothing "secret.key" $
          parseFieldCardano fields "ccKeyfilePath" (D.maybe interpretFilePath)
      ccNetworkTopology <-
          parseFieldCardano fields "ccNetworkTopology" (D.maybe interpretFilePath)
      ccNetworkNodeId <-
          parseFieldCardano fields "ccNetworkNodeId" (D.maybe interpretNodeName)
      ccNetworkPort <- defalultIfNothing 3000 $
          parseFieldCardano fields "ccNetworkPort" (D.maybe interpretWord16)
      ccLogConfig <- parseFieldCardano fields "ccLogConfig" (D.maybe interpretFilePath)
      ccLogPrefix <- parseFieldCardano fields "ccLogPrefix" (D.maybe interpretFilePath)
      ccConfigurationOptions <-
          parseFieldCardano fields "ccConfigurationOptions" interpretConfigurationOptions
      ccEnableMetrics <- parseFieldCardano fields "ccEnableMetrics" D.auto
      ccEkgParams <- parseFieldCardano fields "ccEkgParams" (D.maybe interpretEkgParams)
      return CardanoConfig {..}
    extractOut _ = Nothing

    expectedOut = Record $ Map.fromList
        [ (cardanoFieldModifier "ccDbPath", D.expected (D.maybe interpretFilePath))
        , (cardanoFieldModifier "ccRebuildDB", D.expected (D.auto :: D.Type Bool))
        , (cardanoFieldModifier "ccKeyfilePath", D.expected (D.maybe interpretFilePath))
        , (cardanoFieldModifier "ccNetworkTopology", D.expected (D.maybe interpretFilePath))
        , (cardanoFieldModifier "ccNetworkNodeId", D.expected (D.maybe interpretNodeName))
        , (cardanoFieldModifier "ccNetworkPort", D.expected (D.maybe interpretWord16))
        , (cardanoFieldModifier "ccLogConfig", D.expected (D.maybe interpretFilePath))
        , (cardanoFieldModifier "ccLogPrefix", D.expected (D.maybe interpretFilePath))
        , (cardanoFieldModifier "ccConfigurationOptions", D.expected interpretConfigurationOptions)
        , (cardanoFieldModifier "ccEnableMetrics", D.expected (D.auto :: D.Type Bool))
        , (cardanoFieldModifier "ccEkgParams", D.expected (D.maybe interpretEkgParams))
        ]

-- injects

injectNodeName :: D.InputType NodeName
injectNodeName = D.InputType {..}
  where
    embed (NodeName t) =
        TextLit $ fromString $ toString t
    declared = Text

injectNetworkAddress :: D.InputType NetworkAddress
injectNetworkAddress = D.InputType {..}
  where
    embed (b, w16) = RecordLit (Map.fromList -- NetworkAddress = (ByteString, Word16)
      [ ("IP", D.embed injectByteStringUTF8 b)
      , ("PORT", D.embed injectWord16 w16)
      ])
    declared = Record
      (Map.fromList
          [ ("IP", Core.Text)
          , ("PORT", Core.Natural)
          ]
      )

injectTimestampSec :: D.InputType Timestamp
injectTimestampSec = D.InputType {..}
  where
    embed (Timestamp t) = NaturalLit ((fromIntegral . toInteger . (convertUnit @Microsecond @Second)) t)
    declared = Natural

injectConfigurationOptions :: D.InputType ConfigurationOptions
injectConfigurationOptions = D.InputType {..}
  where
      embed ConfigurationOptions {..} = RecordLit
        (Map.fromList
          [ (cardanoFieldModifier "cfoFilePath", D.embed (injectMaybe injectFilePath) (Just cfoFilePath))
          , (cardanoFieldModifier "cfoKey", D.embed D.inject (Just cfoKey))
          , (cardanoFieldModifier "cfoSystemStart", D.embed (injectMaybe injectTimestampSec) cfoSystemStart)
          , (cardanoFieldModifier "cfoSeed", D.embed D.inject cfoSeed)
          ])

      declared = Record
        (Map.fromList
          [ (cardanoFieldModifier "cfoFilePath", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "cfoKey", D.declared (D.inject :: D.InputType (Maybe Text)))
          , (cardanoFieldModifier "cfoSystemStart", D.declared (injectMaybe injectTimestampSec))
          , (cardanoFieldModifier "cfoSeed", D.declared (D.inject :: D.InputType (Maybe Integer)))
          ])

injectEkgParams :: D.InputType EkgParams
injectEkgParams = contramap toNetworkAddress injectNetworkAddress
  where
      -- Int to Word16 is bad
      toNetworkAddress :: EkgParams -> NetworkAddress
      toNetworkAddress EkgParams {..} = (ekgHost, (fromIntegral ekgPort))

injectCardanoConfig :: D.InputType CardanoConfig
injectCardanoConfig = D.InputType {..}
  where
      embed CardanoConfig {..} = RecordLit
          (Map.fromList
              [ (cardanoFieldModifier "ccDbPath",
                D.embed (injectMaybe injectFilePath) ccDbPath)
              , (cardanoFieldModifier "ccRebuildDB",
                D.embed D.inject ccRebuildDB)
              , (cardanoFieldModifier "ccKeyfilePath",
                D.embed (injectMaybe injectFilePath) (Just ccKeyfilePath))
              , (cardanoFieldModifier "ccNetworkTopology",
                D.embed (injectMaybe injectFilePath) ccNetworkTopology)
              , (cardanoFieldModifier "ccNetworkNodeId",
                D.embed (injectMaybe injectNodeName) ccNetworkNodeId)
              -- `Just` is used to make ncoPort Optional in config.dhall
              , (cardanoFieldModifier "ccNetworkPort",
                D.embed (injectMaybe injectWord16) $ Just ccNetworkPort)
              , (cardanoFieldModifier "ccLogConfig",
                D.embed (injectMaybe injectFilePath) ccLogConfig)
              , (cardanoFieldModifier "ccLogPrefix",
                D.embed (injectMaybe injectFilePath) ccLogPrefix)
              , (cardanoFieldModifier "ccConfigurationOptions",
                D.embed injectConfigurationOptions ccConfigurationOptions)
              , (cardanoFieldModifier "ccEnableMetrics",
                D.embed D.inject ccEnableMetrics)
              , (cardanoFieldModifier "ccEkgParams",
                D.embed (injectMaybe injectEkgParams) ccEkgParams)
              ])

      declared = Record
        (Map.fromList
          [ (cardanoFieldModifier "ccDbPath", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "ccRebuildDB", D.declared (D.inject :: D.InputType Bool))
          , (cardanoFieldModifier "ccKeyfilePath", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "ccNetworkTopology", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "ccNetworkNodeId",  D.declared (injectMaybe injectNodeName))
          , (cardanoFieldModifier "ccNetworkPort", D.declared (injectMaybe injectWord16))
          , (cardanoFieldModifier "ccLogConfig", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "ccLogPrefix", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "ccConfigurationOptions", D.declared injectConfigurationOptions)
          , (cardanoFieldModifier "ccEnableMetrics", D.declared (D.inject :: D.InputType Bool))
          , (cardanoFieldModifier "ccEkgParams", D.declared (injectMaybe injectEkgParams))
          ])
