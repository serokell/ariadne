module Ariadne.Config.Cardano
  ( defaultCardanoConfig
  , CardanoConfig (..)
  ) where

import Universum

import Ariadne.Config.DhallUtil
import Data.Default (def)
import Data.Functor.Contravariant (Contravariant(..))
import qualified Data.Map as Map
import qualified Data.Text.Lazy.Builder as Builder
import Data.Time.Units (Microsecond, Second, convertUnit)
import qualified Dhall as D
import Dhall.Core (Expr(..))
import qualified Dhall.Core as Core
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)
import Pos.Client.CLI.NodeOptions (CommonNodeArgs(..))
import Pos.Client.CLI.Options (CommonArgs(..))
import Pos.Core.Slotting (Timestamp(..))
import Pos.Launcher
import Pos.Network.CLI (NetworkConfigOpts(..))
import Pos.Network.Types (NodeName(..))
import Pos.Statistics (EkgParams(..), StatsdParams(..))
import Pos.Util.TimeWarp (NetworkAddress)
import Serokell.Util (sec)

defaultCardanoConfig :: CardanoConfig
defaultCardanoConfig = CardanoConfig
    CommonNodeArgs
        { dbPath = Just "db-mainnet"
        , rebuildDB = False
        , devGenesisSecretI = Nothing
        , keyfilePath = "secret-mainnet.key"
        , networkConfigOpts = NetworkConfigOpts
            { ncoTopology = Just "config/cardano/topology.yaml"
            , ncoKademlia = Nothing
            , ncoSelf = Just (NodeName "node0")
            , ncoPort = 3000, ncoPolicies = Nothing
            , ncoBindAddress = Nothing
            , ncoExternalAddress = Nothing
            }
        , jlPath = Nothing
        , commonArgs = CommonArgs
            { logConfig = Just "config/cardano/log-config.yaml"
            , logPrefix = Just "logs/mainnet"
            , reportServers = []
            , updateServers = []
            , configurationOptions = ConfigurationOptions
                { cfoFilePath = "config/cardano/cardano-config.yaml"
                , cfoKey = "mainnet_full"
                , cfoSystemStart = Nothing
                , cfoSeed = Nothing
                }
            }
        , updateLatestPath = "update-installer.exe"
        , updateWithPackage = False
        , noNTP = True
        , route53Params = Nothing
        , enableMetrics = False
        , ekgParams = Nothing
        , statsdParams = Nothing
        , cnaDumpGenesisDataPath = Nothing
        , cnaDumpConfiguration = False
        }

newtype CardanoConfig = CardanoConfig
  {getCardanoConfig :: CommonNodeArgs} deriving (Eq, Show)

instance D.Interpret CardanoConfig where
  autoWith _ = CardanoConfig <$> interpretCommonNodeArgs

instance D.Inject CardanoConfig where
  injectWith _ = contramap getCardanoConfig injectCommonNodeArgs

parseFieldCardano :: Map D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
parseFieldCardano = parseField cardanoFieldModifier

cardanoFieldModifier :: D.Text -> D.Text
cardanoFieldModifier = f
  where
    f "ncoTopology" = "topology"
    f "ncoKademlia" = "kademlia"
    f "ncoSelf" = "node-id"
    f "ncoPort" = "default-port"
    f "ncoPolicies" = "policies"
    f "ncoExternalAddress" = "address"
    f "ncoBindAddress" = "listen"

    f "cfoFilePath" = "configuration-file"
    f "cfoKey" = "configuration-key"
    f "cfoSystemStart" = "system-start"
    f "cfoSeed" = "configuration-seed"

    f "logConfig" = "log-config"
    f "logPrefix" = "log-prefix"
    f "reportServers" = "report-server"
    f "updateServers" = "update-server"
    f "configurationOptions" = "configuration-options"

    f "ekgHost" = "host"
    f "ekgPort" = "port"

    f "statsdAddr" = "statsd-server"
    f "statsdInterval" = "statsd-interval"
    f "statsdDebug" = "statsd-debug"
    f "statsdPrefix" = "statsd-prefix"
    f "statsdSuffix" =  "statsd-suffix"

    f "dbPath" = "db-path"
    f "rebuildDB" = "rebuild-db"
    f "devGenesisSecretI" = "genesis-secret"
    f "keyfilePath" = "keyfile"
    f "networkConfigOpts" = "network-config"
    f "jlPath" = "json-log"
    f "commonArgs" = "common-args"
    f "updateLatestPath" = "update-latest-path"
    f "updateWithPackage" = "update-with-package"
    f "noNTP" = "no-ntp"
    f "route53Params" = "route53-health-check"
    f "enableMetrics" = "metrics"
    f "ekgParams" = "ekg-params"
    f "statsdParams" = "statsd-params"
    f "cnaDumpGenesisDataPath" = "dump-genesis-data-to"
    f "cnaDumpConfiguration" = "dump-configuration"
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

interpretNetworkConfigOpts :: D.Type NetworkConfigOpts
interpretNetworkConfigOpts = D.Type extractOut expectedOut
  where
    extractOut (RecordLit fields) = do
      ncoTopology <- parseFieldCardano fields "ncoTopology" (D.maybe interpretFilePath)
      ncoKademlia <- parseFieldCardano fields "ncoKademlia" (D.maybe interpretFilePath)
      ncoSelf <- parseFieldCardano fields "ncoSelf" (D.maybe interpretNodeName)
      ncoPort <- defalultIfNothing 3000 (parseFieldCardano fields "ncoPort" (D.maybe interpretWord16))
      ncoPolicies <- parseFieldCardano fields "ncoPolicies" (D.maybe interpretFilePath)
      ncoExternalAddress <- parseFieldCardano fields "ncoExternalAddress" (D.maybe interpretNetworkAddress)
      ncoBindAddress <- parseFieldCardano fields "ncoBindAddress" (D.maybe interpretNetworkAddress)
      return NetworkConfigOpts {..}
    extractOut _ = Nothing

    expectedOut =
      Record
          (Map.fromList
              [ (cardanoFieldModifier "ncoTopology", D.expected (D.maybe interpretFilePath))
              , (cardanoFieldModifier "ncoKademlia", D.expected (D.maybe interpretFilePath))
              , (cardanoFieldModifier "ncoSelf", D.expected (D.maybe interpretNodeName))
              , (cardanoFieldModifier "ncoPort", D.expected (D.maybe interpretWord16))
              , (cardanoFieldModifier "ncoPolicies", D.expected (D.maybe interpretFilePath))
              , (cardanoFieldModifier "ncoExternalAddress", D.expected (D.maybe interpretNetworkAddress))
              , (cardanoFieldModifier "ncoBindAddress", D.expected (D.maybe interpretNetworkAddress))
              ]
          )

interpretConfigurationOptions :: D.Type ConfigurationOptions
interpretConfigurationOptions = D.Type extractOut expectedOut
  where
    extractOut (RecordLit fields) = do
      cfoFilePath    <- defalultIfNothing (cfoFilePath def) ((parseFieldCardano fields "cfoFilePath") (D.maybe interpretFilePath))
      cfoKey         <- defalultIfNothing (cfoKey def) ((parseFieldCardano fields "cfoKey") (D.maybe D.strictText))
      cfoSystemStart <- parseFieldCardano fields "cfoSystemStart" (D.maybe interpretTimestampSec)
      cfoSeed        <- parseFieldCardano fields "cfoSeed" D.auto
      return ConfigurationOptions{..}
    extractOut _ = Nothing

    expectedOut =
        Record
          (Map.fromList
              [ (cardanoFieldModifier "cfoFilePath", D.expected (D.maybe interpretFilePath))
              , (cardanoFieldModifier "cfoKey", D.expected (D.auto :: D.Type (Maybe Text)))
              , (cardanoFieldModifier "cfoSystemStart", D.expected (D.maybe interpretTimestampSec))
              , (cardanoFieldModifier "cfoSeed", D.expected (D.auto :: D.Type (Maybe Integer)))
              ]
          )

interpretCommonArgs :: D.Type CommonArgs
interpretCommonArgs = D.Type extractOut expectedOut
  where
    extractOut (RecordLit fields) = do
      logConfig <- parseFieldCardano fields "logConfig" (D.maybe interpretFilePath)
      logPrefix <- parseFieldCardano fields "logPrefix" (D.maybe interpretFilePath)
      reportServers <- parseFieldCardano fields "reportServers" D.auto
      updateServers <- parseFieldCardano fields "updateServers" D.auto
      configurationOptions <- parseFieldCardano fields "configurationOptions" interpretConfigurationOptions
      return CommonArgs {..}
    extractOut _ = Nothing

    expectedOut =
      Record
          (Map.fromList
              [ (cardanoFieldModifier "logConfig", D.expected (D.maybe interpretFilePath))
              , (cardanoFieldModifier "logPrefix", D.expected (D.maybe interpretFilePath))
              , (cardanoFieldModifier "reportServers", D.expected (D.auto :: D.Type [Text]))
              , (cardanoFieldModifier "updateServers", D.expected (D.auto :: D.Type [Text]))
              , (cardanoFieldModifier "configurationOptions", D.expected interpretConfigurationOptions)
              ]
          )

interpretEkgParams :: D.Type EkgParams
interpretEkgParams = fmap fromNetworkAddress interpretNetworkAddress
  where
    fromNetworkAddress :: NetworkAddress -> EkgParams
    fromNetworkAddress (host, port) = EkgParams {ekgHost = host, ekgPort = fromIntegral port}

interpretStatsdParams :: D.Type StatsdParams
interpretStatsdParams = D.Type extractOut expectedOut
  where
    extractOut (RecordLit fields) = do
        statsdAddr <- parseFieldCardano fields "statsdAddr" interpretNetworkAddress
        statsdInterval  <- defalultIfNothing 1000 $ parseFieldCardano fields "statsdInterval" (D.maybe interpretInt)
        statsdDebug <- defalultIfNothing False $ parseFieldCardano fields "statsdDebug" (D.auto :: D.Type (Maybe Bool))
        statsdPrefix <- defalultIfNothing "" $ parseFieldCardano fields "statsdPrefix" (D.auto :: D.Type (Maybe Text))
        statsdSuffix <- defalultIfNothing "" $ parseFieldCardano fields "statsdSuffix" (D.auto :: D.Type (Maybe Text))

        return StatsdParams
            { statsdHost = decodeUtf8 (fst statsdAddr)
            , statsdPort = fromIntegral (snd statsdAddr)
            , ..
            }
    extractOut _ = Nothing

    expectedOut =
      Record
          (Map.fromList
              [ (cardanoFieldModifier "statsdAddr", D.expected interpretNetworkAddress)
              , (cardanoFieldModifier "statsdInterval", D.expected (D.maybe interpretInt))
              , (cardanoFieldModifier "statsdDebug", D.expected (D.auto :: D.Type (Maybe Bool)))
              , (cardanoFieldModifier "statsdPrefix", D.expected (D.auto :: D.Type (Maybe Text)))
              , (cardanoFieldModifier "statsdSuffix", D.expected  (D.auto :: D.Type (Maybe Text)))
              ])

interpretCommonNodeArgs :: D.Type CommonNodeArgs
interpretCommonNodeArgs = D.Type extractOut expectedOut
  where
    extractOut (RecordLit fields) = do
      dbPath <- parseFieldCardano fields "dbPath" (D.maybe interpretFilePath)
      rebuildDB <- parseFieldCardano fields "rebuildDB" D.auto
      devGenesisSecretI <- parseFieldCardano fields "devGenesisSecretI" (D.maybe interpretInt)
      keyfilePath <- defalultIfNothing "secret.key" $ parseFieldCardano fields "keyfilePath" (D.maybe interpretFilePath)
      networkConfigOpts <- parseFieldCardano fields "networkConfigOpts" interpretNetworkConfigOpts
      jlPath <- parseFieldCardano fields "jlPath" (D.maybe interpretFilePath)
      commonArgs <- parseFieldCardano fields "commonArgs" interpretCommonArgs
      updateLatestPath <- defalultIfNothing "update-installer.exe" $ parseFieldCardano fields "updateLatestPath" (D.maybe interpretFilePath)
      updateWithPackage <- parseFieldCardano fields "updateWithPackage" D.auto
      noNTP <- parseFieldCardano fields "noNTP" D.auto
      route53Params <- parseFieldCardano fields "route53Params" (D.maybe interpretNetworkAddress)
      enableMetrics <- parseFieldCardano fields "enableMetrics" D.auto
      ekgParams <- parseFieldCardano fields "ekgParams" (D.maybe interpretEkgParams)
      statsdParams <- parseFieldCardano fields "statsdParams" (D.maybe interpretStatsdParams)
      cnaDumpGenesisDataPath <- parseFieldCardano fields "cnaDumpGenesisDataPath" (D.maybe interpretFilePath)
      cnaDumpConfiguration <- parseFieldCardano fields "cnaDumpConfiguration" D.auto
      return CommonNodeArgs {..}
    extractOut _ = Nothing

    expectedOut =
      Record
          (Map.fromList
              [ (cardanoFieldModifier "dbPath", D.expected (D.maybe interpretFilePath))
              , (cardanoFieldModifier "rebuildDB", D.expected (D.auto :: D.Type Bool))
              , (cardanoFieldModifier "devGenesisSecretI", D.expected (D.maybe interpretInt))
              , (cardanoFieldModifier "keyfilePath", D.expected (D.maybe interpretFilePath))
              , (cardanoFieldModifier "networkConfigOpts", D.expected interpretNetworkConfigOpts)
              , (cardanoFieldModifier "jlPath", D.expected (D.maybe interpretFilePath))
              , (cardanoFieldModifier "commonArgs", D.expected interpretCommonArgs)
              , (cardanoFieldModifier "updateLatestPath", D.expected (D.maybe interpretFilePath))
              , (cardanoFieldModifier "updateWithPackage", D.expected (D.auto :: D.Type Bool))
              , (cardanoFieldModifier "noNTP", D.expected (D.auto :: D.Type Bool))
              , (cardanoFieldModifier "route53Params", D.expected (D.maybe interpretNetworkAddress))
              , (cardanoFieldModifier "enableMetrics", D.expected (D.auto :: D.Type Bool))
              , (cardanoFieldModifier "ekgParams", D.expected (D.maybe interpretEkgParams))
              , (cardanoFieldModifier "statsdParams", D.expected (D.maybe interpretStatsdParams))
              , (cardanoFieldModifier "cnaDumpGenesisDataPath", D.expected (D.maybe interpretFilePath))
              , (cardanoFieldModifier "cnaDumpConfiguration", D.expected (D.auto :: D.Type Bool))
              ])

-- injects

injectNodeName :: D.InputType NodeName
injectNodeName = D.InputType {..}
  where
    embed (NodeName t) =
        TextLit $ Builder.fromText t
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

injectNetworkConfigOpts :: D.InputType NetworkConfigOpts
injectNetworkConfigOpts = D.InputType {..}
  where
      embed NetworkConfigOpts {..} = RecordLit
        (Map.fromList
          [ (cardanoFieldModifier "ncoTopology", D.embed (injectMaybe injectFilePath) ncoTopology)
          , (cardanoFieldModifier "ncoKademlia", D.embed (injectMaybe injectFilePath) ncoKademlia)
          , (cardanoFieldModifier "ncoSelf", D.embed (injectMaybe injectNodeName) ncoSelf)
          -- `Just` is used to make ncoPort Optional in config.dhall
          , (cardanoFieldModifier "ncoPort", D.embed (injectMaybe injectWord16) $ Just ncoPort)
          , (cardanoFieldModifier "ncoPolicies", D.embed (injectMaybe injectFilePath) ncoPolicies)
          -- Should be injected as NetworkAddress not tuple
          , (cardanoFieldModifier "ncoExternalAddress", D.embed (injectMaybe injectNetworkAddress) ncoExternalAddress)
          , (cardanoFieldModifier "ncoBindAddress", D.embed (injectMaybe injectNetworkAddress) ncoBindAddress)
          ])

      declared = Record
        (Map.fromList
          [ (cardanoFieldModifier "ncoTopology", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "ncoKademlia",  D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "ncoSelf",  D.declared (injectMaybe injectNodeName))
          , (cardanoFieldModifier "ncoPort", D.declared (injectMaybe injectWord16))
          , (cardanoFieldModifier "ncoPolicies",  D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "ncoExternalAddress", D.declared (injectMaybe injectNetworkAddress))
          , (cardanoFieldModifier "ncoBindAddress", D.declared (injectMaybe injectNetworkAddress))
          ])

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

injectCommonArgs :: D.InputType CommonArgs
injectCommonArgs = D.InputType {..}
  where
      embed CommonArgs {..} = RecordLit
        (Map.fromList
          [ (cardanoFieldModifier "logConfig", D.embed (injectMaybe injectFilePath) logConfig)
          , (cardanoFieldModifier "logPrefix", D.embed (injectMaybe injectFilePath) logPrefix)
          , (cardanoFieldModifier "reportServers", D.embed D.inject reportServers)
          , (cardanoFieldModifier "updateServers", D.embed D.inject updateServers)
          , (cardanoFieldModifier "configurationOptions", D.embed injectConfigurationOptions configurationOptions)
          ])

      declared = Record
        (Map.fromList
          [ (cardanoFieldModifier "logConfig", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "logPrefix", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "reportServers", D.declared (D.inject :: D.InputType [Text]))
          , (cardanoFieldModifier "updateServers", D.declared (D.inject :: D.InputType [Text]))
          , (cardanoFieldModifier "configurationOptions", D.declared injectConfigurationOptions)
          ])

injectEkgParams :: D.InputType EkgParams
injectEkgParams = contramap toNetworkAddress injectNetworkAddress
  where
      -- Int to Word16 is bad
      toNetworkAddress :: EkgParams -> NetworkAddress
      toNetworkAddress EkgParams {..} = (ekgHost, (fromIntegral ekgPort))

injectStatsdParams :: D.InputType StatsdParams
injectStatsdParams = D.InputType {..}
  where
      -- host and port are injected in a single dhall record
      -- Int -> Word16 is not good
    embed StatsdParams {..} = RecordLit
      (Map.fromList
          [ (cardanoFieldModifier "statsdAddr", D.embed injectNetworkAddress (encodeUtf8 statsdHost, (fromIntegral statsdPort)))
          , (cardanoFieldModifier "statsdInterval", D.embed (injectMaybe injectInt) (Just statsdInterval))
          , (cardanoFieldModifier "statsdDebug", D.embed D.inject (Just statsdDebug))
          , (cardanoFieldModifier "statsdPrefix", D.embed D.inject (Just statsdPrefix))
          , (cardanoFieldModifier "statsdSuffix", D.embed D.inject (Just statsdSuffix))
          ])

    declared = Record
      (Map.fromList
          [ (cardanoFieldModifier "statsdAddr", D.declared injectNetworkAddress)
          , (cardanoFieldModifier "statsdInterval", D.declared (injectMaybe injectInt))
          , (cardanoFieldModifier "statsdDebug", D.declared (D.inject :: D.InputType (Maybe Bool)))
          , (cardanoFieldModifier "statsdPrefix", D.declared (D.inject :: D.InputType (Maybe Text)))
          , (cardanoFieldModifier "statsdSuffix", D.declared (D.inject :: D.InputType (Maybe Text)))
          ])

injectCommonNodeArgs :: D.InputType CommonNodeArgs
injectCommonNodeArgs = D.InputType {..}
  where
      embed CommonNodeArgs {..} = RecordLit
            (Map.fromList
                [ (cardanoFieldModifier "dbPath", D.embed (injectMaybe injectFilePath) dbPath)
                , (cardanoFieldModifier "rebuildDB", D.embed D.inject rebuildDB)
                , (cardanoFieldModifier "devGenesisSecretI", D.embed (injectMaybe injectInt) devGenesisSecretI)
                , (cardanoFieldModifier "keyfilePath", D.embed (injectMaybe injectFilePath) (Just keyfilePath))
                , (cardanoFieldModifier "networkConfigOpts", D.embed injectNetworkConfigOpts networkConfigOpts)
                , (cardanoFieldModifier "jlPath", D.embed (injectMaybe injectFilePath) jlPath)
                , (cardanoFieldModifier "commonArgs", D.embed injectCommonArgs commonArgs)
                , (cardanoFieldModifier "updateLatestPath", D.embed (injectMaybe injectFilePath) (Just updateLatestPath))
                , (cardanoFieldModifier "updateWithPackage", D.embed D.inject updateWithPackage)
                , (cardanoFieldModifier "noNTP", D.embed D.inject noNTP)
                , (cardanoFieldModifier "route53Params", D.embed (injectMaybe injectNetworkAddress) route53Params)
                , (cardanoFieldModifier "enableMetrics", D.embed D.inject enableMetrics)
                , (cardanoFieldModifier "ekgParams", D.embed (injectMaybe injectEkgParams) ekgParams)
                , (cardanoFieldModifier "statsdParams", D.embed (injectMaybe injectStatsdParams) statsdParams)
                , (cardanoFieldModifier "cnaDumpGenesisDataPath", D.embed (injectMaybe injectFilePath) cnaDumpGenesisDataPath)
                , (cardanoFieldModifier "cnaDumpConfiguration", D.embed D.inject cnaDumpConfiguration)
                ])

      declared = Record
        (Map.fromList
          [ (cardanoFieldModifier "dbPath", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "rebuildDB", D.declared (D.inject :: D.InputType Bool))
          , (cardanoFieldModifier "devGenesisSecretI", D.declared (injectMaybe injectInt))
          , (cardanoFieldModifier "keyfilePath", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "networkConfigOpts", D.declared injectNetworkConfigOpts)
          , (cardanoFieldModifier "jlPath", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "commonArgs", D.declared injectCommonArgs)
          , (cardanoFieldModifier "updateLatestPath", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "updateWithPackage", D.declared (D.inject :: D.InputType Bool))
          , (cardanoFieldModifier "noNTP", D.declared (D.inject :: D.InputType Bool))
          , (cardanoFieldModifier "route53Params", D.declared (injectMaybe injectNetworkAddress))
          , (cardanoFieldModifier "enableMetrics", D.declared (D.inject :: D.InputType Bool))
          , (cardanoFieldModifier "ekgParams", D.declared (injectMaybe injectEkgParams))
          , (cardanoFieldModifier "statsdParams", D.declared (injectMaybe injectStatsdParams))
          , (cardanoFieldModifier "cnaDumpGenesisDataPath", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "cnaDumpConfiguration", D.declared (D.inject :: D.InputType Bool))
          ])
