module Ariadne.Config.CLI
    ( getConfig
    -- * Exported for testing
    , opts
    , mergeConfigs
    ) where

import Universum

import Ariadne.Config.Ariadne (AriadneConfig(..), defaultAriadneConfig)
import Ariadne.Config.Cardano (CardanoConfig(..), cardanoFieldModifier)
import Ariadne.Config.DhallUtil (fromDhall)
import Ariadne.Config.Wallet (WalletConfig(..), walletFieldModifier)
import Control.Lens (makeLensesWith)
import Data.List (stripPrefix)
import qualified Data.Text.Lazy.IO as LTIO
import qualified Dhall as D
import Formatting (sformat, string, (%))
import IiExtras (postfixLFields)
import Named
import qualified Options.Applicative as Opt
import Pos.Client.CLI.NodeOptions (CommonNodeArgs(..))
import Pos.Client.CLI.Options (CommonArgs(..), templateParser)
import Pos.Core.Slotting (Timestamp(..))
import Pos.Launcher
import Pos.Network.CLI (NetworkConfigOpts(..))
import Pos.Network.Types (NodeName(..))
import Pos.Statistics (EkgParams(..), StatsdParams(..))
import Pos.Util.TimeWarp (NetworkAddress, addrParser, addrParserNoWildcard)
import Serokell.Data.Memory.Units (Byte, fromBytes)
import Serokell.Util (sec)
import Serokell.Util.OptParse (fromParsec)
import Serokell.Util.Parse (byte)
import System.Directory (doesFileExist)
import qualified Text.Read as R (readEither)

newtype CLI_CardanoConfig = CLI_CardanoConfig
  {cli_getCardanoConfig :: CLI_CommonNodeArgs} deriving (Eq, Show, Generic)

data CLI_AriadneConfig = CLI_AriadneConfig
  { cli_acCardano :: CLI_CardanoConfig
  , cli_acWallet :: CLI_WalletConfig } deriving (Eq, Show, Generic)

data CLI_WalletConfig = CLI_WalletConfig
  { cli_wcEntropySize :: Maybe Byte
  } deriving (Eq, Show)

data CLI_NetworkConfigOpts = CLI_NetworkConfigOpts
    { cli_ncoTopology        :: !(Maybe FilePath)
    , cli_ncoKademlia        :: !(Maybe FilePath)
    , cli_ncoSelf            :: !(Maybe NodeName)
    , cli_ncoPort            :: !(Maybe Word16)
    , cli_ncoPolicies        :: !(Maybe FilePath)
    , cli_ncoBindAddress     :: !(Maybe NetworkAddress)
    , cli_ncoExternalAddress :: !(Maybe NetworkAddress)
    } deriving (Eq, Show, Generic)

data CLI_CommonArgs = CLI_CommonArgs
    { cli_logConfig            :: !(Maybe FilePath)
    , cli_logPrefix            :: !(Maybe FilePath)
    , cli_reportServers        :: !(Maybe [Text])
    , cli_updateServers        :: !(Maybe [Text])
    , cli_configurationOptions :: !CLI_ConfigurationOptions
    } deriving (Eq, Show, Generic)

data CLI_ConfigurationOptions = CLI_ConfigurationOptions
    { cli_cfoFilePath    :: !(Maybe FilePath)
    , cli_cfoKey         :: !(Maybe Text)
    , cli_cfoSystemStart :: !(Maybe Timestamp)
    , cli_cfoSeed        :: !(Maybe Integer)
    } deriving (Eq, Show, Generic)

-- All leaves have type Maybe a to provide an ability to override any field
-- except NetworkAddress and EkgParams due to their parsers
data CLI_CommonNodeArgs = CLI_CommonNodeArgs
    { cli_dbPath                 :: !(Maybe FilePath)
    , cli_rebuildDB              :: !(Maybe Bool)
    , cli_devGenesisSecretI      :: !(Maybe Int)
    , cli_keyfilePath            :: !(Maybe FilePath)
    , cli_networkConfigOpts      :: !CLI_NetworkConfigOpts
    , cli_jlPath                 :: !(Maybe FilePath)
    , cli_commonArgs             :: !CLI_CommonArgs
    , cli_updateLatestPath       :: !(Maybe FilePath)
    , cli_updateWithPackage      :: !(Maybe Bool)
    , cli_noNTP                  :: !(Maybe Bool)
    , cli_route53Params          :: !(Maybe NetworkAddress)
    , cli_enableMetrics          :: !(Maybe Bool)
    , cli_ekgParams              :: !(Maybe EkgParams)
    , cli_statsdParams           :: !CLI_StatsdParams
    , cli_cnaDumpGenesisDataPath :: !(Maybe FilePath)
    , cli_cnaDumpConfiguration   :: !(Maybe Bool)
    } deriving (Eq, Show, Generic)

data CLI_StatsdParams = CLI_StatsdParams
    { cli_statsdHost     :: !(Maybe Text)
    , cli_statsdPort     :: !(Maybe Int)
    , cli_statsdInterval :: !(Maybe Int)
    , cli_statsdDebug    :: !(Maybe Bool)
    , cli_statsdPrefix   :: !(Maybe Text)
    , cli_statsdSuffix   :: !(Maybe Text)
    } deriving (Eq, Show, Generic)


makeLensesWith postfixLFields ''CLI_CommonArgs
makeLensesWith postfixLFields ''CLI_NetworkConfigOpts
makeLensesWith postfixLFields ''CLI_StatsdParams
makeLensesWith postfixLFields ''CLI_ConfigurationOptions
makeLensesWith postfixLFields ''CLI_AriadneConfig
makeLensesWith postfixLFields ''CLI_CardanoConfig
makeLensesWith postfixLFields ''CLI_CommonNodeArgs
makeLensesWith postfixLFields ''CLI_WalletConfig

makeLensesWith postfixLFields ''NetworkConfigOpts
makeLensesWith postfixLFields ''CommonArgs
makeLensesWith postfixLFields ''StatsdParams
makeLensesWith postfixLFields ''ConfigurationOptions
makeLensesWith postfixLFields ''CommonNodeArgs
makeLensesWith postfixLFields ''AriadneConfig
makeLensesWith postfixLFields ''CardanoConfig
makeLensesWith postfixLFields ''WalletConfig

-- Poor man's merge config
mergeConfigs :: CLI_AriadneConfig -> AriadneConfig -> AriadneConfig
mergeConfigs overrideAc defaultAc = mergedAriadneConfig
  where
    mergedAriadneConfig = AriadneConfig mergedCardanoConfig mergedWalletConfig

    mergedWalletConfig = WalletConfig $ merge (overrideAc ^. cli_acWalletL . cli_wcEntropySizeL) (defaultAc ^. acWalletL . wcEntropySizeL)

    overrideCna = overrideAc ^. cli_acCardanoL . cli_getCardanoConfigL
    defaultCna = defaultAc ^. acCardanoL . getCardanoConfigL

    overrideNco = overrideCna ^. cli_networkConfigOptsL
    defaultNco = defaultCna ^. networkConfigOptsL

    overrideCa = overrideCna ^. cli_commonArgsL
    defaultCa = defaultCna ^. commonArgsL

    overrideCo = overrideCa ^. cli_configurationOptionsL
    defaultCo = defaultCa ^. configurationOptionsL

    overrideSp = overrideCna ^. cli_statsdParamsL
    mbDefaultSp = defaultCna ^. statsdParamsL

    mergedCardanoConfig = CardanoConfig CommonNodeArgs
        { dbPath = (overrideCna ^. cli_dbPathL) <|> (defaultCna ^. dbPathL)
        , rebuildDB = merge (overrideCna ^. cli_rebuildDBL) (defaultCna ^. rebuildDBL)
        , devGenesisSecretI = (overrideCna ^. cli_devGenesisSecretIL) <|> (defaultCna ^. devGenesisSecretIL)
        , keyfilePath = merge (overrideCna ^. cli_keyfilePathL) (defaultCna ^. keyfilePathL)
        , networkConfigOpts = mergedNetworkConfigOpts
        , jlPath = (overrideCna ^. cli_jlPathL) <|> (defaultCna ^. jlPathL)
        , commonArgs = mergedCommonArgs
        , updateLatestPath = merge (overrideCna ^. cli_updateLatestPathL) (defaultCna ^. updateLatestPathL)
        , updateWithPackage = merge (overrideCna ^. cli_updateWithPackageL) (defaultCna ^. updateWithPackageL)
        , noNTP = merge (overrideCna ^. cli_noNTPL) (defaultCna ^. noNTPL)
        , route53Params = (overrideCna ^. cli_route53ParamsL) <|> (defaultCna ^. route53ParamsL)
        , enableMetrics = merge (overrideCna ^. cli_enableMetricsL) (defaultCna ^. enableMetricsL)
        , ekgParams = (overrideCna ^. cli_ekgParamsL) <|> (defaultCna ^. ekgParamsL)
        , statsdParams = mergedStatsdParams
        , cnaDumpGenesisDataPath = (overrideCna ^. cli_cnaDumpGenesisDataPathL) <|> (defaultCna ^. cnaDumpGenesisDataPathL)
        , cnaDumpConfiguration = merge (overrideCna ^. cli_cnaDumpConfigurationL) (defaultCna ^. cnaDumpConfigurationL)
        }

    mergedNetworkConfigOpts = NetworkConfigOpts
        { ncoTopology  = (overrideNco ^. cli_ncoTopologyL) <|> (defaultNco ^. ncoTopologyL)
        , ncoKademlia = (overrideNco ^. cli_ncoKademliaL) <|> (defaultNco ^. ncoKademliaL)
        , ncoSelf = (overrideNco ^. cli_ncoSelfL) <|> (defaultNco ^. ncoSelfL)
        , ncoPort = merge (overrideNco ^. cli_ncoPortL) (defaultNco ^. ncoPortL)
        , ncoPolicies = (overrideNco ^. cli_ncoPoliciesL) <|> (defaultNco ^. ncoPoliciesL)
        , ncoBindAddress = (overrideNco ^. cli_ncoBindAddressL) <|> (defaultNco ^. ncoBindAddressL)
        , ncoExternalAddress = (overrideNco ^. cli_ncoExternalAddressL) <|> (defaultNco ^. ncoExternalAddressL)
        }

    mergedCommonArgs = CommonArgs
        { logConfig = (overrideCa ^. cli_logConfigL) <|> (defaultCa ^. logConfigL)
        , logPrefix = (overrideCa ^. cli_logPrefixL) <|> (defaultCa ^. logPrefixL)
        , reportServers = merge (overrideCa ^. cli_reportServersL) (defaultCa ^. reportServersL)
        , updateServers = merge (overrideCa ^. cli_updateServersL) (defaultCa ^. updateServersL)
        , configurationOptions = mergedConfigurationOptions
        }

    mergedConfigurationOptions = ConfigurationOptions
        { cfoFilePath = merge (overrideCo ^. cli_cfoFilePathL) (defaultCo ^. cfoFilePathL)
        , cfoKey = merge (overrideCo ^. cli_cfoKeyL) (defaultCo ^. cfoKeyL)
        , cfoSystemStart = (overrideCo ^. cli_cfoSystemStartL) <|> (defaultCo ^. cfoSystemStartL)
        , cfoSeed = (overrideCo ^. cli_cfoSeedL) <|> (defaultCo ^. cfoSeedL)
        }

    mergedStatsdParams = fmap (\defaultSp -> StatsdParams
            { statsdHost = merge (overrideSp ^. cli_statsdHostL) (defaultSp ^. statsdHostL)
            , statsdPort = merge (overrideSp ^. cli_statsdPortL) (defaultSp ^. statsdPortL)
            , statsdInterval = merge (overrideSp ^. cli_statsdIntervalL) (defaultSp ^. statsdIntervalL)
            , statsdDebug = merge (overrideSp ^. cli_statsdDebugL) (defaultSp ^. statsdDebugL)
            , statsdPrefix = merge (overrideSp ^. cli_statsdPrefixL) (defaultSp ^. statsdPrefixL)
            , statsdSuffix = merge (overrideSp ^. cli_statsdSuffixL) (defaultSp ^. statsdSuffixL)
            }) mbDefaultSp

merge :: Maybe a -> a -> a
merge = flip fromMaybe

getConfig :: IO AriadneConfig
getConfig = do
  (configPath, cli_config) <- Opt.execParser opts
  config <- ifM (doesFileExist configPath)
    (do
      -- Dhall will throw well formatted colourful error message
      -- if something goes wrong
      configDhall <- LTIO.readFile configPath
      fromDhall configDhall)
    (do
      putText $ sformat ("File "%string%" not found. Default config will be used.") configPath
      return defaultAriadneConfig)
  return $ mergeConfigs cli_config config

opts :: Opt.ParserInfo (FilePath, CLI_AriadneConfig)
opts = Opt.info (parseOptions <**> Opt.helper)
  ( Opt.fullDesc
  <> Opt.progDesc "Runs Ariadne CLI"
  <> Opt.header "Ariadne CLI" )

parseOptions :: Opt.Parser (FilePath, CLI_AriadneConfig)
parseOptions = do
  configPath <- Opt.strOption
    ( Opt.long "config"
   <> Opt.metavar "FILEPATH"
   <> Opt.value "config/ariadne-config.dhall"
   <> Opt.help "Path to ariadne .dhall configuration file" )
  cli_ariadneConfig <- cliAriadneConfigParser
  return (configPath, cli_ariadneConfig)

cliAriadneConfigParser :: Opt.Parser CLI_AriadneConfig
cliAriadneConfigParser = do
  cli_acCardano <- CLI_CardanoConfig <$> cliCommonNodeArgsParser
  cli_acWallet <- cliWalletParser
  pure CLI_AriadneConfig {..}

cliWalletParser :: Opt.Parser CLI_WalletConfig
cliWalletParser = do
  cli_wcEntropySize <- (fmap . fmap) (fromBytes . fromIntegral) $ optional $ Opt.option parseEntropy
     (  Opt.long (unnamed $ toOptionNameWallet ! #field_name "cli_wcEntropySize")
     <> Opt.metavar "BYTE"
     <> Opt.help "Entropy size in bytes, valid values are: [16, 20, 24, 28, 32]")

  pure CLI_WalletConfig {..}
  where
  parseEntropy = fromParsec byte >>= \b -> if b `elem` [16, 20, 24, 28, 32]
    then return b
    else err b
  err inp = Opt.readerError $ "Invalid entropy size " <> (show inp) <> ". Chose one of [16, 20, 24, 28, 32]"

cliCommonNodeArgsParser :: Opt.Parser CLI_CommonNodeArgs
cliCommonNodeArgsParser = do
  cli_dbPath <- stringParserCardano
    ! #field_name "cli_dbPath"
    ! #meta "FILEPATH"
    ! #help "Path to directory with all DBs used by the node. \
    \If specified path doesn’t exist, a directory will be created."
  cli_rebuildDB <- autoParserCardano
    ! #field_name "cli_rebuildDB"
    ! #meta "BOOL"
    ! #help "If node's database already exists, discard its contents \
    \and create a new one from scratch."
  cli_devGenesisSecretI <- autoParserCardano
    ! #field_name "cli_devGenesisSecretI"
    ! #meta "INT"
    ! #help "Used genesis secret key index."
  cli_keyfilePath <- stringParserCardano
    ! #field_name "cli_keyfilePath"
    ! #meta "FILEPATH"
    ! #help "Path to file with secret key (we use it for Daedalus)."
  cli_networkConfigOpts <- cliNetworkConfigOption
  cli_jlPath <- stringParserCardano
    ! #field_name "cli_jlPath"
    ! #meta "FILEPATH"
    ! #help "Path to JSON log file."
  cli_commonArgs <- cliCommonArgsParser
  cli_updateLatestPath <- stringParserCardano
    ! #field_name "cli_updateLatestPath"
    ! #meta "FILEPATH"
    ! #help "Path to update installer file, \
    \which should be downloaded by Update System."
  cli_updateWithPackage <- autoParserCardano
    ! #field_name "cli_updateWithPackage"
    ! #meta "BOOL"
    ! #help "Enable updating via installer."
  cli_noNTP <- autoParserCardano
    ! #field_name "cli_noNTP"
    ! #meta "BOOL"
    ! #help "Whether to use real NTP servers to synchronise time or rely on local time"

  cli_route53Params <- optional $ Opt.option (fromParsec addrParser) $
    Opt.long (unnamed (toOptionNameCardano ! #field_name "cli_route53Params")) <>
    Opt.metavar "IP:PORT" <>
    Opt.help "Host and port for the Route53 DNS health check."
  cli_enableMetrics <- autoParserCardano
    ! #field_name "cli_enableMetrics"
    ! #meta "BOOL"
    ! #help "Enable metrics (EKG, statsd)"

  cli_ekgParams <- optional cliEkgParamsOption
  cli_statsdParams <- cliStatsdParamsOption

  cli_cnaDumpGenesisDataPath <- stringParserCardano
    ! #field_name "cli_cnaDumpGenesisDataPath"
    ! #meta "FILEPATH"
    ! #help "Dump genesis data in canonical JSON format to this file."

  cli_cnaDumpConfiguration <- autoParserCardano
    ! #field_name "cli_cnaDumpConfiguration"
    ! #meta "BOOL"
    ! #help "Dump configuration and exit."

  pure CLI_CommonNodeArgs{..}

cliEkgParamsOption :: Opt.Parser EkgParams
cliEkgParamsOption = do
  addr <- cliEkgServerOption
  pure $ EkgParams
    { ekgHost = fst addr
    , ekgPort = fromIntegral (snd addr)
    }

cliEkgServerOption :: Opt.Parser NetworkAddress
cliEkgServerOption = Opt.option (fromParsec addrParser) $
  Opt.long (unnamed (toOptionNameCardano ! #field_name "cli_ekgParams")) <>
  Opt.metavar "IP:PORT" <>
  Opt.help "Host and port for the EKG server"

cliStatsdParamsOption :: Opt.Parser CLI_StatsdParams
cliStatsdParamsOption = do
  addr <- optional cliStatsdServerOption
  interval <- autoParserCardano
    ! #field_name "cli_statsdInterval"
    ! #meta "MILLISECONDS"
    ! #help "Polling interval for statsd (milliseconds)"
  debug <- autoParserCardano
    ! #field_name "cli_statsdDebug"
    ! #meta "BOOL"
    ! #help "Enable statsd debug mode"
  prefix <- stringParserCardano
    ! #field_name "cli_statsdPrefix"
    ! #meta "TEXT"
    ! #help "Prefix for statsd"
  suffix <- stringParserCardano
    ! #field_name "cli_statsdSuffix"
    ! #meta "TEXT"
    ! #help "Suffix for statsd"
  pure CLI_StatsdParams
    { -- The network address parser only accepts ByteStrings which are
      -- UTF8 encoded
      cli_statsdHost = decodeUtf8 . fst <$> addr
    , cli_statsdPort = fromIntegral . snd <$> addr
    , cli_statsdInterval = interval
    , cli_statsdDebug = debug
    , cli_statsdPrefix = prefix
    , cli_statsdSuffix = suffix
    }

cliStatsdServerOption :: Opt.Parser NetworkAddress
cliStatsdServerOption = Opt.option (fromParsec addrParserNoWildcard) $
  -- cli_statsdAddr is not a fieldName, but I don't want to break cli_fieldName rule
  Opt.long (unnamed (toOptionNameCardano ! #field_name "cli_statsdAddr")) <>
  Opt.metavar "IP:PORT" <>
  Opt.help "Host and port for the statsd server"

cliNetworkConfigOption :: Opt.Parser CLI_NetworkConfigOpts
cliNetworkConfigOption = do
  cli_ncoTopology <- stringParserCardano
    ! #field_name "cli_ncoTopology"
    ! #meta "FILEPATH"
    ! #help "Path to a YAML file containing the network topology"

  cli_ncoKademlia <- stringParserCardano
    ! #field_name "cli_ncoKademlia"
    ! #meta "FILEPATH"
    ! #help "Path to a YAML file containing the kademlia configuration"

  cli_ncoSelf <- stringParserCardano
    ! #field_name "cli_ncoSelf"
    ! #meta "NODE_ID"
    ! #help "Identifier for this node within the network"

  cli_ncoPort <- autoParserCardano
    ! #field_name "cli_ncoPort"
    ! #meta "PORT"
    ! #help "Port number for IP address to node ID translation"

  cli_ncoPolicies <- stringParserCardano
    ! #field_name "cli_ncoPolicies"
    ! #meta "FILEPATH"
    ! #help "Path to a YAML file containing the network policies"

  cli_ncoExternalAddress <- cliExternalNetworkAddressOption
  cli_ncoBindAddress <- cliListenNetworkAddressOption
  pure CLI_NetworkConfigOpts {..}

cliExternalNetworkAddressOption :: Opt.Parser (Maybe NetworkAddress)
cliExternalNetworkAddressOption = optional $ Opt.option (fromParsec addrParserNoWildcard) $ templateParser
  (unnamed (toOptionNameCardano ! #field_name "cli_ncoExternalAddress"))
  "IP:PORT"
  "IP and port of external address. \
  \Please make sure these IP and port (on which node is running) are accessible \
  \otherwise proper work of CSL isn't guaranteed. \
  \0.0.0.0 is not accepted as a valid host."

cliListenNetworkAddressOption :: Opt.Parser (Maybe NetworkAddress)
cliListenNetworkAddressOption = optional $ Opt.option (fromParsec addrParserNoWildcard) $ templateParser
  (unnamed (toOptionNameCardano ! #field_name "cli_ncoBindAddress"))
  "IP:PORT"
  "IP and port on which to bind and listen. Please make sure these IP \
  \and port are accessible, otherwise proper work of CSL isn't guaranteed."

cliConfigurationOptionsParser :: Opt.Parser CLI_ConfigurationOptions
cliConfigurationOptionsParser = do
  cli_cfoFilePath  <- stringParserCardano
    ! #field_name "cli_cfoFilePath"
    ! #meta "FILEPATH"
    ! #help "Path to a yaml configuration file"
  cli_cfoKey <- stringParserCardano
    ! #field_name "cli_cfoKey"
    ! #meta "TEXT"
    ! #help "Key within the configuration file to use"
  cli_cfoSystemStart <- (fmap . fmap) (Timestamp . sec) $ autoParserCardano
    ! #field_name "cli_cfoSystemStart"
    ! #meta "TIMESTAMP"
    ! #help "System start time. Format - seconds since Unix Epoch."
  cli_cfoSeed        <- autoParserCardano
    ! #field_name "cli_cfoSeed"
    ! #meta "INTEGER"
    ! #help "Seed for genesis generation. Overrides one from configuration file."
  return CLI_ConfigurationOptions{..}

cliCommonArgsParser :: Opt.Parser CLI_CommonArgs
cliCommonArgsParser = do
  cli_logConfig <- stringParserCardano
    ! #field_name "cli_logConfig"
    ! #meta "FILEPATH"
    ! #help "Path to logger configuration."
  cli_logPrefix <- stringParserCardano
    ! #field_name "cli_logPrefix"
    ! #meta "FILEPATH"
    ! #help "Prefix to logger output path."
  cli_reportServers <- optional $ Opt.option listParser $
    Opt.long (unnamed (toOptionNameCardano ! #field_name "cli_reportServers")) <>
    Opt.metavar "URI" <>
    Opt.help "Reporting server to send crash/error logs on. Expected formatting: '[\"serv-uri-1\", \"serv-uri-2\"]'"

  cli_updateServers <- optional $ Opt.option listParser $
    Opt.long (unnamed (toOptionNameCardano ! #field_name "cli_updateServers")) <>
    Opt.metavar "URI" <>
    Opt.help "Server to download updates from. Expected formatting: '[\"serv-uri-1\", \"serv-uri-2\"]'"

  cli_configurationOptions <- cliConfigurationOptionsParser
  pure CLI_CommonArgs {..}


autoParser :: (Read a) =>
      (Named String "field_name" -> Named String "option_name")
    -> Named String "field_name"
    -> Named String "meta"
    -> Named String "help"
    -> Opt.Parser (Maybe a)
autoParser toComponentOptionName name (Named meta) (Named help) =
  Opt.optional $ Opt.option Opt.auto $
    mconcat
      [ Opt.long (unnamed $ toComponentOptionName name)
      , Opt.metavar meta
      , Opt.help help
      ]

autoParserCardano :: (Read a) =>
       Named String "field_name"
    -> Named String "meta"
    -> Named String "help"
    -> Opt.Parser (Maybe a)
autoParserCardano = autoParser toOptionNameCardano

stringParserCardano :: IsString s =>
       Named String "field_name"
    -> Named String "meta"
    -> Named String "help"
    -> Opt.Parser (Maybe s)
stringParserCardano name (Named meta) (Named help) =
  optional $ (fromString <$> ) $ Opt.strOption $ templateParser (unnamed $ toOptionNameCardano name) meta help

toOptionName ::
       Named String "component_prefix"
    -> (D.Text -> D.Text)
    -> Named String "field_name"
    -> Named String "option_name"
toOptionName componentPrefix componentFieldModifier (Named fieldName) = named #option_name ((unnamed componentPrefix) <> toString (componentFieldModifier (fromString name_)))
  where
    name_ = fromMaybe
      (error $ fromString ("fieldName " <> fieldName <> " should contain cli_ prefix."))
      (stripPrefix "cli_" fieldName)

toOptionNameCardano :: Named String "field_name" -> Named String "option_name"
toOptionNameCardano = (toOptionName ! #component_prefix "cardano:") cardanoFieldModifier

toOptionNameWallet :: Named String "field_name" -> Named String "option_name"
toOptionNameWallet = (toOptionName ! #component_prefix "wallet:") walletFieldModifier

listParser :: Opt.ReadM [Text]
listParser =  Opt.eitherReader (R.readEither @[Text])
