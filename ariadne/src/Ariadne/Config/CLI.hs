module Ariadne.Config.CLI
    ( getConfig
    , opts
    , mergeConfigs
    ) where

import Universum

import Ariadne.Config.Ariadne (AriadneConfig(..), defaultAriadneConfig)
import Ariadne.Config.Cardano (CardanoConfig(..), cardanoFieldModifier)
import Ariadne.Config.DhallUtil (fromDhall)
import Ariadne.Config.Wallet (WalletConfig(..), walletFieldModifier)
import Control.Lens (makeLensesWith)
import qualified Data.ByteString.Char8 as BS8
import Data.List (stripPrefix)
import qualified Data.Text.Lazy.IO as LTIO
import qualified Dhall as D
import Formatting (sformat, string, (%))
import IiExtras (postfixLFields)
import qualified Options.Applicative as Opt
import Pos.Client.CLI.NodeOptions (CommonNodeArgs(..))
import Pos.Client.CLI.Options (CommonArgs(..), templateParser)
import Pos.Core.Slotting (Timestamp(..))
import Pos.Launcher
import Pos.Network.CLI (NetworkConfigOpts(..))
import Pos.Network.Types (NodeName(..))
import Pos.Statistics (EkgParams(..), StatsdParams(..))
import Pos.Util.TimeWarp (NetworkAddress)
import Serokell.Data.Memory.Units (Byte, fromBytes)
import Serokell.Util (sec)
import qualified Serokell.Util.Parse as P
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P

import Options.Applicative (ReadM, eitherReader)
import System.Directory (doesFileExist)
import Text.Parsec (Parsec, parse)

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

-- All leaves has type Maybe a to provide an ability to override any field
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
            { dbPath = mergeMaybe (overrideCna ^. cli_dbPathL) (defaultCna ^. dbPathL)
            , rebuildDB = merge (overrideCna ^. cli_rebuildDBL) (defaultCna ^. rebuildDBL)
            , devGenesisSecretI = mergeMaybe (overrideCna ^. cli_devGenesisSecretIL) (defaultCna ^. devGenesisSecretIL)
            , keyfilePath = merge (overrideCna ^. cli_keyfilePathL) (defaultCna ^. keyfilePathL)
            , networkConfigOpts = mergedNetworkConfigOpts
            , jlPath = mergeMaybe (overrideCna ^. cli_jlPathL) (defaultCna ^. jlPathL)
            , commonArgs = mergedCommonArgs
            , updateLatestPath = merge (overrideCna ^. cli_updateLatestPathL) (defaultCna ^. updateLatestPathL)
            , updateWithPackage = merge (overrideCna ^. cli_updateWithPackageL) (defaultCna ^. updateWithPackageL)
            , noNTP = merge (overrideCna ^. cli_noNTPL) (defaultCna ^. noNTPL)
            , route53Params = mergeMaybe (overrideCna ^. cli_route53ParamsL) (defaultCna ^. route53ParamsL)
            , enableMetrics = merge (overrideCna ^. cli_enableMetricsL) (defaultCna ^. enableMetricsL)
            , ekgParams = mergeMaybe (overrideCna ^. cli_ekgParamsL) (defaultCna ^. ekgParamsL)
            , statsdParams = mergeStatsdParams
            , cnaDumpGenesisDataPath = mergeMaybe (overrideCna ^. cli_cnaDumpGenesisDataPathL) (defaultCna ^. cnaDumpGenesisDataPathL)
            , cnaDumpConfiguration = merge (overrideCna ^. cli_cnaDumpConfigurationL) (defaultCna ^. cnaDumpConfigurationL)
            }

        mergedNetworkConfigOpts = NetworkConfigOpts
            { ncoTopology  = mergeMaybe (overrideNco ^. cli_ncoTopologyL) (defaultNco ^. ncoTopologyL)
            , ncoKademlia = mergeMaybe (overrideNco ^. cli_ncoKademliaL) (defaultNco ^. ncoKademliaL)
            , ncoSelf = mergeMaybe (overrideNco ^. cli_ncoSelfL) (defaultNco ^. ncoSelfL)
            , ncoPort = merge (overrideNco ^. cli_ncoPortL) (defaultNco ^. ncoPortL)
            , ncoPolicies = mergeMaybe (overrideNco ^. cli_ncoPoliciesL) (defaultNco ^. ncoPoliciesL)
            , ncoBindAddress = mergeMaybe (overrideNco ^. cli_ncoBindAddressL) (defaultNco ^. ncoBindAddressL)
            , ncoExternalAddress = mergeMaybe (overrideNco ^. cli_ncoExternalAddressL) (defaultNco ^. ncoExternalAddressL)
            }

        mergedCommonArgs = CommonArgs
            { logConfig = mergeMaybe (overrideCa ^. cli_logConfigL) (defaultCa ^. logConfigL)
            , logPrefix = mergeMaybe (overrideCa ^. cli_logPrefixL) (defaultCa ^. logPrefixL)
            , reportServers = merge (overrideCa ^. cli_reportServersL) (defaultCa ^. reportServersL)
            , updateServers = merge (overrideCa ^. cli_updateServersL) (defaultCa ^. updateServersL)
            , configurationOptions = mergedConfigurationOptions
            }

        mergedConfigurationOptions = ConfigurationOptions
            { cfoFilePath = merge (overrideCo ^. cli_cfoFilePathL) (defaultCo ^. cfoFilePathL)
            , cfoKey = merge (overrideCo ^. cli_cfoKeyL) (defaultCo ^. cfoKeyL)
            , cfoSystemStart = mergeMaybe (overrideCo ^. cli_cfoSystemStartL) (defaultCo ^. cfoSystemStartL)
            , cfoSeed = mergeMaybe (overrideCo ^. cli_cfoSeedL) (defaultCo ^. cfoSeedL)
            }

        mergeStatsdParams = fmap (\defaultSp -> StatsdParams
                { statsdHost = merge (overrideSp ^. cli_statsdHostL) (defaultSp ^. statsdHostL)
                , statsdPort = merge (overrideSp ^. cli_statsdPortL) (defaultSp ^. statsdPortL)
                , statsdInterval = merge (overrideSp ^. cli_statsdIntervalL) (defaultSp ^. statsdIntervalL)
                , statsdDebug = merge (overrideSp ^. cli_statsdDebugL) (defaultSp ^. statsdDebugL)
                , statsdPrefix = merge (overrideSp ^. cli_statsdPrefixL) (defaultSp ^. statsdPrefixL)
                , statsdSuffix = merge (overrideSp ^. cli_statsdSuffixL) (defaultSp ^. statsdSuffixL)
                }) mbDefaultSp

mergeMaybe :: Maybe a -> Maybe a -> Maybe a
mergeMaybe (Just override) _ = Just override
mergeMaybe Nothing def = def

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
    cli_wcEntropySize <- (fmap . fmap) fromBytes $ autoParserWallet
        "cli_wcEntropySize"
        "BYTE"
        "Entropy size in bytes, valid values are: [16, 20, 24, 28, 32]"
    pure CLI_WalletConfig {..}

cliCommonNodeArgsParser :: Opt.Parser CLI_CommonNodeArgs
cliCommonNodeArgsParser = do
    cli_dbPath <- stringParserCardano
        "cli_dbPath"
        "FILEPATH"
        "Path to directory with all DBs used by the node. \
        \If specified path doesn’t exist, a directory will be created."
    cli_rebuildDB <- optional $ Opt.switch $
        Opt.long (toOptionNameCardano "cli_rebuildDB") <>
        Opt.help "If node's database already exists, discard its contents \
             \and create a new one from scratch."
    cli_devGenesisSecretI <- autoParserCardano
        "cli_devGenesisSecretI"
        "INT"
        "Used genesis secret key index."
    cli_keyfilePath <- stringParserCardano
        "cli_keyfilePath"
        "FILEPATH"
        "Path to file with secret key (we use it for Daedalus)."
    cli_networkConfigOpts <- cliNetworkConfigOption
    cli_jlPath <- stringParserCardano
        "cli_jlPath"
        "FILEPATH"
        "Path to JSON log file."
    cli_commonArgs <- cliCommonArgsParser
    cli_updateLatestPath <- stringParserCardano
        "cli_updateLatestPath"
        "FILEPATH"
        "Path to update installer file, \
        \which should be downloaded by Update System."
    cli_updateWithPackage <- optional $ Opt.switch $
        Opt.long (toOptionNameCardano "cli_updateWithPackage") <>
        Opt.help "Enable updating via installer."
    cli_noNTP <- optional $ Opt.switch $
        Opt.long (toOptionNameCardano "cli_noNTP") <>
        Opt.help "Whether to use real NTP servers to synchronise time or rely on local time"

    cli_route53Params <- optional $ Opt.option (fromParsec addrParser) $
        Opt.long (toOptionNameCardano "cli_route53Params") <>
        Opt.metavar "IP:PORT" <>
        Opt.help "Host and port for the Route53 DNS health check."
    cli_enableMetrics <- optional $ Opt.switch $
        Opt.long (toOptionNameCardano "cli_enableMetrics") <>
        Opt.help "Enable metrics (EKG, statsd)"

    cli_ekgParams <- optional ekgParamsOption
    cli_statsdParams <- cliStatsdParamsOption

    cli_cnaDumpGenesisDataPath <- stringParserCardano
        "cli_cnaDumpGenesisDataPath"
        "FILEPATH"
        "Dump genesis data in canonical JSON format to this file."

    cli_cnaDumpConfiguration <- autoParserCardano
        "cli_cnaDumpConfiguration"
        "BOOL"
        "Dump configuration and exit."

    pure CLI_CommonNodeArgs{..}

-- Copied from Pos.Statistics.Ekg because it is not in exposed modules
ekgParamsOption :: Opt.Parser EkgParams
ekgParamsOption = do
    addr <- cliEkgServerOption
    pure $ EkgParams
        { ekgHost = fst addr
        , ekgPort = fromIntegral (snd addr)
        }
-- end Copied

-- Copied from Pos.Util.TimeWarp because it is not in exposed modules
-- | Parsed for network address in format @host:port@.
addrParser :: P.Parser NetworkAddress
addrParser = (,) <$> (encodeUtf8 <$> P.host) <*> (P.char ':' *> P.port) <* P.eof

-- | Parses an IPv4 NetworkAddress where the host is not 0.0.0.0.
addrParserNoWildcard :: P.Parser NetworkAddress
addrParserNoWildcard = do
    (host, port) <- addrParser
    if host == BS8.pack "0.0.0.0" then empty
    else return (host, port)
-- end Copied

-- Copied from Pos.Util.OptParse because it is not in exposed modules
fromParsec :: Parsec Text () a -> ReadM a
fromParsec parser =
    eitherReader $ first show . parse parser "<CLI options>" . toText
-- end Copied

cliEkgServerOption :: Opt.Parser NetworkAddress
cliEkgServerOption = Opt.option (fromParsec addrParser) $
    Opt.long (toOptionNameCardano "cli_ekgParams") <>
    Opt.metavar "IP:PORT" <>
    Opt.help "Host and port for the EKG server"

cliStatsdParamsOption :: Opt.Parser CLI_StatsdParams
cliStatsdParamsOption = do
    addr <- optional cliStatsdServerOption
    interval <- autoParserCardano
        "cli_statsdInterval"
        "MILLISECONDS"
        "Polling interval for statsd (milliseconds)"
    debug <- autoParserCardano
        "cli_statsdDebug"
        "BOOL"
        "Enable statsd debug mode"
    prefix <- (fmap . fmap) fromString $ stringParserCardano
        "cli_statsdPrefix"
        "TEXT"
        "Prefix for statsd"
    suffix <- (fmap . fmap) fromString $ stringParserCardano
        "cli_statsdSuffix"
        "TEXT"
        "Suffix for statsd"
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
    Opt.long (toOptionNameCardano "cli_statsdAddr") <>
    Opt.metavar "IP:PORT" <>
    Opt.help "Host and port for the statsd server"

cliNetworkConfigOption :: Opt.Parser CLI_NetworkConfigOpts
cliNetworkConfigOption = do
    cli_ncoTopology <- stringParserCardano
            "cli_ncoTopology"
            "FILEPATH"
            "Path to a YAML file containing the network topology"

    cli_ncoKademlia <- stringParserCardano
            "cli_ncoKademlia"
            "FILEPATH"
            "Path to a YAML file containing the kademlia configuration"

    cli_ncoSelf <- (fmap . fmap) fromString $ stringParserCardano
            "cli_ncoSelf"
            "NODE_ID"
            "Identifier for this node within the network"

    cli_ncoPort <- autoParserCardano
            "cli_ncoPort"
            "PORT"
            "Port number for IP address to node ID translation"

    cli_ncoPolicies <- stringParserCardano
            "cli_ncoPolicies"
            "FILEPATH"
            "Path to a YAML file containing the network policies"

    cli_ncoExternalAddress <- cliExternalNetworkAddressOption
    cli_ncoBindAddress <- cliListenNetworkAddressOption
    pure CLI_NetworkConfigOpts {..}

cliExternalNetworkAddressOption :: Opt.Parser (Maybe NetworkAddress)
cliExternalNetworkAddressOption = optional $ Opt.option (fromParsec addrParserNoWildcard) $ templateParser
        (toOptionNameCardano "cli_ncoExternalAddress")
        "IP:PORT"
        "IP and port of external address. \
        \Please make sure these IP and port (on which node is running) are accessible \
        \otherwise proper work of CSL isn't guaranteed. \
        \0.0.0.0 is not accepted as a valid host."

cliListenNetworkAddressOption :: Opt.Parser (Maybe NetworkAddress)
cliListenNetworkAddressOption = optional $ Opt.option (fromParsec addrParserNoWildcard) $ templateParser
        (toOptionNameCardano "cli_ncoBindAddress")
        "IP:PORT"
        "IP and port on which to bind and listen. Please make sure these IP \
        \and port are accessible, otherwise proper work of CSL isn't guaranteed."

cliConfigurationOptionsParser :: Opt.Parser CLI_ConfigurationOptions
cliConfigurationOptionsParser = do
    cli_cfoFilePath  <- stringParserCardano
        "cli_cfoFilePath"
        "FILEPATH"
        "Path to a yaml configuration file"
    cli_cfoKey <- (fmap . fmap) fromString $ stringParserCardano
        "cli_cfoKey"
        "TEXT"
        "Key within the configuration file to use"
    cli_cfoSystemStart <- (fmap . fmap) (Timestamp . sec) $ autoParserCardano
        "cli_cfoSystemStart"
        "TIMESTAMP"
        "System start time. Format - seconds since Unix Epoch."
    cli_cfoSeed        <- autoParserCardano
        "cli_cfoSeed"
        "INTEGER"
        "Seed for genesis generation. Overrides one from configuration file."
    return CLI_ConfigurationOptions{..}

cliCommonArgsParser :: Opt.Parser CLI_CommonArgs
cliCommonArgsParser = do
    cli_logConfig <- stringParserCardano
        "cli_logConfig"
        "FILEPATH"
        "Path to logger configuration."
    cli_logPrefix <- stringParserCardano
        "cli_logPrefix"
        "FILEPATH"
        "Prefix to logger output path."
    cli_reportServers <- optional $ many $
        fromString <$>
        Opt.strOption
            (templateParser
                (toOptionNameCardano "cli_reportServers")
                "URI"
                "Reporting server to send crash/error logs on.")
    cli_updateServers <- optional $ many $
        fromString <$>
        Opt.strOption
            (templateParser
                (toOptionNameCardano "cli_updateServers")
                "URI"
                "Server to download updates from.")
    cli_configurationOptions <- cliConfigurationOptionsParser
    pure CLI_CommonArgs {..}


autoParser :: (Read a) => (String -> String) -> String -> String -> String -> Opt.Parser (Maybe a)
autoParser toComponentOptionName name meta help =
    Opt.optional $ Opt.option Opt.auto $
        mconcat
            [ Opt.long $ toComponentOptionName name
            , Opt.metavar meta
            , Opt.help help
            ]

autoParserCardano :: (Read a) => String -> String -> String -> Opt.Parser (Maybe a)
autoParserCardano = autoParser toOptionNameCardano

stringParserCardano :: String -> String -> String -> Opt.Parser (Maybe String)
stringParserCardano name meta help =
    optional $ Opt.strOption $ templateParser (toOptionNameCardano name) meta help

toOptionName :: String -> (D.Text -> D.Text) -> String -> String
toOptionName componentPrefix componentFieldModifier name = componentPrefix <> toString (componentFieldModifier (fromString name_))
  where
    name_ = fromMaybe
      (error $ fromString ("Name " <> name <> " should contain cli_ prefix."))
      (stripPrefix "cli_" name)

toOptionNameCardano :: String -> String
toOptionNameCardano = toOptionName "cardano:" cardanoFieldModifier

toOptionNameWallet :: String -> String
toOptionNameWallet = toOptionName "wallet:" walletFieldModifier

autoParserWallet :: (Read a) => String -> String -> String -> Opt.Parser (Maybe a)
autoParserWallet = autoParser toOptionNameWallet
