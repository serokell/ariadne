module Ariadne.Config.CLI
    ( getConfig
    -- * Exported for testing
    , opts
    , mergeConfigs
    ) where

import Universum

import Control.Lens (makeLensesWith, (%=))
import Data.List.Utils (replace)
import Data.Version (showVersion)
import qualified Dhall as D
import Distribution.System (OS(..), buildOS)
import Formatting (sformat, string, (%))
import IiExtras (postfixLFields)
import Options.Applicative
  (auto, help, long, metavar, option, strOption, switch, value)
import qualified Options.Applicative as Opt
import Paths_ariadne (version)
import Pos.Infra.Statistics (EkgParams(..), StatsdParams(..))
import Pos.Infra.Util.TimeWarp
  (NetworkAddress, addrParser, addrParserNoWildcard)
import Pos.Launcher ()
import Serokell.Data.Memory.Units (Byte, fromBytes)
import Serokell.Util.OptParse (fromParsec)
import Serokell.Util.Parse (byte)
import System.Directory
  (XdgDirectory(..), doesFileExist, getCurrentDirectory, getXdgDirectory)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import qualified Text.Read as R (readEither)

import Ariadne.Config.Ariadne (AriadneConfig(..), defaultAriadneConfig)
import Ariadne.Config.Cardano (CardanoConfig(CardanoConfig), CommonArgs(..), CommonNodeArgs(..)
    , ConfigurationOptions (..), NetworkConfigOpts(..), cardanoFieldModifier)
import Ariadne.Config.DhallUtil (fromDhall)
import Ariadne.Config.Presence (Presence (File), _File)
import Ariadne.Config.Wallet (WalletConfig(..), walletFieldModifier)

newtype CLI_CardanoConfig = CLI_CardanoConfig
    { cli_getCardanoConfig :: CLI_CommonNodeArgs
    }

data CLI_AriadneConfig = CLI_AriadneConfig
    { cli_acCardano :: CLI_CardanoConfig
    , cli_acWallet :: CLI_WalletConfig
    }

data CLI_WalletConfig = CLI_WalletConfig
    { cli_wcEntropySize :: Maybe Byte
    }

data CLI_NetworkConfigOpts = CLI_NetworkConfigOpts
    { cli_ncoTopology        :: !(Maybe FilePath)
    , cli_ncoPort            :: !(Maybe Word16)
    }

data CLI_CommonArgs = CLI_CommonArgs
    { cli_logConfig            :: !(Maybe FilePath)
    , cli_logPrefix            :: !(Maybe FilePath)
    , cli_configurationOptions :: !CLI_ConfigurationOptions
    }

data CLI_ConfigurationOptions = CLI_ConfigurationOptions
    { cli_cfoFilePath :: !(Maybe FilePath) }

-- All leaves have type Maybe a to provide an ability to override any field
-- except NetworkAddress and EkgParams due to their parsers
data CLI_CommonNodeArgs = CLI_CommonNodeArgs
    { cli_dbPath                 :: !(Maybe FilePath)
    , cli_rebuildDB              :: !(Maybe Bool)
    , cli_devGenesisSecretI      :: !(Maybe Int)
    , cli_keyfilePath            :: !(Maybe FilePath)
    , cli_networkConfigOpts      :: !CLI_NetworkConfigOpts
    , cli_commonArgs             :: !CLI_CommonArgs
    , cli_enableMetrics          :: !(Maybe Bool)
    , cli_ekgParams              :: !(Maybe EkgParams)
    }

makeLensesWith postfixLFields ''CLI_CommonArgs
makeLensesWith postfixLFields ''CLI_NetworkConfigOpts
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
    -- TODO: AD-175 Overridable update configuration
    mergedAriadneConfig = AriadneConfig mergedCardanoConfig mergedWalletConfig (defaultAc ^. acUpdateL) mergedHistoryConfig

    mergedWalletConfig = WalletConfig $ merge (overrideAc ^. cli_acWalletL . cli_wcEntropySizeL) (defaultAc ^. acWalletL . wcEntropySizeL)

    mergedHistoryConfig = defaultAc ^. acHistoryL

    overrideCna = overrideAc ^. cli_acCardanoL . cli_getCardanoConfigL
    defaultCna = defaultAc ^. acCardanoL . getCardanoConfigL

    overrideNco = overrideCna ^. cli_networkConfigOptsL
    defaultNco = defaultCna ^. networkConfigOptsL

    overrideCa = overrideCna ^. cli_commonArgsL
    defaultCa = defaultCna ^. commonArgsL

    defaultCo = defaultCa ^. configurationOptionsL

    mergedCardanoConfig = CardanoConfig CommonNodeArgs
        { dbPath = (overrideCna ^. cli_dbPathL) <|> (defaultCna ^. dbPathL)
        , rebuildDB = merge (overrideCna ^. cli_rebuildDBL) (defaultCna ^. rebuildDBL)
        , devGenesisSecretI = (overrideCna ^. cli_devGenesisSecretIL) <|> (defaultCna ^. devGenesisSecretIL)
        , keyfilePath = merge (overrideCna ^. cli_keyfilePathL) (defaultCna ^. keyfilePathL)
        , networkConfigOpts = mergedNetworkConfigOpts
        , commonArgs = mergedCommonArgs
        , enableMetrics = merge (overrideCna ^. cli_enableMetricsL) (defaultCna ^. enableMetricsL)
        , ekgParams = (overrideCna ^. cli_ekgParamsL) <|> (defaultCna ^. ekgParamsL)
        }

    mergedNetworkConfigOpts = NetworkConfigOpts
        { ncoTopology  = (File $ overrideNco ^. cli_ncoTopologyL) <|> (defaultNco ^. ncoTopologyL)
        , ncoPort = merge (overrideNco ^. cli_ncoPortL) (defaultNco ^. ncoPortL)
        }

    mergedCommonArgs = CommonArgs
        { logConfig = File $ (overrideCa ^. cli_logConfigL) <|> (defaultCa ^. logConfigL ^? _File)
        , logPrefix = (overrideCa ^. cli_logPrefixL) <|> (defaultCa ^. logPrefixL)
        , configurationOptions = ConfigurationOptions $ defaultCo ^. cfoL
        }

merge :: Maybe a -> a -> a
merge = flip fromMaybe

data ConfigDirectories = ConfigDirectories
  { cdDataDir :: !FilePath
  , cdPWD :: !FilePath }

getConfig :: String -> IO AriadneConfig
getConfig commitHash = do
  xdgConfigPath <- getXdgDirectory XdgConfig "ariadne"
  (configPath, printVersion, cli_config) <- Opt.execParser (opts xdgConfigPath)
  when printVersion $ do
    putTextLn $ sformat ("Ariadne v"%string%" commit:"%string) (showVersion version) commitHash
    exitSuccess

  config <- ifM (doesFileExist configPath)
    (do
      -- Dhall will throw well formatted colourful error message
      -- if something goes wrong

      -- Passing path as dhall import is needed for relative import paths
      -- to be relative to the config path.
      unresolved <- fromDhall @AriadneConfig $ toDhallImport configPath
      configDirs <- ConfigDirectories <$> getXdgDirectory XdgData "ariadne" <*> getCurrentDirectory
      return (resolvePaths unresolved configPath configDirs))
    (do
      putStrLn $ sformat ("File "%string%" not found. Default config will be used.") configPath
      return defaultAriadneConfig)

  return $ mergeConfigs cli_config config
    where
      resolvePaths :: AriadneConfig -> FilePath -> ConfigDirectories -> AriadneConfig
      resolvePaths unresolved ariadneConfigPath configDirs =
        execState (resolveState (takeDirectory ariadneConfigPath) configDirs) unresolved

      resolveState :: FilePath -> ConfigDirectories -> State AriadneConfig ()
      resolveState ariadneConfigDir configDirs = do
        let commNodeArgsL = acCardanoL . getCardanoConfigL
            resolve_ = resolve ariadneConfigDir configDirs
        commNodeArgsL.dbPathL %= (fmap resolve_)
        commNodeArgsL.keyfilePathL %= resolve_

      resolve :: FilePath -> ConfigDirectories -> FilePath -> FilePath
      resolve prefix ConfigDirectories{..} path
        | isPrefixOf "@DATA" path = replace "@DATA" cdDataDir path
        | isPrefixOf "@PWD" path = replace "@PWD" cdPWD path
        | isAbsoluteConsiderTilde path = path
        | otherwise = prefix </> path

      isAbsoluteConsiderTilde :: FilePath -> Bool
      isAbsoluteConsiderTilde p = if buildOS == Windows
        then isAbsolute p
        else if isPrefixOf "~/" p
          then True
          else isAbsolute p

      toDhallImport :: FilePath -> D.Text
      toDhallImport = fromString . f
        where
          f path
            | isAbsoluteConsiderTilde path = path --relative paths without `.` are invalid in dhall.
            | otherwise = "." </> path

opts :: FilePath -> Opt.ParserInfo (FilePath, Bool, CLI_AriadneConfig)
opts xdgConfigPath = Opt.info ((parseOptions xdgConfigPath) <**> Opt.helper)
  (  Opt.fullDesc
  <> Opt.header "Ariadne wallet"
  <> Opt.footer ("For more details see https://serokell.io/ariadne/"))

parseOptions :: FilePath -> Opt.Parser (FilePath, Bool, CLI_AriadneConfig)
parseOptions xdgConfigPath = do
  configPath <- strOption $ mconcat
    [ long "config"
    , metavar "FILEPATH"
    , value (xdgConfigPath </> "ariadne-config.dhall")
    , help "Path to ariadne .dhall configuration file"
    ]
  printVersion <- switch $ mconcat
    [ long "version"
    , help "Show current version"
    ]
  cli_ariadneConfig <- cliAriadneConfigParser
  return (configPath, printVersion, cli_ariadneConfig)

cliAriadneConfigParser :: Opt.Parser CLI_AriadneConfig
cliAriadneConfigParser = do
  cli_acCardano <- CLI_CardanoConfig <$> cliCommonNodeArgsParser
  cli_acWallet <- cliWalletParser
  pure CLI_AriadneConfig {..}

cliWalletParser :: Opt.Parser CLI_WalletConfig
cliWalletParser = do
  cli_wcEntropySize <- (fmap . fmap) (fromBytes . fromIntegral) $ optional $ Opt.option parseEntropy $ mconcat
     [ long $ toOptionNameWallet "wcEntropySize"
     , metavar "BYTE"
     , help "Entropy size in bytes, valid values are: [16, 20, 24, 28, 32]"
     ]
  pure CLI_WalletConfig {..}
  where
  parseEntropy = fromParsec byte >>= \b -> if b `elem` [16, 20, 24, 28, 32]
    then return b
    else err b
  err inp = Opt.readerError $ "Invalid entropy size " <> (show inp) <> ". Chose one of [16, 20, 24, 28, 32]"

cliCommonNodeArgsParser :: Opt.Parser CLI_CommonNodeArgs
cliCommonNodeArgsParser = do
  cli_dbPath <- optional $ strOption $ mconcat
    [ long $ toOptionNameCardano "dbPath"
    , metavar "FILEPATH"
    , help "Path to directory with all DBs used by the node. \
    \If specified path doesn’t exist, a directory will be created."
    ]
  cli_rebuildDB <- optional $ option auto $ mconcat
    [ long $ toOptionNameCardano "rebuildDB"
    , metavar "BOOL"
    , help "If node's database already exists, discard its contents \
    \and create a new one from scratch."
    ]
  cli_devGenesisSecretI <- optional $ option auto $ mconcat
    [ long $ toOptionNameCardano "devGenesisSecretI"
    , metavar "INT"
    , help "Used genesis secret key index."
    ]
  cli_keyfilePath <- optional $ strOption $ mconcat
    [ long $ toOptionNameCardano "keyfilePath"
    , metavar "FILEPATH"
    , help "Path to file with secret key (we use it for Daedalus)."
    ]
  cli_networkConfigOpts <- cliNetworkConfigOption
  cli_commonArgs <- cliCommonArgsParser
  cli_enableMetrics <- optional $ option auto $ mconcat
    [ long $ toOptionNameCardano "enableMetrics"
    , metavar "BOOL"
    , help "Enable metrics (EKG, statsd)"
    ]
  cli_ekgParams <- optional cliEkgParamsOption
  pure CLI_CommonNodeArgs{..}

cliEkgParamsOption :: Opt.Parser EkgParams
cliEkgParamsOption = do
  addr <- cliEkgServerOption
  pure $ EkgParams
    { ekgHost = fst addr
    , ekgPort = fromIntegral (snd addr)
    }

cliEkgServerOption :: Opt.Parser NetworkAddress
cliEkgServerOption = option (fromParsec addrParser) $ mconcat
  [ long $ toOptionNameCardano "ekgParams"
  , metavar "IP:PORT"
  , help "Host and port for the EKG server"
  ]

cliNetworkConfigOption :: Opt.Parser CLI_NetworkConfigOpts
cliNetworkConfigOption = do
  cli_ncoTopology <- optional $ strOption $ mconcat
    [ long $ toOptionNameCardano "ncoTopology"
    , metavar "FILEPATH"
    , help "Path to a YAML file containing the network topology"
    ]
  cli_ncoPort <- optional $ option auto $ mconcat
    [ long $ toOptionNameCardano "ncoPort"
    , metavar "PORT"
    , help "Port number for IP address to node ID translation"
    ]
  pure CLI_NetworkConfigOpts {..}

cliExternalNetworkAddressOption :: Opt.Parser (Maybe NetworkAddress)
cliExternalNetworkAddressOption = optional $ option (fromParsec addrParserNoWildcard) $ mconcat
  [ long $ toOptionNameCardano "ncoExternalAddress"
  , metavar "IP:PORT"
  , help "IP and port of external address. \
  \Please make sure these IP and port (on which node is running) are accessible \
  \otherwise proper work of CSL isn't guaranteed. \
  \0.0.0.0 is not accepted as a valid host."
  ]

cliListenNetworkAddressOption :: Opt.Parser (Maybe NetworkAddress)
cliListenNetworkAddressOption = optional $ option (fromParsec addrParserNoWildcard) $ mconcat
  [ long $ toOptionNameCardano "ncoBindAddress"
  , metavar "IP:PORT"
  , help "IP and port on which to bind and listen. Please make sure these IP \
    \and port are accessible, otherwise proper work of CSL isn't guaranteed."
  ]

cliConfigurationOptionsParser :: Opt.Parser CLI_ConfigurationOptions
cliConfigurationOptionsParser = do
  cli_cfoFilePath  <- optional . strOption . mconcat $
    [ long $ toOptionNameCardano "cfoFilePath"
    , metavar "FILEPATH"
    , help "Path to a yaml configuration file"
    ]
  return CLI_ConfigurationOptions{..}

cliCommonArgsParser :: Opt.Parser CLI_CommonArgs
cliCommonArgsParser = do
  cli_logConfig <- optional . strOption . mconcat $
    [ long $ toOptionNameCardano "logConfig"
    , metavar "FILEPATH"
    , help "Path to logger configuration."
    ]
  cli_logPrefix <- optional $ strOption $ mconcat
    [ long $ toOptionNameCardano "logPrefix"
    , metavar "FILEPATH"
    , help "Prefix to logger output path."
    ]
  cli_configurationOptions <- cliConfigurationOptionsParser
  pure CLI_CommonArgs {..}

toOptionNameCardano :: D.Text -> String
toOptionNameCardano = ("cardano:" <>) . toString . cardanoFieldModifier

toOptionNameWallet :: D.Text -> String
toOptionNameWallet =  ("wallet:" <>) . toString . walletFieldModifier

listParser :: Opt.ReadM [Text]
listParser =  Opt.eitherReader (R.readEither @[Text])
