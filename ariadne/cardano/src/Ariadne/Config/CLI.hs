{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Ariadne.Config.CLI
    ( getConfig
    -- * Exported for testing
    , opts
    , mergeConfigs
    ) where

import Universum

import Control.Lens (makeLensesWith, zoom, (%=))
import Data.List.Utils (replace)
import Data.Time.Units (fromMicroseconds)
import Data.Version (showVersion)
import qualified Dhall as D
import Distribution.System (OS(..), buildOS)
import Formatting (sformat, string, (%))
import IiExtras (postfixLFields)
import Options.Applicative
  (auto, help, long, metavar, option, strOption, switch, value)
import qualified Options.Applicative as Opt
import Paths_ariadne_cardano (version)
import Pos.Core.Slotting (Timestamp(..))
import Pos.Infra.Network.Types (NodeName(..))
import Pos.Infra.Statistics (EkgParams(..))
import Pos.Infra.Util.TimeWarp (NetworkAddress, addrParser)
import Pos.Launcher
import Serokell.Data.Memory.Units (Byte, fromBytes)
import Serokell.Util.OptParse (fromParsec)
import Serokell.Util.Parse (byte)
import System.Directory
  (XdgDirectory(..), doesFileExist, getCurrentDirectory, getXdgDirectory)
import System.FilePath (isAbsolute, takeDirectory, (</>))

import Ariadne.Config.Ariadne
  (AriadneConfig(..), acCardanoL, acWalletL, defaultAriadneConfig)
import Ariadne.Config.Cardano
import Ariadne.Config.DhallUtil (fromDhall)
import Ariadne.Config.Wallet
  (WalletConfig(..), walletFieldModifier, wcKeyfilePathL)

-- All leaves have type Maybe a to provide an ability to override any field
-- except EkgParams due to its parser
data CLI_AriadneConfig = CLI_AriadneConfig
    { cli_acCardano :: !CLI_CardanoConfig
    , cli_acWallet :: !CLI_WalletConfig
    } deriving (Eq, Show, Generic)

data CLI_CardanoConfig = CLI_CardanoConfig
    { cli_dbPath :: !(Maybe FilePath)
    , cli_rebuildDB :: !(Maybe Bool)
    , cli_networkTopology :: !(Maybe FilePath)
    , cli_networkNodeId :: !(Maybe NodeName)
    , cli_networkPort :: !(Maybe Word16)
    , cli_logConfig :: !(Maybe FilePath)
    , cli_logPrefix :: !(Maybe FilePath)
    , cli_configurationOptions :: !CLI_ConfigurationOptions
    , cli_enableMetrics :: !(Maybe Bool)
    , cli_ekgParams :: !(Maybe EkgParams)
    } deriving (Eq, Show, Generic)

data CLI_ConfigurationOptions = CLI_ConfigurationOptions
    { cli_cfoFilePath    :: !(Maybe FilePath)
    , cli_cfoKey         :: !(Maybe Text)
    , cli_cfoSystemStart :: !(Maybe Timestamp)
    , cli_cfoSeed        :: !(Maybe Integer)
    } deriving (Eq, Show, Generic)

data CLI_WalletConfig = CLI_WalletConfig
    { cli_wcEntropySize :: !(Maybe Byte)
    , cli_wcKeyfilePath :: !(Maybe FilePath)
    } deriving (Eq, Show)

makeLensesWith postfixLFields ''CLI_ConfigurationOptions
makeLensesWith postfixLFields ''CLI_CardanoConfig

makeLensesWith postfixLFields ''ConfigurationOptions

-- Poor man's merge config
mergeConfigs :: CLI_AriadneConfig -> AriadneConfig -> AriadneConfig
mergeConfigs overrideAc defaultAc = mergedAriadneConfig
  where
    merge :: Maybe a -> a -> a
    merge = flip fromMaybe

    -- TODO: AD-175 Overridable update configuration
    mergedAriadneConfig = AriadneConfig
        { acCardano = mergedCardanoConfig
        , acWallet = mergedWalletConfig
        , acUpdate = acUpdate defaultAc
        , acHistory = acHistory defaultAc
        }

    -- Merge Wallet config
    overrideWc = cli_acWallet overrideAc
    defaultWc = acWallet defaultAc
    mergedWalletConfig = WalletConfig
        { wcEntropySize =
            cli_wcEntropySize overrideWc `merge` wcEntropySize defaultWc
        , wcKeyfilePath =
            cli_wcKeyfilePath overrideWc `merge` wcKeyfilePath defaultWc
        }

    -- Merge Cardano config
    overrideCC :: CLI_CardanoConfig
    defaultCC :: CardanoConfig
    overrideCC = cli_acCardano overrideAc
    defaultCC = acCardano defaultAc

    overrideCO :: CLI_ConfigurationOptions
    defaultCO :: ConfigurationOptions
    overrideCO = cli_configurationOptions overrideCC
    defaultCO = ccConfigurationOptions defaultCC

    mergedCardanoConfig = defaultCC
        { ccDbPath = (overrideCC ^. cli_dbPathL) <|> ccDbPath defaultCC
        , ccRebuildDB =
            merge (overrideCC ^. cli_rebuildDBL) (ccRebuildDB defaultCC)
        , ccNetworkTopology  =
            (overrideCC ^. cli_networkTopologyL) <|> ccNetworkTopology defaultCC
        , ccNetworkNodeId =
            (overrideCC ^. cli_networkNodeIdL) <|> ccNetworkNodeId defaultCC
        , ccNetworkPort =
            merge (overrideCC ^. cli_networkPortL) (ccNetworkPort defaultCC)
        , ccLogConfig =
            (overrideCC ^. cli_logConfigL) <|> ccLogConfig defaultCC
        , ccLogPrefix =
            (overrideCC ^. cli_logPrefixL) <|> ccLogPrefix defaultCC
        , ccConfigurationOptions = mergedConfigurationOptions
        , ccEnableMetrics =
            merge (overrideCC ^. cli_enableMetricsL) (ccEnableMetrics defaultCC)
        , ccEkgParams =
            (overrideCC ^. cli_ekgParamsL) <|> ccEkgParams defaultCC
        }

    mergedConfigurationOptions = defaultCO
        { cfoFilePath = merge (overrideCO ^. cli_cfoFilePathL) (defaultCO ^. cfoFilePathL)
        , cfoKey = merge (overrideCO ^. cli_cfoKeyL) (defaultCO ^. cfoKeyL)
        , cfoSystemStart = (overrideCO ^. cli_cfoSystemStartL) <|> (defaultCO ^. cfoSystemStartL)
        , cfoSeed = (overrideCO ^. cli_cfoSeedL) <|> (defaultCO ^. cfoSeedL)
        }

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

  xdgDataPath <- getXdgDirectory XdgData "ariadne"
  configDirs <- ConfigDirectories xdgDataPath <$> getCurrentDirectory
  unresolvedConfig <- ifM (doesFileExist configPath)
    -- Dhall will throw well formatted colourful error message
    -- if something goes wrong

    -- Passing path as dhall import is needed for relative import paths
    -- to be relative to the config path.
    (fromDhall @AriadneConfig $ toDhallImport configPath)
    (do
      putStrLn $ sformat ("File "%string%" not found. Default config will be used.") configPath
      return (defaultAriadneConfig xdgDataPath))

  let config = resolvePaths unresolvedConfig configPath configDirs
  return $ mergeConfigs cli_config config
    where
      resolvePaths :: AriadneConfig -> FilePath -> ConfigDirectories -> AriadneConfig
      resolvePaths unresolved ariadneConfigPath configDirs =
        execState (resolveState (takeDirectory ariadneConfigPath) configDirs) unresolved

      resolveState :: FilePath -> ConfigDirectories -> State AriadneConfig ()
      resolveState ariadneConfigDir configDirs = do
        let resolve_ = resolve ariadneConfigDir configDirs
        zoom acCardanoL $ do
          ccDbPathL %= fmap resolve_
          ccNetworkTopologyL %= fmap resolve_
          ccLogConfigL %= fmap resolve_
          ccLogPrefixL %= fmap resolve_
          ccConfigurationOptionsL.cfoFilePathL %= resolve_
        zoom acWalletL $ do
          wcKeyfilePathL %= resolve_

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
  cli_acCardano <- cliCardanoConfigParser
  cli_acWallet <- cliWalletParser
  pure CLI_AriadneConfig {..}

cliWalletParser :: Opt.Parser CLI_WalletConfig
cliWalletParser = do
  cli_wcEntropySize <- (fmap . fmap) (fromBytes . fromIntegral) $ optional $ Opt.option parseEntropy $ mconcat
     [ long $ toOptionNameWallet "wcEntropySize"
     , metavar "BYTE"
     , help "Entropy size in bytes, valid values are: [16, 20, 24, 28, 32]"
     ]
  cli_wcKeyfilePath <- optional $ strOption $ mconcat
    [ long $ toOptionNameWallet "wcKeyfilePath"
    , metavar "FILEPATH"
    , help "Path to file with secret key."
    ]
  pure CLI_WalletConfig {..}
  where
  parseEntropy = fromParsec byte >>= \b -> if b `elem` [16, 20, 24, 28, 32]
    then return b
    else err b
  err inp = Opt.readerError $ "Invalid entropy size " <> (show inp) <> ". Chose one of [16, 20, 24, 28, 32]"

cliCardanoConfigParser :: Opt.Parser CLI_CardanoConfig
cliCardanoConfigParser = do
  cli_dbPath <- optional $ strOption $ mconcat
    [ long $ toOptionNameCardano "ccDbPath"
    , metavar "FILEPATH"
    , help "Path to directory with all DBs used by the node. \
    \If specified path doesn’t exist, a directory will be created."
    ]
  cli_rebuildDB <- optional $ option auto $ mconcat
    [ long $ toOptionNameCardano "ccRebuildDB"
    , metavar "BOOL"
    , help "If node's database already exists, discard its contents \
    \and create a new one from scratch."
    ]
  cli_networkTopology <- optional $ strOption $ mconcat
    [ long $ toOptionNameCardano "ccNetworkTopology"
    , metavar "FILEPATH"
    , help "Path to a YAML file containing the network topology"
    ]
  cli_networkNodeId <- optional $ strOption $ mconcat
    [ long $ toOptionNameCardano "ccNetworkNodeId"
    , metavar "NODE_ID"
    , help "Identifier for this node within the network"
    ]
  cli_networkPort <- optional $ option auto $ mconcat
    [ long $ toOptionNameCardano "ccNetworkPort"
    , metavar "PORT"
    , help "Port number for IP address to node ID translation"
    ]
  cli_logConfig <- optional $ strOption $ mconcat
    [ long $ toOptionNameCardano "ccLogConfig"
    , metavar "FILEPATH"
    , help "Path to logger configuration."
    ]
  cli_logPrefix <- optional $ strOption $ mconcat
    [ long $ toOptionNameCardano "ccLogPrefix"
    , metavar "FILEPATH"
    , help "Prefix to logger output path."
    ]

  cli_configurationOptions <- cliConfigurationOptionsParser

  cli_enableMetrics <- optional $ option auto $ mconcat
    [ long $ toOptionNameCardano "ccEnableMetrics"
    , metavar "BOOL"
    , help "Enable metrics (EKG)."
    ]
  cli_ekgParams <- optional cliEkgParamsOption

  pure CLI_CardanoConfig{..}

cliEkgParamsOption :: Opt.Parser EkgParams
cliEkgParamsOption = do
  addr <- cliEkgServerOption
  pure $ EkgParams
    { ekgHost = fst addr
    , ekgPort = fromIntegral (snd addr)
    }

cliEkgServerOption :: Opt.Parser NetworkAddress
cliEkgServerOption = option (fromParsec addrParser) $ mconcat
  [ long $ toOptionNameCardano "ccEkgParams"
  , metavar "IP:PORT"
  , help "Host and port for the EKG server"
  ]

cliConfigurationOptionsParser :: Opt.Parser CLI_ConfigurationOptions
cliConfigurationOptionsParser = do
  cli_cfoFilePath  <- optional $ strOption $ mconcat
    [ long $ toOptionNameCardano "cfoFilePath"
    , metavar "FILEPATH"
    , help "Path to a yaml configuration file"
    ]
  cli_cfoKey <- optional $ strOption $ mconcat
    [ long $ toOptionNameCardano "cfoKey"
    , metavar "TEXT"
    , help "Key within the configuration file to use"
    ]
  cli_cfoSystemStart <-
      (fmap . fmap) (Timestamp . fromMicroseconds . (*) 1000000) $
      optional $ option auto $
      mconcat
    [ long $ toOptionNameCardano "cfoSystemStart"
    , metavar "TIMESTAMP"
    , help "System start time. Format - seconds since Unix Epoch."
    ]
  cli_cfoSeed        <- optional $ option auto $ mconcat
    [ long $ toOptionNameCardano "cfoSeed"
    , metavar "INTEGER"
    , help "Seed for genesis generation. Overrides one from configuration file."
    ]
  return CLI_ConfigurationOptions{..}

toOptionNameCardano :: D.Text -> String
toOptionNameCardano = ("cardano:" <>) . toString . cardanoFieldModifier

toOptionNameWallet :: D.Text -> String
toOptionNameWallet =  ("wallet:" <>) . toString . walletFieldModifier
