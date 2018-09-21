{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Ariadne.Config.CLI
       ( getConfig
         -- * Exported for testing
       , opts
       , mergeConfigs
       ) where

import Control.Lens (makeLensesWith, zoom, (%=))
import Data.List.Utils (replace)
import Data.Time.Units (fromMicroseconds)
import Data.Version (showVersion)
import qualified Dhall as D
import Distribution.System (OS(..), buildOS)
import Formatting (sformat, string, (%))
import Options.Applicative
  (auto, help, long, metavar, option, showDefault, strOption, switch, value)
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
  (XdgDirectory(..), createDirectoryIfMissing, doesFileExist,
  getCurrentDirectory, getXdgDirectory)
import System.FilePath (isAbsolute, takeDirectory, (</>))

import Ariadne.Config.Ariadne
  (AriadneConfig(..), acCardanoL, acHistoryL, acWalletL, defaultAriadneConfig)
import Ariadne.Config.Cardano
import Ariadne.Config.DhallUtil (fromDhall)
import Ariadne.Config.History
import Ariadne.Config.Update
import Ariadne.Config.Wallet
  (WalletConfig(..), walletFieldModifier, wcAcidDBPathL, wcKeyfilePathL)
import Ariadne.Util

-- All leaves have type Maybe a to provide an ability to override any field
-- except EkgParams due to its parser
data CLI_AriadneConfig = CLI_AriadneConfig
    { cli_acCardano :: !CLI_CardanoConfig
    , cli_acWallet :: !CLI_WalletConfig
    , cli_acUpdate :: !CLI_UpdateConfig
    , cli_acHistory :: !CLI_HistoryConfig
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
    , cli_wcAcidDBPath  :: !(Maybe FilePath)
    } deriving (Eq, Show)

data CLI_UpdateConfig = CLI_UpdateConfig
    { cli_ucVersionCheckUrl :: !(Maybe Text)
    , cli_ucUpdateUrl :: !(Maybe Text)
    , cli_ucCheckDelay :: !(Maybe Int)
    } deriving (Eq, Show)

data CLI_HistoryConfig = CLI_HistoryConfig
    { cli_hcPath :: !(Maybe FilePath)
    } deriving (Eq, Show)

makeLensesWith postfixLFields ''CLI_ConfigurationOptions
makeLensesWith postfixLFields ''CLI_CardanoConfig
makeLensesWith postfixLFields ''CLI_HistoryConfig

makeLensesWith postfixLFields ''ConfigurationOptions

-- Poor man's merge config
mergeConfigs :: CLI_AriadneConfig -> AriadneConfig -> AriadneConfig
mergeConfigs overrideAc defaultAc = mergedAriadneConfig
  where
    merge :: Maybe a -> a -> a
    merge = flip fromMaybe

    mergedAriadneConfig = AriadneConfig
        { acCardano = mergedCardanoConfig
        , acWallet = mergedWalletConfig
        , acUpdate = mergedUpdateConfig
        , acHistory = mergedHistoryConfig
        }

    -- Merge Wallet config
    overrideWc = cli_acWallet overrideAc
    defaultWc = acWallet defaultAc
    mergedWalletConfig = WalletConfig
        { wcEntropySize =
            cli_wcEntropySize overrideWc `merge` wcEntropySize defaultWc
        , wcKeyfilePath =
            cli_wcKeyfilePath overrideWc `merge` wcKeyfilePath defaultWc
        , wcAcidDBPath =
            cli_wcAcidDBPath overrideWc `merge` wcAcidDBPath defaultWc
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
        , ccEkgParams =
            (overrideCC ^. cli_ekgParamsL) <|> ccEkgParams defaultCC
        }

    mergedConfigurationOptions = defaultCO
        { cfoFilePath = merge (overrideCO ^. cli_cfoFilePathL) (defaultCO ^. cfoFilePathL)
        , cfoKey = merge (overrideCO ^. cli_cfoKeyL) (defaultCO ^. cfoKeyL)
        , cfoSystemStart = (overrideCO ^. cli_cfoSystemStartL) <|> (defaultCO ^. cfoSystemStartL)
        , cfoSeed = (overrideCO ^. cli_cfoSeedL) <|> (defaultCO ^. cfoSeedL)
        }

    -- Merge Update config
    overrideUc = cli_acUpdate overrideAc
    defaultUc = acUpdate defaultAc
    mergedUpdateConfig = defaultUc
        { ucVersionCheckUrl = merge (cli_ucVersionCheckUrl overrideUc) (ucVersionCheckUrl defaultUc)
        , ucUpdateUrl = merge (cli_ucUpdateUrl overrideUc) (ucUpdateUrl defaultUc)
        , ucCheckDelay = merge (cli_ucCheckDelay overrideUc) (ucCheckDelay defaultUc)
        }

    -- Merge History config
    overrideHc = cli_acHistory overrideAc
    defaultHc = acHistory defaultAc
    mergedHistoryConfig = defaultHc
        { hcPath = merge (overrideHc ^. cli_hcPathL) (defaultHc ^. hcPathL)
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
      -- If default config is used we create data directory because
      -- all data is put there.
      createDirectoryIfMissing True xdgDataPath
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
        zoom acHistoryL $ do
          hcPathL %= resolve_
        zoom acWalletL $ do
          wcAcidDBPathL %= resolve_
          wcKeyfilePathL %= resolve_


      resolve :: FilePath -> ConfigDirectories -> FilePath -> FilePath
      resolve prefix ConfigDirectories{..} path
        | "@DATA" `isPrefixOf` path = replace "@DATA" cdDataDir path
        | "@PWD" `isPrefixOf` path = replace "@PWD" cdPWD path
        | isAbsoluteConsiderTilde path = path
        | otherwise = prefix </> path

      isAbsoluteConsiderTilde :: FilePath -> Bool
      isAbsoluteConsiderTilde p
        | buildOS == Windows = isAbsolute p
        | "~/" `isPrefixOf` p = True
        | otherwise = isAbsolute p

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
    , showDefault
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
  cli_acUpdate <- cliUpdateParser
  cli_acHistory <- cliHistoryParser
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
  cli_wcAcidDBPath <- optional $ strOption $ mconcat
     [ long $ toOptionNameWallet "wcAcidDBPath"
     , metavar "FilePath"
     , help "Wallets database path"
     ]
  pure CLI_WalletConfig {..}
  where
  parseEntropy = fromParsec byte >>= \b -> if b `elem` [16, 20, 24, 28, 32]
    then return b
    else err b
  err inp = Opt.readerError $ "Invalid entropy size " <> (show inp) <> ". Chose one of [16, 20, 24, 28, 32]"

cliUpdateParser :: Opt.Parser CLI_UpdateConfig
cliUpdateParser = do
  cli_ucVersionCheckUrl <- optional $ strOption $ mconcat
    [ long $ toOptionNameUpdate "ucVersionCheckUrl"
    , metavar "URL"
    , help "URL used to get the latest software version"
    ]
  cli_ucUpdateUrl <- optional $ strOption $ mconcat
    [ long $ toOptionNameUpdate "ucUpdateUrl"
    , metavar "URL"
    , help "URL displayed in the message about a new version"
    ]
  cli_ucCheckDelay  <- optional $ option auto $ mconcat
    [ long $ toOptionNameUpdate "ucCheckDelay"
    , metavar "INT'"
    , help "How often to check for a new version (in seconds)"
    ]
  pure CLI_UpdateConfig {..}

cliHistoryParser :: Opt.Parser CLI_HistoryConfig
cliHistoryParser = do
  cli_hcPath <- optional $ strOption $ mconcat
    [ long $ toOptionNameHistory "hcPath"
    , metavar "FILEPATH"
    , help "Path to file with command history."
    ]
  pure CLI_HistoryConfig {..}

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

toOptionNameUpdate :: D.Text -> String
toOptionNameUpdate = ("update:" <>) . toString . updateFieldModifier

toOptionNameHistory :: D.Text -> String
toOptionNameHistory = ("history:" <>) . toString . historyFieldModifier
