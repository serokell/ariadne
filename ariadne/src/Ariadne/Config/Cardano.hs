module Ariadne.Config.Cardano
  ( defaultCardanoConfig
  , cardanoFieldModifier
  , CardanoConfig (..)
  , CommonArgs (..)
  , toCardanoCommonArgs
  , CommonNodeArgs (..)
  , toCardanoCommonNodeArgs
  , ConfigurationOptions (..)
  , toCardanoConfigurationOptions
  , NodeParams (..)
  , toCardanoNodeParams
  , NetworkConfigOpts (..)
  , toCardanoNetworkConfigOpts
  , getNodeParams
  , gtSscParams
  , loggingParams
  ) where

import Universum hiding (show)

import Ariadne.Config.DhallUtil
import Ariadne.Cardano.Orphans ()
import Ariadne.Config.Presence (Presence (File, There), _File)
import Control.Lens (ix)
import Crypto.Random (MonadRandom)
import Data.Default (Default(def))
import Data.FileEmbed (embedFile)
import Data.Functor.Contravariant (Contravariant(..))
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.Time.Units (Microsecond, Second, convertUnit, fromMicroseconds)
import Data.Yaml (decodeEither', decodeFileEither)
import qualified Dhall as D
import Dhall.Core (Expr(..))
import qualified Dhall.Core as Core
import Dhall.Parser (Src(..))
import Dhall.TypeCheck (X)
import Pos.Behavior (BehaviorConfig(..))
import Pos.Client.CLI (NodeArgs(..))
import Pos.Core (HasConfiguration, RichSecrets (rsPrimaryKey, rsVssKeyPair), genesisSecretsRich)
import Pos.Core.Slotting (Timestamp(..))
import Pos.Crypto (SecretKey, VssKeyPair, keyGen, runSecureRandom, vssKeyGen)
import Pos.Infra.DHT.Real.Param (KademliaParams(..))
import Pos.Infra.Network.CLI (intNetworkConfigOpts)
import Pos.Infra.Network.Types (NetworkConfig, NodeName(..))
import Pos.Infra.Network.Yaml (Topology(..))
import Pos.Infra.Statistics (EkgParams(..), StatsdParams(..))
import Pos.Infra.Util.TimeWarp (NetworkAddress)
import Pos.Launcher (BaseParams(..), Configuration (..), LoggingParams(..))
import Pos.Ssc.Types (SscParams(..))
import Pos.Update.Params (UpdateParams(..))
import Pos.Util.UserSecret (UserSecret, peekUserSecret, usPrimKey, usVss, writeUserSecret)
import Pos.Util.Util (eitherToThrow)
import System.Directory (getCurrentDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Wlog (LoggerConfig(..), WithLogger, logInfo)
import System.Wlog.LoggerName (LoggerName)
import Text.Show (Show(show))

import qualified Pos.Launcher as Cardano (ConfigurationOptions(..), NodeParams(..))
import qualified Pos.Client.CLI.NodeOptions as Cardano (CommonNodeArgs(..))
import qualified Pos.Client.CLI.Options as Cardano (CommonArgs(..))
import qualified Pos.Infra.Network.CLI as Cardano (NetworkConfigOpts(..))

data CommonNodeArgs = CommonNodeArgs
    { dbPath                 :: !(Maybe FilePath)
    , rebuildDB              :: !Bool
    , devGenesisSecretI      :: !(Maybe Int)
    , keyfilePath            :: !FilePath
    , networkConfigOpts      :: !NetworkConfigOpts
    , commonArgs             :: !CommonArgs
    , enableMetrics          :: !Bool
    , ekgParams              :: !(Maybe EkgParams)
    } deriving (Eq, Show)

toCardanoCommonNodeArgs :: CommonNodeArgs -> Cardano.CommonNodeArgs
toCardanoCommonNodeArgs CommonNodeArgs{..} = Cardano.CommonNodeArgs
    { Cardano.dbPath = dbPath
    , Cardano.rebuildDB = rebuildDB
    , Cardano.devGenesisSecretI = devGenesisSecretI
    , Cardano.keyfilePath = keyfilePath
    , Cardano.networkConfigOpts = toCardanoNetworkConfigOpts networkConfigOpts
    , Cardano.jlPath = Nothing
    , Cardano.commonArgs = toCardanoCommonArgs commonArgs
    , Cardano.updateLatestPath = ""
    , Cardano.updateWithPackage = False
    , Cardano.route53Params = Nothing
    , Cardano.enableMetrics = enableMetrics
    , Cardano.ekgParams = ekgParams
    , Cardano.statsdParams = Nothing
    , Cardano.cnaDumpGenesisDataPath = Nothing
    , Cardano.cnaDumpConfiguration = False
    }

data NodeParams = NodeParams
    { npDbPathM        :: !(Maybe FilePath)     -- ^ Path to node's database
    , npRebuildDb      :: !Bool                 -- ^ @True@ if data-base should be rebuilt
    , npSecretKey      :: !SecretKey            -- ^ Primary secret key of node
    , npUserSecret     :: !UserSecret           -- ^ All node secret keys
    , npBaseParams     :: !BaseParams           -- ^ See 'BaseParams'
    , npEnableMetrics  :: !Bool                 -- ^ Gather runtime statistics.
    , npEkgParams      :: !(Maybe EkgParams)    -- ^ EKG statistics monitoring.
    , npNetworkConfig  :: !(NetworkConfig KademliaParams)
    , npBehaviorConfig :: !BehaviorConfig       -- ^ Behavior (e.g. SSC settings)
    } deriving Show

-- Checked all fields except UserSecret, NetworkConfig and BaseParams
instance Eq NodeParams where
    np == np' = npDbPathM np == npDbPathM np'
        && npRebuildDb np == npRebuildDb np'
        && npSecretKey np == npSecretKey np'
        && npEnableMetrics np == npEnableMetrics np'
        && npEkgParams np == npEkgParams np'
        && npBehaviorConfig np == npBehaviorConfig np'

toCardanoNodeParams :: NodeParams -> Cardano.NodeParams
toCardanoNodeParams NodeParams{..} = Cardano.NodeParams
    { Cardano.npDbPathM = npDbPathM
    , Cardano.npRebuildDb = npRebuildDb
    , Cardano.npSecretKey = npSecretKey
    , Cardano.npUserSecret = npUserSecret
    , Cardano.npBaseParams = npBaseParams
    , Cardano.npJLFile = Nothing
    , Cardano.npReportServers = []
    , Cardano.npUpdateParams = UpdateParams "" False []
    , Cardano.npRoute53Params = Nothing
    , Cardano.npEnableMetrics = npEnableMetrics
    , Cardano.npEkgParams = npEkgParams
    , Cardano.npStatsdParams = Nothing
    , Cardano.npNetworkConfig = npNetworkConfig
    , Cardano.npBehaviorConfig = npBehaviorConfig
    }

data NetworkConfigOpts = NetworkConfigOpts
    { ncoTopology :: !(Presence Topology)
    , ncoPort  :: !Word16
    } deriving Show

instance Eq NetworkConfigOpts where
    nco == nco' = ncoPort nco ==  ncoPort nco'

toCardanoNetworkConfigOpts :: NetworkConfigOpts -> Cardano.NetworkConfigOpts
toCardanoNetworkConfigOpts NetworkConfigOpts{..} = Cardano.NetworkConfigOpts
    { Cardano.ncoTopology = ncoTopology ^? _File
    , Cardano.ncoKademlia = Nothing
    , Cardano.ncoSelf = Nothing
    , Cardano.ncoPort = ncoPort
    , Cardano.ncoPolicies = Nothing
    , Cardano.ncoBindAddress = Nothing
    , Cardano.ncoExternalAddress = Nothing
    }

data ConfigurationOptions = ConfigurationOptions
    { cfo :: !(Presence Configuration) }
    deriving (Eq, Show)

instance Default ConfigurationOptions where
    def = ConfigurationOptions $ There defConfiguration

toCardanoConfigurationOptions :: ConfigurationOptions -> Cardano.ConfigurationOptions
toCardanoConfigurationOptions ConfigurationOptions{..} = Cardano.ConfigurationOptions
    { Cardano.cfoFilePath = maybe "" id $ cfo ^? _File
    , Cardano.cfoKey = "mainnet_full"
    , Cardano.cfoSystemStart = Nothing
    , Cardano.cfoSeed = Nothing
    }

data CommonArgs = CommonArgs
    { logConfig            :: !(Presence LoggerConfig)
    , logPrefix            :: !(Maybe FilePath)
    , configurationOptions :: !ConfigurationOptions
    }

-- Checked all fields except logConfig
instance Eq CommonArgs where
    ca == ca' = logPrefix ca == logPrefix ca'
        && configurationOptions ca == configurationOptions ca'

instance Show CommonArgs where
    show CommonArgs{..} = "CommonArgs { " <> show logPrefix <>
        ", " <> show configurationOptions <> " }"

toCardanoCommonArgs :: CommonArgs -> Cardano.CommonArgs
toCardanoCommonArgs CommonArgs{..} = Cardano.CommonArgs
    { Cardano.logConfig = logConfig ^? _File
    , Cardano.logPrefix = Nothing
    , Cardano.reportServers = []
    , Cardano.updateServers = []
    , Cardano.configurationOptions = toCardanoConfigurationOptions configurationOptions
    }

newtype CardanoConfig = CardanoConfig
    { getCardanoConfig :: CommonNodeArgs }
    deriving (Eq, Show)

instance D.Interpret CardanoConfig where
    autoWith _ = CardanoConfig <$> interpretCommonNodeArgs

instance D.Inject CardanoConfig where
    injectWith _ = contramap getCardanoConfig injectCommonNodeArgs

defTopology :: Topology
defTopology = either (error "LoggerConfig retrieving failed during parsing") id . decodeEither' $
    $(embedFile $ (unsafePerformIO $ (\fp -> flip take fp $ (length fp) - 8 ) <$> getCurrentDirectory)
        <> "/config/cardano/topology.yaml")

defLoggerConfig :: LoggerConfig
defLoggerConfig = either (error "LoggerConfig retrieving failed during parsing") id . decodeEither' $
    $(embedFile $ (unsafePerformIO $ (\fp -> flip take fp $ (length fp) - 8 ) <$> getCurrentDirectory)
        <> "/config/cardano/log-config.yaml")

defConfiguration :: Configuration
defConfiguration = either (error "Configuration retrieving failed during parsing") id . decodeEither' $
    $(embedFile $ (unsafePerformIO $ (\fp -> flip take fp $ (length fp) - 8 ) <$> getCurrentDirectory)
        <> "/config/cardano/cardano-config.yaml")

defaultCardanoConfig :: CardanoConfig
defaultCardanoConfig = CardanoConfig
    CommonNodeArgs
        { dbPath = Just "db-mainnet"
        , rebuildDB = False
        , devGenesisSecretI = Nothing
        , keyfilePath = "secret-mainnet.key"
        , networkConfigOpts = NetworkConfigOpts
            { ncoTopology = There defTopology
            , ncoPort = 3000
            }
        , commonArgs = CommonArgs
            { logConfig = There defLoggerConfig
            , logPrefix = Just "logs/mainnet"
            , configurationOptions = ConfigurationOptions
                { cfo = There defConfiguration }
            }
        , enableMetrics = False
        , ekgParams = Nothing
        }

loggingParams :: LoggerName -> CommonNodeArgs -> LoggingParams
loggingParams defaultName CommonNodeArgs{..} = LoggingParams
    { lpHandlerPrefix = logPrefix commonArgs
    , lpConfigPath    = (logConfig commonArgs) ^? _File
    , lpDefaultName   = defaultName
    , lpConsoleLog    = Nothing -- no override by default
    }

gtSscParams :: CommonNodeArgs -> VssKeyPair -> BehaviorConfig -> SscParams
gtSscParams CommonNodeArgs {..} vssSK BehaviorConfig{..} =
    SscParams
    { spSscEnabled = True
    , spVssKeyPair = vssSK
    , spBehavior   = bcSscBehavior
    }

-- | This function prepares 'UserSecret' for later usage by node. It
-- ensures that primary key and VSS key are present in
-- 'UserSecret'. They are either taken from generated secrets or
-- generated by this function using secure source of randomness.
prepareUserSecret :: forall m . (HasConfiguration, MonadIO m, WithLogger m)
    => CommonNodeArgs -> UserSecret -> m (SecretKey, UserSecret)
prepareUserSecret CommonNodeArgs {devGenesisSecretI} userSecret = do
    (_, userSecretWithVss) <-
        fillUserSecretVSS (rsVssKeyPair <$> predefinedRichKeys) userSecret
    fillPrimaryKey (rsPrimaryKey <$> predefinedRichKeys) userSecretWithVss
  where
    onUnknownGeneratedSecrets = error $
        "devGenesisSecretI is specified, but no generatedSecrets is present.\n" <>
        "Try to change initializer in genesis spec"

    predefinedRichKeys :: Maybe RichSecrets
    predefinedRichKeys
        | Just secretsRich <- genesisSecretsRich
        , Just i <- devGenesisSecretI = secretsRich ^? ix i
        | Nothing <- devGenesisSecretI = Nothing
        | otherwise = onUnknownGeneratedSecrets

-- Make sure UserSecret contains a primary key.
fillPrimaryKey :: (MonadIO m, WithLogger m)
    => Maybe SecretKey -> UserSecret -> m (SecretKey, UserSecret)
fillPrimaryKey = fillUserSecretPart (snd <$> keyGen) usPrimKey "signing key"

-- Make sure UserSecret contains a VSS key.
fillUserSecretVSS :: (MonadIO m, WithLogger m)
  => Maybe VssKeyPair -> UserSecret -> m (VssKeyPair, UserSecret)
fillUserSecretVSS = fillUserSecretPart vssKeyGen usVss "VSS keypair"

-- Make sure UserSecret contains something.
fillUserSecretPart :: (MonadIO m, WithLogger m) => (forall n. MonadRandom n => n a)
    -> (Lens' UserSecret (Maybe a)) -> Text -> Maybe a -> UserSecret -> m (a, UserSecret)
fillUserSecretPart genValue l description desiredValue userSecret = do
    toSet <- getValueToSet
    let newUS = userSecret & l .~ Just toSet
    (toSet, newUS) <$ writeUserSecret newUS
  where
    getValueToSet
        | Just desired <- desiredValue = pure desired
        | Just existing <- userSecret ^. l = pure existing
        | otherwise = do
            logInfo $
                "Found no " <> description <>
                " in keyfile, generating random one..."
            liftIO (runSecureRandom genValue)

getBaseParams :: LoggerName -> CommonNodeArgs -> BaseParams
getBaseParams defaultLoggerName args@CommonNodeArgs {..} =
    BaseParams { bpLoggingParams = loggingParams defaultLoggerName args }

getKeyfilePath :: CommonNodeArgs -> FilePath
getKeyfilePath CommonNodeArgs {..} = case devGenesisSecretI of
    Nothing -> keyfilePath
    Just i  -> "node-" ++ show i ++ "." ++ keyfilePath

getNodeParams :: (MonadIO m, WithLogger m, MonadCatch m, HasConfiguration)
    => LoggerName -> CommonNodeArgs -> NodeArgs -> m NodeParams
getNodeParams defaultLoggerName cArgs@CommonNodeArgs{..} NodeArgs{..} = do
    (primarySK, userSecret) <-
        prepareUserSecret cArgs =<< peekUserSecret (getKeyfilePath cArgs)
    npNetworkConfig <- intNetworkConfigOpts $ toCardanoNetworkConfigOpts networkConfigOpts
    npBehaviorConfig <- case behaviorConfigPath of
        Nothing -> pure def
        Just fp -> eitherToThrow =<< liftIO (decodeFileEither fp)
    pure NodeParams
        { npDbPathM = dbPath
        , npRebuildDb = rebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npBaseParams = getBaseParams defaultLoggerName cArgs
        , npEnableMetrics = enableMetrics
        , npEkgParams = ekgParams
        , ..
        }

parseFieldCardano ::
       Map.InsOrdHashMap D.Text (Expr Src X) -> D.Text -> D.Type a -> Maybe a
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
    f "reportServers" = "report-servers"
    f "updateServers" = "update-servers"
    f "configurationOptions" = "configuration-options"

    f "ekgHost" = "host"
    f "ekgPort" = "port"

    f "dbPath" = "db-path"
    f "rebuildDB" = "rebuild-db"
    f "devGenesisSecretI" = "genesis-secret"
    f "keyfilePath" = "keyfile"
    f "networkConfigOpts" = "network-config"
    f "commonArgs" = "common-args"
    f "enableMetrics" = "metrics"
    f "ekgParams" = "ekg-params"
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

interpretNetworkConfigOpts :: D.Type NetworkConfigOpts
interpretNetworkConfigOpts = D.Type extractOut expectedOut
  where
    extractOut (RecordLit fields) = do
      ncoTopology <- (<$>) File $ parseFieldCardano fields "ncoTopology" (D.maybe interpretFilePath)
      ncoPort <- defalultIfNothing 3000 (parseFieldCardano fields "ncoPort" (D.maybe interpretWord16))
      return NetworkConfigOpts {..}
    extractOut _ = Nothing

    expectedOut =
      Record
          (Map.fromList
              [ (cardanoFieldModifier "ncoTopology", D.expected (D.maybe interpretFilePath))
              , (cardanoFieldModifier "ncoPort", D.expected (D.maybe interpretWord16))
              ]
          )

interpretConfigurationOptions :: D.Type ConfigurationOptions
interpretConfigurationOptions = D.Type extractOut expectedOut
  where
    extractOut (RecordLit fields) = do
      cfo <- defalultIfNothing (cfo def) ((parseFieldCardano fields "cfo") (Just . File <$> D.maybe interpretFilePath))
      return ConfigurationOptions{..}
    extractOut _ = Nothing

    expectedOut = Record (Map.fromList [(cardanoFieldModifier "cfo", D.expected $ D.maybe interpretFilePath)])

interpretCommonArgs :: D.Type CommonArgs
interpretCommonArgs = D.Type extractOut expectedOut
  where
    extractOut (RecordLit fields) = do
      logConfig <- (<$>) File . parseFieldCardano fields "logConfig" $ D.maybe interpretFilePath
      logPrefix <- parseFieldCardano fields "logPrefix" (D.maybe interpretFilePath)
      configurationOptions <- parseFieldCardano fields "configurationOptions" interpretConfigurationOptions
      return CommonArgs {..}
    extractOut _ = Nothing

    expectedOut =
      Record
          (Map.fromList
              [ (cardanoFieldModifier "logConfig", D.expected (D.maybe interpretFilePath))
              , (cardanoFieldModifier "logPrefix", D.expected (D.maybe interpretFilePath))
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
      commonArgs <- parseFieldCardano fields "commonArgs" interpretCommonArgs
      enableMetrics <- parseFieldCardano fields "enableMetrics" D.auto
      ekgParams <- parseFieldCardano fields "ekgParams" (D.maybe interpretEkgParams)
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
              , (cardanoFieldModifier "commonArgs", D.expected interpretCommonArgs)
              , (cardanoFieldModifier "enableMetrics", D.expected (D.auto :: D.Type Bool))
              , (cardanoFieldModifier "ekgParams", D.expected (D.maybe interpretEkgParams))
              ])

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

injectNetworkConfigOpts :: D.InputType NetworkConfigOpts
injectNetworkConfigOpts = D.InputType {..}
  where
      embed NetworkConfigOpts {..} = RecordLit
        (Map.fromList
          [ (cardanoFieldModifier "ncoTopology", D.embed (injectMaybe injectFilePath) $ ncoTopology ^? _File)
          , (cardanoFieldModifier "ncoPort", D.embed (injectMaybe injectWord16) $ Just ncoPort)
          ])

      declared = Record
        (Map.fromList
          [ (cardanoFieldModifier "ncoTopology", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "ncoPort", D.declared (injectMaybe injectWord16))
          ])

injectConfigurationOptions :: D.InputType ConfigurationOptions
injectConfigurationOptions = D.InputType {..}
  where
      embed ConfigurationOptions {..} = RecordLit $ Map.fromList
        [ (cardanoFieldModifier "cfo", D.embed (injectMaybe injectFilePath) $ cfo ^? _File) ]

      declared = Record $ Map.fromList
        [ (cardanoFieldModifier "cfo", D.declared (injectMaybe injectFilePath)) ]

injectCommonArgs :: D.InputType CommonArgs
injectCommonArgs = D.InputType {..}
  where
      embed CommonArgs {..} = RecordLit
        (Map.fromList
          [ (cardanoFieldModifier "logConfig", D.embed (injectMaybe injectFilePath) $ logConfig ^? _File)
          , (cardanoFieldModifier "logPrefix", D.embed (injectMaybe injectFilePath) logPrefix)
          , (cardanoFieldModifier "configurationOptions", D.embed injectConfigurationOptions configurationOptions)
          ])

      declared = Record
        (Map.fromList
          [ (cardanoFieldModifier "logConfig", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "logPrefix", D.declared (injectMaybe injectFilePath))
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
                , (cardanoFieldModifier "commonArgs", D.embed injectCommonArgs commonArgs)
                , (cardanoFieldModifier "enableMetrics", D.embed D.inject enableMetrics)
                , (cardanoFieldModifier "ekgParams", D.embed (injectMaybe injectEkgParams) ekgParams)
                ])

      declared = Record
        (Map.fromList
          [ (cardanoFieldModifier "dbPath", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "rebuildDB", D.declared (D.inject :: D.InputType Bool))
          , (cardanoFieldModifier "devGenesisSecretI", D.declared (injectMaybe injectInt))
          , (cardanoFieldModifier "keyfilePath", D.declared (injectMaybe injectFilePath))
          , (cardanoFieldModifier "networkConfigOpts", D.declared injectNetworkConfigOpts)
          , (cardanoFieldModifier "commonArgs", D.declared injectCommonArgs)
          , (cardanoFieldModifier "enableMetrics", D.declared (D.inject :: D.InputType Bool))
          , (cardanoFieldModifier "ekgParams", D.declared (injectMaybe injectEkgParams))
          ])
