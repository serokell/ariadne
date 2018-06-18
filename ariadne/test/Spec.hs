import Universum

import Ariadne.Cardano.Orphans ()
import Ariadne.Config.Ariadne (AriadneConfig(..), defaultAriadneConfig)
import Ariadne.Config.Cardano (CardanoConfig(..))
import Ariadne.Config.CLI (mergeConfigs, opts)
import Ariadne.Config.DhallUtil (fromDhall, toDhall)
import Ariadne.Config.Update (UpdateConfig(..))
import Ariadne.Config.Wallet (WalletConfig(..))
import Control.Lens (makeLensesWith)
import IiExtras (postfixLFields)
import qualified Options.Applicative as Opt
import Pos.Client.CLI.NodeOptions (CommonNodeArgs(..))
import Pos.Client.CLI.Options (CommonArgs(..))
import Pos.Launcher
import Pos.Network.CLI (NetworkConfigOpts(..))
import Pos.Network.Types (NodeName(..))
import Pos.Statistics (EkgParams(..), StatsdParams(..))
import Serokell.Data.Memory.Units (fromBytes)
import Test.Ariadne.Cardano.Arbitrary ()
import Test.Hspec (Expectation, Spec, describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

makeLensesWith postfixLFields ''CommonNodeArgs
makeLensesWith postfixLFields ''AriadneConfig
makeLensesWith postfixLFields ''CardanoConfig

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Ariadne.Config" $ do
  it "CLI can override any field" overrideConfigUnitTest
  prop "handles any CardanoConfig" propHandleCardanoConfig

propHandleCardanoConfig :: CardanoConfig -> Property
propHandleCardanoConfig conf = monadicIO $ do
  parsed <- run $ (fromDhall . toDhall) conf
  assert (conf == parsed)

overrideConfigUnitTest :: Expectation
overrideConfigUnitTest = actual `shouldBe` Just expectedAriadneConfig
  where
    opts' = opts "doesNotMatter"
    actual =
        (`mergeConfigs` ariadneConfigSample) . view _3 <$>
        Opt.getParseResult (Opt.execParserPure Opt.defaultPrefs opts' cliArgs)

cliArgs :: [String]
cliArgs =
  [ "--config", "doesNotMatter"
  , "--cardano:db-path", "new-db-path"
  , "--cardano:rebuild-db", "True"
  , "--cardano:genesis-secret", "111"
  , "--cardano:keyfile", "new-keyfile"
  , "--cardano:topology", "new-topology"
  , "--cardano:kademlia", "new-kademlia"
  , "--cardano:node-id", "new-node-id"
  , "--cardano:default-port", "4444"
  , "--cardano:policies", "new-policies"
  , "--cardano:address", "255.255.255.255:8888"
  , "--cardano:listen", "255.255.255.254:8888"
  , "--cardano:json-log", "new-json-log"
  , "--cardano:log-config", "new-log-config"
  , "--cardano:log-prefix", "new-log-prefix"
  , "--cardano:report-servers", "[\"new-report-server-1\", \"new-report-server-2\"]"
  , "--cardano:update-servers", "[\"new-update-server-1\", \"new-update-server-2\"]"
  , "--cardano:configuration-file", "new-configuration-file"
  , "--cardano:configuration-key", "new-configuration-key"
  , "--cardano:system-start", "89"
  , "--cardano:configuration-seed", "9"
  , "--cardano:update-latest-path", "new-update-latest-path"
  , "--cardano:update-with-package", "True"
  , "--cardano:route53-health-check", "255.255.255.253:8888"
  , "--cardano:metrics", "True"
  , "--cardano:ekg-params", "255.255.255.252:8888"
  , "--cardano:statsd-server", "255.255.255.251:8888"
  , "--cardano:statsd-interval", "1000"
  , "--cardano:statsd-debug", "True"
  , "--cardano:statsd-prefix", "new-statsd-prefix"
  , "--cardano:statsd-suffix", "new-statsd-suffix"
  , "--cardano:dump-genesis-data-to", "new-dump-genesis-data-to"
  , "--cardano:dump-configuration", "True"
  , "--wallet:entropy-size", "32"
  ]


expectedAriadneConfig :: AriadneConfig
expectedAriadneConfig = AriadneConfig
  { acCardano = CardanoConfig
    { getCardanoConfig = CommonNodeArgs
        { dbPath = Just "new-db-path"
        , rebuildDB = True
        , devGenesisSecretI = Just 111
        , keyfilePath = "new-keyfile"
        , networkConfigOpts = NetworkConfigOpts
            { ncoTopology = Just "new-topology"
            , ncoKademlia = Just "new-kademlia"
            , ncoSelf = Just (NodeName "new-node-id")
            , ncoPort = 4444
            , ncoPolicies = Just "new-policies"
            , ncoBindAddress = Just ("255.255.255.254",8888)
            , ncoExternalAddress = Just ("255.255.255.255",8888)
            }
        , jlPath = Just "new-json-log"
        , commonArgs = CommonArgs
          { logConfig = Just "new-log-config"
          , logPrefix = Just "new-log-prefix"
          , reportServers = ["new-report-server-1", "new-report-server-2"]
          , updateServers = ["new-update-server-1", "new-update-server-2"]
          , configurationOptions = ConfigurationOptions
            {cfoFilePath = "new-configuration-file"
            , cfoKey = "new-configuration-key"
            , cfoSystemStart = Just 89000000, cfoSeed = Just 9
            }
          }
        , updateLatestPath = "new-update-latest-path"
        , updateWithPackage = True
        , route53Params = Just ("255.255.255.253",8888)
        , enableMetrics = True
        , ekgParams = Just (EkgParams {ekgHost = "255.255.255.252", ekgPort = 8888})
        , statsdParams = Just StatsdParams
          {statsdHost = "255.255.255.251"
          , statsdPort = 8888
          , statsdInterval = 1000
          , statsdDebug = True
          , statsdPrefix = "new-statsd-prefix"
          , statsdSuffix = "new-statsd-suffix"
          }
        , cnaDumpGenesisDataPath = Just "new-dump-genesis-data-to"
        , cnaDumpConfiguration = True
        }
    }
  , acWallet = WalletConfig {wcEntropySize = fromBytes 32}
  , acUpdate = UpdateConfig
      { ucVersionCheckUrl = "https://ariadnewallet.io"
      , ucCheckDelay = 3600
      }
  }

ariadneConfigSample :: AriadneConfig
ariadneConfigSample = defaultAriadneConfig & (acCardanoL . getCardanoConfigL . statsdParamsL) .~ statsdSample
  where
    statsdSample = Just StatsdParams
      { statsdHost     = "host"
      , statsdPort     = 2020
      , statsdInterval = 1010
      , statsdDebug    = False
      , statsdPrefix   = "prefix"
      , statsdSuffix   = "suffix"
      }
