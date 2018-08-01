{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Universum

import Ariadne.Cardano.Orphans ()
import Ariadne.Config.Ariadne (AriadneConfig(..), defaultAriadneConfig)
import Ariadne.Config.Cardano (CardanoConfig(..), CommonArgs(..)
    , CommonNodeArgs(..), NetworkConfigOpts(..), ConfigurationOptions(..)
    , defTopology, defConfiguration, defLoggerConfig)
import Ariadne.Config.CLI (mergeConfigs, opts)
import Ariadne.Config.DhallUtil (fromDhall, toDhall)
import Ariadne.Config.History (HistoryConfig(..))
import Ariadne.Config.Presence (Presence(..))
import Ariadne.Config.Update (UpdateConfig(..))
import Ariadne.Config.Wallet (WalletConfig(..))
import Control.Lens (makeLensesWith)
import IiExtras (postfixLFields)
import qualified Options.Applicative as Opt
import Pos.Infra.Statistics (EkgParams(..))
import Serokell.Data.Memory.Units (fromBytes)
import Test.Ariadne.Cardano.Arbitrary ()
import Test.Ariadne.Knit (knitSpec)
import Test.Hspec (Expectation, Spec, describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

makeLensesWith postfixLFields ''CommonNodeArgs
makeLensesWith postfixLFields ''AriadneConfig
makeLensesWith postfixLFields ''CardanoConfig

main :: IO ()
main = hspec $ do
    configSpec
    knitSpec

configSpec :: Spec
configSpec = describe "Ariadne.Config" $ do
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
        (`mergeConfigs` defaultAriadneConfig) . view _3 <$>
        Opt.getParseResult (Opt.execParserPure Opt.defaultPrefs opts' cliArgs)

cliArgs :: [String]
cliArgs =
  ["--config", "doesNotMatter"
  , "--cardano:db-path", "new-db-path"
  , "--cardano:rebuild-db", "True"
  , "--cardano:genesis-secret", "111"
  , "--cardano:keyfile", "new-keyfile"
  , "--cardano:topology", "new-topology"
  , "--cardano:node-id", "new-node-id"
  , "--cardano:default-port", "4444"
  , "--cardano:log-config", "new-log-config"
  , "--cardano:log-prefix", "new-log-prefix"
  , "--cardano:configuration-file", "new-configuration-file"
  , "--cardano:metrics", "True"
  , "--cardano:ekg-params", "255.255.255.252:8888"
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
            { ncoTopology = There defTopology
            , ncoPort = 4444
            }
        , commonArgs = CommonArgs
          { logConfig = There defLoggerConfig
          , logPrefix = Just "new-log-prefix"
          , configurationOptions = ConfigurationOptions
            { cfo = There defConfiguration }
          }
        , enableMetrics = True
        , ekgParams = Just (EkgParams {ekgHost = "255.255.255.252", ekgPort = 8888})
        }
    }
  , acWallet = WalletConfig {wcEntropySize = fromBytes 32}
  , acUpdate = UpdateConfig
      { ucVersionCheckUrl = "https://serokell.io/ariadne/version"
      , ucUpdateUrl = "https://serokell.io/ariadne/"
      , ucCheckDelay = 3600
      }
  , acHistory = HistoryConfig {hcPath = "ariadne_history.db"}
  }
