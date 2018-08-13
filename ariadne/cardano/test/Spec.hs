{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Universum

import Control.Lens (makeLensesWith)
import Control.Spoon (teaspoon)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Yaml as Yaml
import IiExtras (postfixLFields)
import qualified Options.Applicative as Opt
import Pos.Client.CLI.NodeOptions (CommonNodeArgs(..))
import Pos.Infra.Network.Types (NodeName(..))
import Pos.Infra.Statistics (EkgParams(..))
import Pos.Launcher (Configuration, ConfigurationOptions(..))
import Serokell.Data.Memory.Units (fromBytes)
import Test.Ariadne.Cardano.Arbitrary ()
import Test.Ariadne.Knit (knitSpec)
import Test.Hspec
  (Expectation, Spec, describe, expectationFailure, hspec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Text.JSON.Canonical as Canonical

import Ariadne.Cardano.Orphans ()
import Ariadne.Config.Ariadne (AriadneConfig(..), defaultAriadneConfig)
import Ariadne.Config.Cardano
  (CardanoConfig(..), defaultConfigurationYaml, defaultGenesisJson,
  defaultLoggerConfig, defaultTopology)
import Ariadne.Config.CLI (mergeConfigs, opts)
import Ariadne.Config.DhallUtil (fromDhall, toDhall)
import Ariadne.Config.Wallet (WalletConfig(..))

makeLensesWith postfixLFields ''CommonNodeArgs
makeLensesWith postfixLFields ''AriadneConfig
makeLensesWith postfixLFields ''CardanoConfig

main :: IO ()
main = hspec $ do
    configSpec
    knitSpec

configSpec :: Spec
configSpec = describe "Ariadne.Config" $ do
  it "CLI can override a certain set of fields" overrideConfigUnitTest
  it "Embedded values are correct" embeddedValuesUnitTest
  prop "handles any CardanoConfig" propHandleCardanoConfig

propHandleCardanoConfig :: CardanoConfig -> Property
propHandleCardanoConfig conf = monadicIO $ do
  parsed <- run $ (fromDhall . toDhall) conf
  assert (conf == parsed)

overrideConfigUnitTest :: Expectation
overrideConfigUnitTest =
    case parserResult of
        Opt.Success (_, _, cliConfig) ->
            (cliConfig `mergeConfigs` defaultAriadneConfig) `shouldBe`
            expectedAriadneConfig
        Opt.Failure failure ->
            let (failureMsg, _) = Opt.renderFailure failure "test"
            in expectationFailure ("Parser failed: " <> failureMsg)
        _ -> error $ "Unexpected parser result: " <> show parserResult
  where
    opts' = opts "doesNotMatter"
    parserResult = Opt.execParserPure Opt.defaultPrefs opts' cliArgs

type MultiConfiguration = Map Text Configuration

embeddedValuesUnitTest :: Expectation
embeddedValuesUnitTest = do
    whenNothing_ (teaspoon defaultLoggerConfig) $
        expectationFailure "defaultLoggerConfig is broken"
    whenNothing_ (teaspoon defaultTopology) $
        expectationFailure "defaultLoggerConfig is broken"
    whenLeft (Yaml.decodeEither @MultiConfiguration defaultConfigurationYaml) $
        expectationFailure . mappend "defaultConfigurationYaml is broken: "
    whenLeft (Canonical.parseCanonicalJSON (BSL.fromStrict defaultGenesisJson)) $
        expectationFailure . mappend "defaultGenesisJson is broken: "


cliArgs :: [String]
cliArgs =
  [ "--config", "doesNotMatter"
  , "--cardano:db-path", "new-db-path"
  , "--cardano:rebuild-db", "True"
  , "--cardano:keyfile", "new-keyfile"
  , "--cardano:topology", "new-topology"
  , "--cardano:node-id", "new-node-id"
  , "--cardano:default-port", "4444"
  , "--cardano:log-config", "new-log-config"
  , "--cardano:log-prefix", "new-log-prefix"
  , "--cardano:configuration-file", "new-configuration-file"
  , "--cardano:configuration-key", "new-configuration-key"
  , "--cardano:system-start", "89"
  , "--cardano:configuration-seed", "9"
  , "--cardano:metrics", "True"
  , "--cardano:ekg-params", "255.255.255.252:8888"
  , "--wallet:entropy-size", "32"
  ]


expectedAriadneConfig :: AriadneConfig
expectedAriadneConfig = defaultAriadneConfig
  { acCardano = defaultCardanoConfig
      { ccDbPath = Just "new-db-path"
      , ccRebuildDB = True
      , ccKeyfilePath = "new-keyfile"
      , ccNetworkTopology = Just "new-topology"
      , ccNetworkNodeId = Just (NodeName "new-node-id")
      , ccNetworkPort = 4444
      , ccLogConfig = Just "new-log-config"
      , ccLogPrefix = Just "new-log-prefix"
      , ccConfigurationOptions = ConfigurationOptions
          { cfoFilePath = "new-configuration-file"
          , cfoKey = "new-configuration-key"
          , cfoSystemStart = Just 89000000
          , cfoSeed = Just 9
          }
      , ccEnableMetrics = True
      , ccEkgParams = Just
        (EkgParams {ekgHost = "255.255.255.252", ekgPort = 8888})
      }
  , acWallet = defaultWalletConfig
    { wcEntropySize = fromBytes 32
    }
  }
  where
    defaultCardanoConfig = acCardano defaultAriadneConfig
    defaultWalletConfig = acWallet defaultAriadneConfig
