{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Lens (makeLensesWith)
import Control.Spoon (teaspoon)
import qualified Data.ByteString.Lazy as BSL
import Data.Scientific
  (Scientific(..), base10Exponent, normalize, scientific, toBoundedInteger)
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as Opt
import Serokell.Data.Memory.Units (fromBytes)
import Test.Hspec
  (Expectation, Spec, describe, expectationFailure, hspec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, arbitrary, forAll, suchThat)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Text.JSON.Canonical as Canonical

import NType (Elem)
import Pos.Client.CLI.NodeOptions (CommonNodeArgs(..))
import Pos.Core (Coin(..), mkCoin)
import Pos.Infra.Network.Types (NodeName(..))
import Pos.Infra.Statistics (EkgParams(..))
import Pos.Launcher (Configuration, ConfigurationOptions(..))

import Ariadne.Cardano.Knit (Cardano, adaMultiplier, adaToCoin, maxCoin, tyCoin)
import Ariadne.Cardano.Orphans ()
import Ariadne.Config.Ariadne (AriadneConfig(..), defaultAriadneConfig)
import Ariadne.Config.Cardano
  (CardanoConfig(..), defaultConfigurationYaml, defaultGenesisJson,
  defaultLoggerConfig, defaultTopology)
import Ariadne.Config.CLI (mergeConfigs, opts)
import Ariadne.Config.DhallUtil (fromDhall, toDhall)
import Ariadne.Config.History (HistoryConfig(..))
import Ariadne.Config.Logging (LoggingConfig(..))
import Ariadne.Config.Update (UpdateConfig(..))
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Util (postfixLFields)
import Knit (ComponentValue(..), Core, TyProjection(..), toValue)

import Test.Ariadne.Bip44 (bip44KeyPairGen, bip44PathGen)
import Test.Ariadne.Cardano.Arbitrary ()
import Test.Ariadne.History.Arbitrary ()
import Test.Ariadne.Knit (knitSpec)
import Test.Ariadne.Update.Arbitrary ()
import Test.Ariadne.Wallet.Arbitrary ()

makeLensesWith postfixLFields ''CommonNodeArgs
makeLensesWith postfixLFields ''AriadneConfig
makeLensesWith postfixLFields ''CardanoConfig

defaultAriadneCfg :: AriadneConfig
defaultAriadneCfg = defaultAriadneConfig "/data/"

main :: IO ()
main = hspec $ do
    configSpec
    knitSpec
    bip44PathGen
    bip44KeyPairGen
    cardanoKnitSpec

configSpec :: Spec
configSpec = describe "Ariadne.Config" $ do
  it "CLI can override a certain set of fields" overrideConfigUnitTest
  it "Embedded values are correct" embeddedValuesUnitTest
  prop "handles any CardanoConfig" propHandleCardanoConfig
  prop "handles any WalletConfig" propHandleWalletConfig
  prop "handles any UpdateConfig" propHandleUpdateConfig
  prop "handles any HistoryConfig" propHandleHistoryConfig
  prop "handles any LoggingConfig" propHandleHistoryConfig

propHandleCardanoConfig :: CardanoConfig -> Property
propHandleCardanoConfig conf = monadicIO $ do
  parsed <- run $ (fromDhall . toDhall) conf
  assert (conf == parsed)

propHandleWalletConfig :: WalletConfig -> Property
propHandleWalletConfig conf = monadicIO $ do
  parsed <- run $ (fromDhall . toDhall) conf
  assert (conf == parsed)

propHandleUpdateConfig :: UpdateConfig -> Property
propHandleUpdateConfig conf = monadicIO $ do
  parsed <- run $ (fromDhall . toDhall) conf
  assert (conf == parsed)

propHandleHistoryConfig :: HistoryConfig -> Property
propHandleHistoryConfig conf = monadicIO $ do
  parsed <- run $ (fromDhall . toDhall) conf
  assert (conf == parsed)

propHandleLoggingConfig :: LoggingConfig -> Property
propHandleLoggingConfig conf = monadicIO $ do
  parsed <- run $ (fromDhall . toDhall) conf
  assert (conf == parsed)

overrideConfigUnitTest :: Expectation
overrideConfigUnitTest =
    case parserResult of
        Opt.Success (_, _, cliConfig) ->
            (cliConfig `mergeConfigs` defaultAriadneCfg) `shouldBe`
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
  , "--cardano:topology", "new-topology"
  , "--cardano:node-id", "new-node-id"
  , "--cardano:default-port", "4444"
  , "--cardano:log-config", "new-log-config"
  , "--cardano:log-prefix", "new-log-prefix"
  , "--cardano:configuration-file", "new-configuration-file"
  , "--cardano:configuration-key", "new-configuration-key"
  , "--cardano:system-start", "89"
  , "--cardano:configuration-seed", "9"
  , "--cardano:ekg-params", "255.255.255.252:8888"
  , "--wallet:entropy-size", "32"
  , "--wallet:keyfile", "new-secret-mainnet.key"
  , "--wallet:wallet-db-path", "new-wallet-db"
  , "--update:version-check-url", "new-version-check-url"
  , "--update:update-url", "new-update-url"
  , "--update:check-delay", "21600"
  , "--history:path", "new-history-path"
  , "--logging:path", "new-logging-path"
  ]


expectedAriadneConfig :: AriadneConfig
expectedAriadneConfig = defaultAriadneCfg
  { acCardano = defaultCardanoConfig
      { ccDbPath = Just "new-db-path"
      , ccRebuildDB = True
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
      , ccEkgParams = Just
        (EkgParams {ekgHost = "255.255.255.252", ekgPort = 8888})
      }
  , acWallet = defaultWalletConfig
    { wcEntropySize = fromBytes 32
    , wcKeyfilePath = "new-secret-mainnet.key"
    , wcAcidDBPath  = "new-wallet-db"
    }
  , acUpdate = defaultUpdateConfig
      { ucVersionCheckUrl = "new-version-check-url"
      , ucUpdateUrl       = "new-update-url"
      , ucCheckDelay      = 21600
      }
  , acHistory = defaultHistoryConfig
      { hcPath = "new-history-path"
      }
  , acLogging = defaultLoggingConfig
      { lcPath = "new-logging-path"
      }
  }
  where
    defaultCardanoConfig = acCardano defaultAriadneCfg
    defaultWalletConfig = acWallet defaultAriadneCfg
    defaultUpdateConfig = acUpdate defaultAriadneCfg
    defaultHistoryConfig = acHistory defaultAriadneCfg
    defaultLoggingConfig = acLogging defaultAriadneCfg

cardanoKnitSpec :: Spec
cardanoKnitSpec = do
  specAdaToCoin
  specTyCoin

type Components = '[Core, Cardano]

getMaybeCoin :: (Elem Components Cardano, Elem Components Core) => Scientific -> Maybe Coin
getMaybeCoin x = tpMatcher tyCoin $ toValue @Components @Core $ ValueNumber x

specTyCoin :: Spec
specTyCoin = describe "Check tyCoin" $ do
  prop "Accept correct numbers" propTyCoinAccept
  prop "Refuse incorrect numbers" propTyCoinRefuse

coinAcceptanceCondition :: Scientific -> Bool
coinAcceptanceCondition x =
  x * adaMultiplier <= maxCoin && x > 0 && (base10Exponent $ normalize x) >= -6

coinRefusionCondition :: Scientific -> Bool
coinRefusionCondition x =
  (base10Exponent $ normalize x) < -6 || x * adaMultiplier > maxCoin || x < 0

propTyCoinAccept :: Property
propTyCoinAccept =
  forAll (arbitrary `suchThat` coinAcceptanceCondition) $ \x ->
    getMaybeCoin x ==
      (mkCoin <$> (toBoundedInteger @Word64) (x * adaMultiplier))

propTyCoinRefuse :: Property
propTyCoinRefuse =
  forAll (arbitrary `suchThat` coinRefusionCondition) $ isNothing . getMaybeCoin

specAdaToCoin :: Spec
specAdaToCoin = describe "Check adaToCoin" $ do
    prop "Accept correct numbers" propAdaToCoinAccept
    prop "Refuse incorrect numbers" propAdaToCoinRefuse
    it "Unit Test" adaToCoinUnitTest

propAdaToCoinAccept :: Property
propAdaToCoinAccept =
    forAll (arbitrary `suchThat` coinAcceptanceCondition) $ \x ->
      adaToCoin x == (Just $ x * adaMultiplier)

propAdaToCoinRefuse :: Property
propAdaToCoinRefuse =
    forAll (arbitrary `suchThat` coinRefusionCondition) $ isNothing . adaToCoin

adaToCoinUnitTest :: Expectation
adaToCoinUnitTest = do
  adaToCoin x `shouldBe` (Just $ (normalize x) * adaMultiplier)
  adaToCoin y `shouldBe` Nothing
  where
    x :: Scientific
    x = scientific 1 7
    y :: Scientific
    y = scientific 12345678 -7
