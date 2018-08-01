{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Universum

import Control.Lens (makeLensesWith)
import IiExtras (postfixLFields)
import qualified Options.Applicative as Opt
import Pos.Client.CLI.NodeOptions (CommonNodeArgs(..))
import Pos.Client.CLI.Options (CommonArgs(..))
import Pos.Infra.Network.CLI (NetworkConfigOpts(..))
import Pos.Infra.Network.Types (NodeName(..))
import Pos.Infra.Statistics (EkgParams(..))
import Pos.Launcher (ConfigurationOptions(..))
import Serokell.Data.Memory.Units (fromBytes)
import Test.Ariadne.Cardano.Arbitrary ()
import Test.Ariadne.Knit (knitSpec)
import Test.Hspec (Expectation, Spec, describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Ariadne.Cardano.Orphans ()
import Ariadne.Config.Ariadne (AriadneConfig(..), defaultAriadneConfig)
import Ariadne.Config.Cardano (CardanoConfig(..))
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
    { getCardanoConfig = defaultCommonNodeArgs
        { dbPath = Just "new-db-path"
        , rebuildDB = True
        , keyfilePath = "new-keyfile"
        , networkConfigOpts = defaultNetworkConfigOpts
            { ncoTopology = Just "new-topology"
            , ncoSelf = Just (NodeName "new-node-id")
            , ncoPort = 4444
            }
        , commonArgs = defaultCommonArgs
          { logConfig = Just "new-log-config"
          , logPrefix = Just "new-log-prefix"
          , configurationOptions = ConfigurationOptions
            { cfoFilePath = "new-configuration-file"
            , cfoKey = "new-configuration-key"
            , cfoSystemStart = Just 89000000
            , cfoSeed = Just 9
            }
          }
        , enableMetrics = True
        , ekgParams = Just
          (EkgParams {ekgHost = "255.255.255.252", ekgPort = 8888})
        }
    }
  , acWallet = defaultWalletConfig
    { wcEntropySize = fromBytes 32
    }
  }
  where
    defaultCardanoConfig = acCardano defaultAriadneConfig
    defaultCommonNodeArgs = getCardanoConfig defaultCardanoConfig
    defaultCommonArgs = commonArgs defaultCommonNodeArgs
    defaultNetworkConfigOpts = networkConfigOpts defaultCommonNodeArgs

    defaultWalletConfig = acWallet defaultAriadneConfig
