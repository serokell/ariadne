import Universum

import Ariadne.Cardano.Orphans ()
import Ariadne.Config.Cardano (CardanoConfig)
import Ariadne.Config.DhallUtil (fromDhall, toDhall)
import Test.Ariadne.Cardano.Arbitrary ()
import Test.Hspec (Spec, describe, hspec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Ariadne.Config.Instances" $ do
    prop "handles any CardanoConfig" propHandleCardanoConfig

propHandleCardanoConfig :: CardanoConfig -> Property
propHandleCardanoConfig conf = monadicIO $ do
  parsed <- run $ (fromDhall . toDhall) conf
  assert (conf == parsed)
