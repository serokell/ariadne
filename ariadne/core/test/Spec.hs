import Test.Hspec (hspec)

import qualified Test.Ariadne.Logging as Logging

main :: IO ()
main = hspec $ do
  Logging.spec
