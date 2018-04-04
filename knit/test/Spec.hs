import Data.Text (Text)
import Data.Void
import IiExtras (longestMatch)
import Test.Hspec (Expectation, Spec, describe, hspec, it, shouldBe)
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Char (string)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "longestMatch"$ do
    it "parses a" unitTest1
    it "parses aa" unitTest2

unitTest1 :: Expectation
unitTest1 =
  runParser (longestMatch parserList) "" "a" `shouldBe` (Right True)

unitTest2 :: Expectation
unitTest2 =
  runParser (longestMatch parserList) "" "aa" `shouldBe` (Right False)

parserList :: [Parser Bool]
parserList = [True <$ string "a", False <$ string "aa"]

type Parser = Parsec Void Text


