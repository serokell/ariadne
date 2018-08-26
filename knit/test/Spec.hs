import Universum

import Test.Hspec (Expectation, Spec, describe, hspec, it, shouldBe)
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Char as P

import Knit.Tokenizer (longestMatch)

main :: IO ()
main = do
  hspec specLongestMatch

-- longestMatch tests

specLongestMatch :: Spec
specLongestMatch = describe "longestMatch" $ do
    it "parses a" unitTest1
    it "parses aa" unitTest2

unitTest1 :: Expectation
unitTest1 =
  runParser (longestMatch parserList) "" "a" `shouldBe` (Right True)

unitTest2 :: Expectation
unitTest2 =
  runParser (longestMatch parserList) "" "aa" `shouldBe` (Right False)

parserList :: [Parser Bool]
parserList = [True <$ P.string "a", False <$ P.string "aa"]

type Parser = Parsec Void Text
