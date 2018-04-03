import Control.Applicative (optional)
import Control.Applicative as A
import Data.Function (on)
import Data.List (maximumBy)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe
import Data.Text (Text)
import Data.Traversable (for)
import Data.Void
import Test.Hspec (Expectation, Spec, describe, hspec, it, shouldBe)
import Text.Megaparsec
  (Parsec, getParserState, getPosition, lookAhead, runParser, try,
  updateParserState)
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

longestMatch :: [Parser Bool] -> Parser Bool
longestMatch ps = do
  ps' <-
    for ps $ \p ->
        optional . try . lookAhead $ do
            datum <- p
            position <- getPosition
            pState <- getParserState
            return (position, (pState, datum))
  case nonEmpty (catMaybes ps') of
    Nothing -> A.empty
    Just ps'' -> do
        let tup = snd $ maximumBy (compare `on` fst) ps''
        applyParser tup
      where
        applyParser (pState, datum) = do
            updateParserState (const pState)
            return datum
