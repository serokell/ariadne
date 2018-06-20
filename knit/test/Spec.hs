{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Universum

import qualified Ariadne.Cardano.Knit as Knit
import Ariadne.TaskManager.Face (TaskId(..))
import qualified Ariadne.TaskManager.Knit as Knit
import Data.List.NonEmpty (fromList)
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import IiExtras
import Knit
import Test.Hspec (Expectation, Spec, describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  (Arbitrary(..), Gen, Property, elements, listOf, property)
import Test.QuickCheck.Gen (listOf1, oneof)
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Char as P

main :: IO ()
main = do
  hspec specLongestMatch
  hspec specTokenizer

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

-- Tokenizer tests

specTokenizer :: Spec
specTokenizer = describe "Knit.Tokenizer" $ do
    prop "accepts any input" propAcceptsAnyInput
    prop "handles valid input" propHandlesValidInput

-- The only components that have tokens
type Components = '[Knit.Core, Knit.Cardano, Knit.TaskManager]

propAcceptsAnyInput :: Property
propAcceptsAnyInput = property $ isJust . (tokenize' @Components) . fromString

propHandlesValidInput :: Property
propHandlesValidInput = property $ liftA2 (==) (map snd . tokenize . (detokenize @Components)) identity

instance Arbitrary (Token Components) where
  arbitrary = genTokenComponents @Components

class ComponentTokenGen components component where
  componentTokenGen :: [Gen (Knit.Token components)]

instance Elem components Knit.Core => ComponentTokenGen components Knit.Core where
  componentTokenGen =
    [ toToken . TokenNumber <$> arbitrary @Scientific
    , toToken . TokenString <$> arbitrary @String
    , toToken . TokenFilePath <$> genValidFilePath
    ]

instance Elem components Knit.Cardano => ComponentTokenGen components Knit.Cardano where
  componentTokenGen =
    [ toToken . Knit.TokenAddress <$> arbitrary
    , toToken . Knit.TokenPublicKey <$> arbitrary
    , toToken . Knit.TokenHash <$> arbitrary
    ]

instance Elem components Knit.TaskManager => ComponentTokenGen components Knit.TaskManager where
  componentTokenGen =
    [ toToken . Knit.TokenTaskId . TaskId <$> arbitrary]

genName :: Gen Name
genName = (Name . fromList) <$> listOf1 letters
  where
    letters :: Gen (NonEmpty Letter)
    letters = fromList <$> listOf1 (unsafeMkLetter <$> elements alphasList)

genValidFilePath :: Gen FilePath
genValidFilePath = do
  prefix <- genPrefix
  path <- listOf (elements symbList)
  return $ prefix <> path
  where
    genPrefix :: Gen FilePath
    genPrefix = elements ["./", "/", "../"]

    symbList :: FilePath
    symbList =
        alphasList
        <> ['0'..'9']
        <> ['.', '/', '-', '_']

alphasList :: [Char]
alphasList = ['A'..'Z'] <> ['a'..'z']

tokenUnknownList :: forall components.
  (KnownSpine components, AllConstrained (ComponentTokenizer components) components)
  => [Knit.Token components]
tokenUnknownList = filter unknown $ map snd
  (foldMap ((tokenize @components) . T.singleton) ([minBound..maxBound] :: [Char]))
  where
    unknown :: Token components -> Bool
    unknown (TokenUnknown _) = True
    unknown _ = False

genTokenComponents
  :: forall components.
  ( KnownSpine components
  , AllConstrained (ComponentTokenGen components) components
  , AllConstrained (ComponentTokenizer components) components)
  => Gen (Knit.Token components)
genTokenComponents = oneof $ componentTokens <> baseTokens
  where
    baseTokens :: [Gen (Token components)]
    baseTokens =
      [ TokenSquareBracket <$> elements [BracketSideOpening, BracketSideClosing]
      , TokenParenthesis <$> elements [BracketSideOpening, BracketSideClosing]
      , return TokenEquals
      , return TokenSemicolon
      , TokenName <$> genName
      , TokenKey <$> genName
      , elements (tokenUnknownList @components)
      ]

    componentTokens :: [Gen (Token components)]
    componentTokens = go (knownSpine @components)

    go
      :: forall components'.
         AllConstrained (ComponentTokenGen components) components'
      => Spine components'
      -> [Gen (Knit.Token components)]
    go RNil = []
    go ((Proxy :: Proxy component) :& xs) =
      (componentTokenGen @components @component) ++ (go xs)
