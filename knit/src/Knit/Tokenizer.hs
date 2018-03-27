{-# LANGUAGE AllowAmbiguousTypes #-}

module Knit.Tokenizer where

import Data.Text
import Control.Lens
import Data.Void
import Data.Loc
import Data.Union
import Control.Monad
import Control.Applicative as A
import Data.Functor
import Data.Maybe
import Data.List.NonEmpty as NonEmpty
import Data.Char
import Data.Vinyl.Core
import Data.Vinyl.TypeLevel
import Data.Proxy

import Control.Applicative.Combinators.NonEmpty as NonEmpty
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char

import Knit.Name
import Knit.Utils

data BracketSide = BracketSideOpening | BracketSideClosing
    deriving (Eq, Ord, Show)

makePrisms ''BracketSide

withBracketSide :: a -> a -> BracketSide -> a
withBracketSide onOpening onClosing = \case
    BracketSideOpening -> onOpening
    BracketSideClosing -> onClosing

type Tokenizer = Parsec Void Text

newtype UnknownChar = UnknownChar Char
    deriving (Eq, Ord, Show)

data family ComponentToken component

class ComponentTokenizer components component where
  componentTokenizer :: Tokenizer (Token components)

data Token components
  = Token (Union ComponentToken components)
  | TokenSquareBracket BracketSide
  | TokenParenthesis BracketSide
  | TokenEquals
  | TokenSemicolon
  | TokenName Name
  | TokenKey Name
  | TokenUnknown UnknownChar

deriving instance Eq (Union ComponentToken components) => Eq (Token components)
deriving instance Ord (Union ComponentToken components) => Ord (Token components)
deriving instance Show (Union ComponentToken components) => Show (Token components)

makePrisms ''Token

toToken
  :: forall components component.
     Elem components component
  => ComponentToken component
  -> Token components
toToken = Token . uliftElem

fromToken
  :: forall components component.
     Elem components component
  => Token components
  -> Maybe (ComponentToken component)
fromToken = umatchElem <=< preview _Token

tokenize
  :: (KnownSpine components, AllConstrained (ComponentTokenizer components) components)
  => Text
  -> [(Span, Token components)]
tokenize = fromMaybe noTokenErr . tokenize'
  where
    noTokenErr =
        -- This error cannot happen because we have a catch-all case
        -- in the definition of 'pToken' - 'pUnknown'. Anything that
        -- can't be tokenized will be treated as 'TokenUnknown'.
        error "tokenize: no token could be consumed. This is a bug"

tokenize'
  :: (KnownSpine components, AllConstrained (ComponentTokenizer components) components)
  => Text
  -> Maybe [(Span, Token components)]
tokenize' = parseMaybe (between pSkip eof (many pToken))

pToken
  :: (KnownSpine components, AllConstrained (ComponentTokenizer components) components)
  => Tokenizer (Span, Token components)
pToken = withPosition (try pToken' <|> pUnknown) <* pSkip
  where
    posToLoc :: SourcePos -> Loc
    posToLoc SourcePos{..} = uncurry loc
        ( fromIntegral . unPos $ sourceLine
        , fromIntegral . unPos $ sourceColumn )
    withPosition p = do
        position1 <- posToLoc <$> getPosition
        t <- p
        position2 <- posToLoc <$> getPosition
        return (spanFromTo position1 position2, t)

pUnknown :: Tokenizer (Token components)
pUnknown = TokenUnknown . UnknownChar <$> anyChar

pSkip :: Tokenizer ()
pSkip = skipMany (void spaceChar)

pToken'
  :: (KnownSpine components, AllConstrained (ComponentTokenizer components) components)
  => Tokenizer (Token components)
pToken' = choice
    [ pPunctuation
    , pIdentifier
    , pToken''
    ] <?> "token"

pToken''
  :: forall components.
     (KnownSpine components, AllConstrained (ComponentTokenizer components) components)
  => Tokenizer (Token components)
pToken'' = go (knownSpine @components)
  where
    go
      :: forall components'.
         AllConstrained (ComponentTokenizer components) components'
      => Spine components'
      -> Tokenizer (Token components)
    go RNil = A.empty
    go ((Proxy :: Proxy component) :& xs) =
      (componentTokenizer @_ @component) <|> go xs

pPunctuation :: Tokenizer (Token components)
pPunctuation = choice
    [ char '[' $> TokenSquareBracket BracketSideOpening
    , char ']' $> TokenSquareBracket BracketSideClosing
    , char '(' $> TokenParenthesis BracketSideOpening
    , char ')' $> TokenParenthesis BracketSideClosing
    , char '=' $> TokenEquals
    , char ';' $> TokenSemicolon
    ] <?> "punct"

pIdentifier :: Tokenizer (Token components)
pIdentifier = do
    name <- NonEmpty.sepBy1 pNameSection (char '-')
    notFollowedBy (satisfy isAlphaNum)
    isKey <- isJust <$> optional (char ':')
    return $ (if isKey then TokenKey else TokenName) (Name name)

pNameSection :: Tokenizer (NonEmpty Letter)
pNameSection = NonEmpty.some1 pLetter

pLetter :: Tokenizer Letter
pLetter = unsafeMkLetter <$> satisfy isAlpha
