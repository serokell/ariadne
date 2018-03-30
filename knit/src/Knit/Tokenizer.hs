{-# LANGUAGE AllowAmbiguousTypes #-}

module Knit.Tokenizer where

import Control.Applicative as A
import Control.Lens
import Control.Monad
import Data.Char
import Data.Function
import Data.Functor
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.Loc
import Data.Maybe
import Data.Proxy
import Data.Text as T
import Data.Traversable
import Data.Union
import Data.Void
import IiExtras

import Control.Applicative.Combinators.NonEmpty as NonEmpty
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char

import Knit.Name

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
  componentTokenizer :: [Tokenizer (Token components)]

class ComponentDetokenizer component where
  componentTokenRender :: ComponentToken component -> Text

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

tokenRender
  :: forall components.
     AllConstrained ComponentDetokenizer components
  => Token components
  -> Text
tokenRender = \case
  Token u -> ufold @ComponentDetokenizer componentTokenRender u
  TokenSquareBracket bs -> withBracketSide "[" "]" bs
  TokenParenthesis bs -> withBracketSide "(" ")" bs
  TokenEquals -> "="
  TokenSemicolon -> ";"
  TokenName name -> T.pack (show name)
  TokenKey name -> T.pack (shows name ":")
  TokenUnknown (UnknownChar c) -> T.singleton c

detokenize
  :: AllConstrained ComponentDetokenizer components
  => [Token components]
  -> Text
detokenize = T.unwords . List.map tokenRender

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
pToken = withSpan (try pToken' <|> pUnknown) <* pSkip
  where
    posToLoc :: SourcePos -> Loc
    posToLoc SourcePos{..} = uncurry loc
        ( fromIntegral . unPos $ sourceLine
        , fromIntegral . unPos $ sourceColumn )
    withSpan p = do
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
    , pToken''
    , pIdentifier
    ] <?> "token"

pToken''
  :: forall components.
     (KnownSpine components, AllConstrained (ComponentTokenizer components) components)
  => Tokenizer (Token components)
pToken'' = longestMatch (go (knownSpine @components))
  where
    go
      :: forall components'.
         AllConstrained (ComponentTokenizer components) components'
      => Spine components'
      -> [Tokenizer (Token components)]
    go RNil = []
    go ((Proxy :: Proxy component) :& xs) =
      componentTokenizer @_ @component ++ go xs

longestMatch :: [Tokenizer (Token components)] -> Tokenizer (Token components)
longestMatch ps = do
  ps' <-
    for ps $ \p -> do
      optional . try . lookAhead $ do
        -- we discard the value and parse it again later;
        -- this is probably bad for performance, but I haven't
        -- measured it
        _ <- p
        position <- getPosition
        return (position, p)
  case nonEmpty (catMaybes ps') of
    Nothing -> A.empty
    Just ps'' -> snd $ maximumBy (compare `on` fst) ps''

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

pSomeAlphaNum :: Tokenizer Text
pSomeAlphaNum = takeWhile1P (Just "alphanumeric") isAlphaNum
