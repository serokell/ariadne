{-# LANGUAGE AllowAmbiguousTypes #-}

module Knit.Tokenizer where

import Control.Applicative as A
import Control.Lens
import Control.Monad
import Data.Char
import Data.Functor
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.Loc
import Data.Maybe
import Data.Proxy
import Data.Text as T
import Data.Union
import Data.Void
import Formatting (build, sformat, (%))
import IiExtras

import Control.Applicative.Combinators.NonEmpty as NonEmpty
import Text.Megaparsec hiding (Token, many)
import Text.Megaparsec.Char

import Knit.Name

-- | The side of a bracket.
--
-- Opening: @([{<@
-- Closing: @)]}>@
data BracketSide = BracketSideOpening | BracketSideClosing
    deriving (Eq, Ord, Show)

makePrisms ''BracketSide

-- | Eliminator (inline case analysis) for 'BracketSide'.
withBracketSide :: a -> a -> BracketSide -> a
withBracketSide onOpening onClosing = \case
    BracketSideOpening -> onOpening
    BracketSideClosing -> onClosing

-- | A tokenizer is a type of parser that returns a sequence of tokens rather
-- than a syntax tree. We use monadic parser combinators from the 'megaparsec'
-- library to implement our tokenizer.
--
-- The first two parameters to 'Parsec' are the type of custom parse errors and
-- the type of the input stream. We don't report parse errors from the
-- tokenizer, so we set the first parameter to 'Void', meaning that custom parse
-- errors cannot be constructed or reported (although there are still stock
-- parse errors, which we utilize for backtracking). As to the input stream
-- type, we pick 'Text' because it's supposedly more efficient than 'String'.
type Tokenizer = Parsec Void Text

-- | The 'ComponentToken' data family defines the token type of a particular
-- component. For example:
--
-- @
-- data instance ComponentToken Core
--  = TokenNumber Scientific
--  | TokenString String
--  | TokenFilePath FilePath
-- @
--
-- If a component does not need to extend lexical primitives of the language,
-- the data instance can be an empty data type:
--
-- @
-- data instance ComponentToken Core
-- @
data family ComponentToken component

-- | The 'ComponentTokenizer' class defines the tokenization logic of a
-- particular component. The first parameter, 'components', provides the ability
-- to reference the overall list of components in the application, and data
-- instances should not match on it. The second parameter, 'component', is the
-- component for which we are defining the data instance.
--
-- The 'componentTokenizer' method defines a list of tokenizers per token type
-- of the component. For example:
--
-- @
-- instance Elem components Core => ComponentTokenizer components Core where
--   componentTokenizer =
--     [ toToken . TokenNumber <$> pScientific,
--       toToken . TokenString <$> pString,
--       toToken . TokenFilePath <$> pFilePath  ]
-- @
--
class ComponentTokenizer components component where
  componentTokenizer :: [Tokenizer (Token components)]

-- | The 'ComponentDetokenizer' class defines the detokenization logic of a
-- particular component. Detokenization is inverse of tokenization, so for any
-- token @t@, @tokenize (detokenize t) = t@ must hold.
--
class ComponentDetokenizer component where
  componentTokenRender :: ComponentToken component -> Text

-- | 'Token' represents a group of characters in the input stream that will be
-- treated as a terminal in the grammar definition of the language (during the
-- next stage, parsing).
--
-- By default, we have tokens for square/round brackets, for various
-- punctuation, for identifiers, etc. Components can add their own token types,
-- so one can think of 'Token' as an extensible data type. For instance, let's
-- say we have the following data instances:
--
-- @
-- data instance ComponentToken components A
--   = TokenX X
--   | TokenY Y
--
-- data instance ComponentToken components B
--   = TokenM M
--   | TokenN N
-- @
--
-- Then @Token '[A, B]@ is roughly equivalent to:
--
-- @
-- data TokenAB
--   = TokenX X
--   | TokenY Y
--   | TokenM M
--   | TokenN N
--   | TokenSquareBracket BracketSide
--   | TokenParenthesis BracketSide
--   ...
--   | TokenUnknown Char
-- @
--
data Token components
  = Token (Union ComponentToken components)
  | TokenSquareBracket BracketSide
  | TokenParenthesis BracketSide
  | TokenEquals
  | TokenSemicolon
  | TokenName Name
  | TokenKey Name
  | TokenUnknown Char

deriving instance Eq (Union ComponentToken components) => Eq (Token components)
deriving instance Ord (Union ComponentToken components) => Ord (Token components)
deriving instance Show (Union ComponentToken components) => Show (Token components)

makePrisms ''Token

-- | Convert a token of some particular component to a 'Token'.
--
-- @
--            TokenNumber 4  :: ComponentToken Core@
--   toToken (TokenNumber 4) :: Elem components Core => Token components
-- @
toToken
  :: forall components component.
     Elem components component
  => ComponentToken component
  -> Token components
toToken = Token . uliftElem

-- | Match on a 'Token', expecting it to belong to a particular component.
-- Returns 'Nothing' if the token belongs to a different component.
--
-- prop> fromToken \@cs \@c . toToken \@cs \@c == Just
fromToken
  :: forall components component.
     Elem components component
  => Token components
  -> Maybe (ComponentToken component)
fromToken = umatchElem <=< preview _Token

-- | Render (detokenize) an individual token.
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
  TokenName name -> sformat build name
  TokenKey name -> sformat (build%":") name
  TokenUnknown c -> T.singleton c

-- | Detokenize a sequence of tokens.
--
-- Ignoring the spans returned by tokenization, we have the following property:
--
-- prop> tokenize . detokenize . tokenize == tokenize
--
detokenize
  :: AllConstrained ComponentDetokenizer components
  => [Token components]
  -> Text
detokenize = T.unwords . List.map tokenRender

-- | Tokenize a string of characters into a sequence of tokens. Tokenization
-- cannot fail, as unrecognized characters are mapped to 'TokenUnknown'.
--
-- Beside each token, a source span is returned, which can be later used to
-- highlight this token in the original source.
--
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

-- | An internal function for tokenization that returns 'Nothing' on parse
-- failure. Of course, parse failure is not supposed to ever happen, so this is
-- only used in tests:
--
-- prop> isJust . tokenize' == const True
--
tokenize'
  :: (KnownSpine components, AllConstrained (ComponentTokenizer components) components)
  => Text
  -> Maybe [(Span, Token components)]
tokenize' =
  -- 'parseMaybe' runs a parser, returning the result as 'Just' in case of
  -- success and as 'Nothing' in case of failure. It expects the parser to
  -- consume the entire input up to 'eof' and fails otherwise.
  parseMaybe $
    -- We skip unimportant characters in the beginning of the inupt string
    -- and after each token, covering all space around tokens.
    --
    -- We alse annotate each token with its source span.
    pSkip *> many (withSpan pToken <* pSkip)

-- | Add a source span to the result of tokenization.
withSpan :: Tokenizer a -> Tokenizer (Span, a)
withSpan p = do
    position1 <- posToLoc <$> getPosition
    t <- p
    position2 <- posToLoc <$> getPosition
    return (spanFromTo position1 position2, t)
  where
    posToLoc :: SourcePos -> Loc
    posToLoc SourcePos{..} = uncurry loc
        ( fromIntegral . unPos $ sourceLine
        , fromIntegral . unPos $ sourceColumn )

-- | Skip a (possibly empty) sequence of unimportant characters. For now, this
-- only includes whitespace, but could be potentially extended to comments.
pSkip :: Tokenizer ()
pSkip = skipMany (void spaceChar)

-- | Parser for a token, including punctuation, component tokens, identifiers,
-- and the possibility of unrecognized characters.
pToken
  :: (KnownSpine components, AllConstrained (ComponentTokenizer components) components)
  => Tokenizer (Token components)
pToken = try (pPunctuation <|> pToken' <|> pIdentifier) <|> pUnknown

-- | Parse any single character, considering it to be unrecognized/unknown.
pUnknown :: Tokenizer (Token components)
pUnknown = TokenUnknown <$> anyChar

-- | Parser for component tokens. That is, tokens added by components, rather
-- than inherent to the Knit language framework.
--
-- To resolve conflicts between components and to make their order irrelevant,
-- we use the 'longestMatch' combinator rather than regular 'choice'. However,
-- ideally 'longestMatch' must be improved to account for the possibility of
-- several valid parses of the same length.
pToken'
  :: forall components.
     (KnownSpine components, AllConstrained (ComponentTokenizer components) components)
  => Tokenizer (Token components)
pToken' = longestMatch (go (knownSpine @components))
  where
    go
      :: forall components'.
         AllConstrained (ComponentTokenizer components) components'
      => Spine components'
      -> [Tokenizer (Token components)]
    go RNil = []
    go ((Proxy :: Proxy component) :& xs) =
      componentTokenizer @_ @component ++ go xs

-- | Parser for punctuation tokens.
pPunctuation :: Tokenizer (Token components)
pPunctuation = choice
    [ char '[' $> TokenSquareBracket BracketSideOpening
    , char ']' $> TokenSquareBracket BracketSideClosing
    , char '(' $> TokenParenthesis BracketSideOpening
    , char ')' $> TokenParenthesis BracketSideClosing
    , char '=' $> TokenEquals
    , char ';' $> TokenSemicolon
    ] <?> "punct"

-- | Parser for identifiers and keys.
--
-- Identifiers are non-empty sequences of name sections, separated
-- by hyphens. Keys are identifiers followed by a colon.
--
-- Examples of identifiers: @send@, @tx-out@, @patak-bardaq-skovoroda@.
-- Examples of keys: @slot-duration:@, @max-tx-size:@, @addr:@.
--
pIdentifier :: Tokenizer (Token components)
pIdentifier = do
    name <- NonEmpty.sepBy1 pNameSection (char '-')
    notFollowedBy (satisfy isAlphaNum)
    isKey <- isJust <$> optional (char ':')
    return $ (if isKey then TokenKey else TokenName) (Name name)

-- | Parser for name sections - non-empty sequences of alphabetic characters.
pNameSection :: Tokenizer (NonEmpty Letter)
pNameSection = NonEmpty.some1 pLetter

-- | Parser for characters classified as alphabetic by Unicode.
pLetter :: Tokenizer Letter
pLetter = unsafeMkLetter <$> satisfy isAlpha

-- | Parser for non-empty sequences of alphanumeric characters.
pSomeAlphaNum :: Tokenizer Text
pSomeAlphaNum = takeWhile1P (Just "alphanumeric") isAlphaNum
