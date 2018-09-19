{-# LANGUAGE AllowAmbiguousTypes #-}

module Knit.Parser where

import Control.Applicative as A
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Control.Lens
import Data.Foldable as F
import Data.List as List
import Data.List.NonEmpty
import Data.Loc
import Data.Monoid (First)
import Data.Proxy
import Data.Text
import Text.Earley

import Knit.Prelude
import Knit.Syntax
import Knit.Tokenizer

tok :: Getting (First a) (Token components) a -> Prod r e (s, Token components) a
tok p = terminal (preview $ _2 . p)

inBrackets
    :: Getting (First ()) (Token components) BracketSide
    -> Prod r e (s, Token components) a
    -> Prod r e (s, Token components) a
inBrackets p r =
    tok (p . _BracketSideOpening) *> r <* tok (p . _BracketSideClosing)

class ComponentLitGrammar components component where
  componentLitGrammar :: Grammar r (Prod r Text (s, Token components) (Lit components))

gComponentsLit
  :: forall components r s.
     (AllConstrained (ComponentLitGrammar components) components, KnownSpine components)
  => Grammar r (Prod r Text (s, Token components) (Lit components))
gComponentsLit = go (knownSpine @components)
  where
    go
      :: forall components'.
         (AllConstrained (ComponentLitGrammar components) components')
      => Spine components'
      -> Grammar r (Prod r Text (s, Token components) (Lit components))
    go (Base ()) = rule A.empty
    go (Step (Proxy :: Proxy component, xs)) = do
      nt1 <- go xs
      nt2 <- componentLitGrammar @_ @component
      rule $ nt1 <|> nt2

gExpr
  :: forall components r s.
     (AllConstrained (ComponentLitGrammar components) components, KnownSpine components)
  => Grammar r (Prod r Text (s, Token components) (Expr NoExt CommandId components))
gExpr = mdo
    ntName <- rule $ tok _TokenName
    ntKey <- rule $ tok _TokenKey
    ntComponentsLit <- gComponentsLit @components
    ntExprLit <- rule $ ExprLit NoExt <$> ntComponentsLit <?> "literal"
    ntArg <- rule $ asum
        [ ArgKw NoExt <$> ntKey <*> ntExprAtom
        , ArgPos NoExt <$> ntExprAtom
        ] <?> "argument"
    ntExpr1 <- rule $ asum
        [ ExprProcCall NoExt <$> ntProcCall
        , ntExprAtom
        , pure (ExprProcCall NoExt $ ProcCall NoExt (CommandIdOperator OpUnit) [])
        ] <?> "expression"
    ntExpr <- rule $ mkExprGroup <$> ntExpr1 `sepBy1` tok _TokenSemicolon
    ntProcCall <- rule $
      ProcCall NoExt
        <$> (CommandIdName <$> ntName)
        <*> some ntArg
        <?> "procedure call"
    ntProcCall0 <- rule $
        (\name -> ExprProcCall NoExt $ ProcCall NoExt (CommandIdName name) []) <$> ntName
        <?> "procedure call w/o arguments"
    ntExprAtom <- rule $ asum
        [ ntExprLit
        , ntProcCall0
        , inBrackets _TokenParenthesis ntExpr <?> "parenthesized expression"
        ] <?> "atom"
    return ntExpr

mkExprGroup :: NonEmpty (Expr NoExt CommandId components) -> Expr NoExt CommandId components
mkExprGroup = F.foldr1 opAndThen
  where
    opAndThen e1 e = ExprProcCall NoExt $
      ProcCall NoExt (CommandIdOperator OpAndThen) [ArgPos NoExt e1, ArgPos NoExt e]

pExpr
  :: (AllConstrained (ComponentLitGrammar components) components, KnownSpine components)
  => Parser Text [(s, Token components)] (Expr NoExt CommandId components)
pExpr = parser gExpr

data ParseError components = ParseError
    { peSource :: Text
    , peReport :: Report Text [(Span, Token components)]
    }

parse
  :: ( KnownSpine components
     , AllConstrained (ComponentTokenizer components) components
     , AllConstrained (ComponentLitGrammar components) components
     )
  => Text
  -> Either (ParseError components) (Expr NoExt CommandId components)
parse str = over _Left (ParseError str) . toEither . fullParses pExpr . tokenize $ str
  where
    toEither = \case
      ([] , r) -> Left r
      (a:_, _) -> Right a

parseErrorSpans :: ParseError components -> [Span]
parseErrorSpans = List.map fst . unconsumed . peReport
