{-# LANGUAGE AllowAmbiguousTypes #-}

module Knit.Parser where

import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Control.Lens
import Data.Loc
import Data.Monoid (First)
import Text.Earley
import Data.Text
import Data.List as List
import Data.List.NonEmpty
import Data.Proxy
import Control.Applicative as A
import Data.Foldable as F
import IiExtras

import Knit.Tokenizer
import Knit.Syntax

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
    go RNil = rule A.empty
    go ((Proxy :: Proxy component) :& xs) = do
      nt1 <- go xs
      nt2 <- componentLitGrammar @_ @component
      rule $ nt1 <|> nt2

gExpr
  :: forall components r s.
     (AllConstrained (ComponentLitGrammar components) components, KnownSpine components)
  => Grammar r (Prod r Text (s, Token components) (Expr CommandName components))
gExpr = mdo
    ntName <- rule $ tok _TokenName
    ntKey <- rule $ tok _TokenKey
    ntComponentsLit <- gComponentsLit @components
    ntExprLit <- rule $ ExprLit <$> ntComponentsLit <?> "literal"
    ntArg <- rule $ asum
        [ ArgKw <$> ntKey <*> ntExprAtom
        , ArgPos <$> ntExprAtom
        ] <?> "argument"
    ntExpr1 <- rule $ asum
        [ ExprProcCall <$> ntProcCall
        , ntExprAtom
        , pure (ExprProcCall $ ProcCall (OperatorName OpUnit) [])
        ] <?> "expression"
    ntExpr <- rule $ mkExprGroup <$> ntExpr1 `sepBy1` tok _TokenSemicolon
    ntProcCall <- rule $
      ProcCall
        <$> (ProcedureName <$> ntName)
        <*> some ntArg
        <?> "procedure call"
    ntProcCall0 <- rule $
        (\name -> ExprProcCall $ ProcCall (ProcedureName name) []) <$> ntName
        <?> "procedure call w/o arguments"
    ntExprAtom <- rule $ asum
        [ ntExprLit
        , ntProcCall0
        , inBrackets _TokenParenthesis ntExpr <?> "parenthesized expression"
        ] <?> "atom"
    return ntExpr

mkExprGroup :: NonEmpty (Expr CommandName components) -> Expr CommandName components
mkExprGroup = F.foldr1 opSemicolon
  where
    opSemicolon e1 e = ExprProcCall $
      ProcCall (OperatorName OpSemicolon) [ArgPos e1, ArgPos e]

pExpr
  :: (AllConstrained (ComponentLitGrammar components) components, KnownSpine components)
  => Parser Text [(s, Token components)] (Expr CommandName components)
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
  -> Either (ParseError components) (Expr CommandName components)
parse str = over _Left (ParseError str) . toEither . fullParses pExpr . tokenize $ str
  where
    toEither = \case
      ([] , r) -> Left r
      (a:_, _) -> Right a

parseErrorSpans :: ParseError components -> [Span]
parseErrorSpans = List.map fst . unconsumed . peReport
