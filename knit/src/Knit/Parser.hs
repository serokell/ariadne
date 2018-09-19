{-# LANGUAGE AllowAmbiguousTypes #-}

module Knit.Parser where

import Control.Applicative as A
import Control.Lens
import Data.Foldable as F
import Data.List as List
import Data.Loc
import Data.Monoid (First)
import Data.Proxy
import Data.Text
import Text.Earley

import Knit.Prelude
import Knit.Syntax
import Knit.Tokenizer

data ParseTreeExt

type instance XExprProcCall ParseTreeExt = NoExt
type instance XExprLit ParseTreeExt = Span

type instance XProcCall ParseTreeExt = Maybe Span

type instance XArgPos ParseTreeExt = NoExt
type instance XArgKw ParseTreeExt = Span

tok :: Getting (First a) (Token components) a -> Prod r e (Span, Token components) (Span, a)
tok p = _tok p --terminal (preview $ _2 . p)

inBrackets
    :: Getting (First ()) (Token components) BracketSide
    -> Prod r e (Span, Token components) a
    -> Prod r e (Span, Token components) a
inBrackets p r =
    tok (p . _BracketSideOpening) *> r <* tok (p . _BracketSideClosing)

class ComponentLitGrammar components component where
  componentLitGrammar :: Grammar r (Prod r Text (Span, Token components) (Span, Lit components))

gComponentsLit
  :: forall components r.
     (AllConstrained (ComponentLitGrammar components) components, KnownSpine components)
  => Grammar r (Prod r Text (Span, Token components) (Span, Lit components))
gComponentsLit = go (knownSpine @components)
  where
    go
      :: forall components'.
         (AllConstrained (ComponentLitGrammar components) components')
      => Spine components'
      -> Grammar r (Prod r Text (Span, Token components) (Span, Lit components))
    go (Base ()) = rule A.empty
    go (Step (Proxy :: Proxy component, xs)) = do
      nt1 <- go xs
      nt2 <- componentLitGrammar @_ @component
      rule $ nt1 <|> nt2

gExpr
  :: forall components r.
     (AllConstrained (ComponentLitGrammar components) components, KnownSpine components)
  => Grammar r (Prod r Text (Span, Token components) (Expr ParseTreeExt CommandId components))
gExpr = mdo
    ntName <- rule $ tok _TokenName
    ntKey <- rule $ tok _TokenKey
    ntComponentsLit <- gComponentsLit @components
    ntExprLit <- rule $ uncurry ExprLit <$> ntComponentsLit <?> "literal"
    ntArg <- rule $ asum
        [ uncurry ArgKw <$> ntKey <*> ntExprAtom
        , ArgPos NoExt <$> ntExprAtom
        ] <?> "argument"
    ntExpr1 <- rule $ asum
        [ ExprProcCall NoExt <$> ntProcCall
        , ntExprAtom
        , pure (ExprProcCall NoExt $ ProcCall Nothing (CommandIdOperator OpUnit) [])
        ] <?> "expression"
    ntExpr <- rule $ mkExprGroup ntExpr1 (fst <$> tok _TokenSemicolon)
    ntProcCall <- rule $
      uncurry ProcCall
        <$> (bimap Just CommandIdName <$> ntName)
        <*> some ntArg
        <?> "procedure call"
    ntProcCall0 <- rule $
      (\(s, name) -> ExprProcCall NoExt $ ProcCall (Just s) (CommandIdName name) [])
        <$> ntName
        <?> "procedure call w/o arguments"
    ntExprAtom <- rule $ asum
        [ ntExprLit
        , ntProcCall0
        , inBrackets _TokenParenthesis ntExpr <?> "parenthesized expression"
        ] <?> "atom"
    return ntExpr

mkExprGroup
    :: Alternative f
    => f (Expr ParseTreeExt CommandId components)
    -> f Span
    -> f (Expr ParseTreeExt CommandId components)
mkExprGroup expr sep = expr <|> liftA3 opAndThen expr sep (mkExprGroup expr sep)
  where
    opAndThen e1 sep' e2 = ExprProcCall NoExt $
      ProcCall (Just sep') (CommandIdOperator OpAndThen) [ArgPos NoExt e1, ArgPos NoExt e2]

pExpr
  :: (AllConstrained (ComponentLitGrammar components) components, KnownSpine components)
  => Parser Text [(Span, Token components)] (Expr ParseTreeExt CommandId components)
pExpr = parser gExpr

data ParseError components = ParseError
    { peSource :: Text
    , peReport :: Report Text [(Span, Token components)]
    }

parseTree
  :: ( KnownSpine components
     , AllConstrained (ComponentTokenizer components) components
     , AllConstrained (ComponentLitGrammar components) components
     )
  => Text
  -> Either (ParseError components) (Expr ParseTreeExt CommandId components)
parseTree str = over _Left (ParseError str) . toEither . fullParses pExpr . tokenize $ str
  where
    toEither = \case
      ([] , r) -> Left r
      (a:_, _) -> Right a

parse
  :: ( KnownSpine components
     , AllConstrained (ComponentTokenizer components) components
     , AllConstrained (ComponentLitGrammar components) components
     )
  => Text
  -> Either (ParseError components) (Expr NoExt CommandId components)
parse = second dropExt . parseTree

parseErrorSpans :: ParseError components -> [Span]
parseErrorSpans = List.map fst . unconsumed . peReport
