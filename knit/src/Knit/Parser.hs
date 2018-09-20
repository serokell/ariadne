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

type instance XExprProcCall ParseTreeExt = MaybeWithBrackets ()
type instance XExprLit ParseTreeExt = MaybeWithBrackets Span

type instance XProcCall ParseTreeExt = Maybe Span

type instance XArgPos ParseTreeExt = NoExt
type instance XArgKw ParseTreeExt = Span

data MaybeWithBrackets a = MaybeWithBrackets (Maybe (Span, Span)) a
    deriving (Eq, Ord, Show)

noBrackets :: a -> MaybeWithBrackets a
noBrackets = MaybeWithBrackets Nothing

addBrackets :: (Span, Span) -> MaybeWithBrackets a -> MaybeWithBrackets a
addBrackets brackets (MaybeWithBrackets _ a) = MaybeWithBrackets (Just brackets) a

tok :: Getting (First a) (Token components) a -> Prod r e (Span, Token components) (Span, a)
tok p = terminal (sequenceA . fmap (preview p))

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
    ntExprLit <- rule $ uncurry (ExprLit . noBrackets) <$> ntComponentsLit <?> "literal"
    ntArg <- rule $ asum
        [ uncurry ArgKw <$> ntKey <*> ntExprAtom
        , ArgPos NoExt <$> ntExprAtom
        ] <?> "argument"
    ntExpr1 <- rule $ asum
        [ ExprProcCall (noBrackets ()) <$> ntProcCall
        , ntExprAtom
        , pure (ExprProcCall (noBrackets ()) $ ProcCall Nothing (CommandIdOperator OpUnit) [])
        ] <?> "expression"
    ntExpr <- rule $ mkExprGroup ntExpr1 (fst <$> tok _TokenSemicolon)
    ntProcCall <- rule $
      uncurry ProcCall
        <$> (bimap Just CommandIdName <$> ntName)
        <*> some ntArg
        <?> "procedure call"
    ntProcCall0 <- rule $
      (\(s, name) -> ExprProcCall (noBrackets ()) $ ProcCall (Just s) (CommandIdName name) [])
        <$> ntName
        <?> "procedure call w/o arguments"
    ntInBrackets <- rule $
      addBracketsExpr
        <$> bracketSpan _BracketSideOpening
        <*> ntExpr
        <*> bracketSpan _BracketSideClosing
        <?> "parenthesized expression"
    ntExprAtom <- rule $ asum
        [ ntExprLit
        , ntProcCall0
        , ntInBrackets
        ] <?> "atom"
    return ntExpr
  where
    bracketSpan side = fst <$> tok (_TokenParenthesis . side)

    addBracketsExpr l (ExprProcCall ext pCall) r = ExprProcCall (addBrackets (l, r) ext) pCall
    addBracketsExpr l (ExprLit ext lit) r = ExprLit (addBrackets (l, r) ext) lit

mkExprGroup
  :: Alternative f
  => f (Expr ParseTreeExt CommandId components)
  -> f Span
  -> f (Expr ParseTreeExt CommandId components)
mkExprGroup expr sep =
    (Knit.Prelude.foldl' opAndThen)
    <$> expr
    <*> many ((,) <$> sep <*> expr)
  where
    opAndThen e1 (sep', e2) = ExprProcCall (noBrackets ()) $
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
