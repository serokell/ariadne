{-# LANGUAGE AllowAmbiguousTypes #-}

module Knit.Parser
       ( ComponentTokenToLit(..)
       , ParseError(..)
       , parseErrorSpans
       , gComponentsLit
       , gExpr
       , mkExprGroup
       , pExpr
       , parse
       , parseTree
       ) where

import Control.Applicative as A
import Control.Lens
import Data.Foldable as F
import Data.List as List
import Data.Loc
import Data.Monoid (First)
import Data.Proxy
import Data.Text (Text)
import Text.Earley

import Knit.ParseTreeExt
import Knit.Prelude
import Knit.Syntax
import Knit.Tokenizer

tok
  :: Getting (First a) (Token components) a
  -> Prod r e (TokenWithSpace components) (TokenWithSpace components, a)
tok p = terminal (\x -> (x,) <$> preview (twsToken.lItem.p) x)

class ComponentTokenToLit components component where
  componentTokenToLit :: Token components -> Maybe (Lit components)

gComponentsLit
  :: forall components r.
     (AllConstrained (ComponentTokenToLit components) components, KnownSpine components)
  => Grammar r (Prod r Text
        (TokenWithSpace components)
        (TokenWithSpace components, Lit components))
gComponentsLit = go (knownSpine @components)
  where
    go
      :: forall components'.
         (AllConstrained (ComponentTokenToLit components) components')
      => Spine components'
      -> Grammar r (Prod r Text
            (TokenWithSpace components)
            (TokenWithSpace components, Lit components))
    go (Base ()) = rule A.empty
    go (Step (Proxy :: Proxy component, xs)) = do
      nt1 <- go xs
      nt2 <- rule $ terminal $ \t -> do
        lit <- componentTokenToLit @_ @component $ t^.twsToken.lItem
        pure (t, lit)
      rule $ nt1 <|> nt2

gExpr
  :: forall components r.
     (AllConstrained (ComponentTokenToLit components) components, KnownSpine components)
  => Bool
  -> Grammar r (Prod r Text (TokenWithSpace components) (Expr ParseTreeExt CommandId components))
gExpr canBeIncomplete = mdo
    ntName <- rule $ terminal $ \x -> case x^.twsToken.lItem of
      TokenName (comp, name) | comp == Complete || canBeIncomplete -> Just ((x, comp), name)
      _ -> Nothing
    ntKey <- rule $ terminal $ \x -> case x^.twsToken.lItem of
      TokenKey (comp, name) | comp == Complete || canBeIncomplete -> Just ((x, comp), name)
      _ -> Nothing
    ntComponentsLit <- gComponentsLit @components
    ntExprLit <- rule $ uncurry ExprLit <$> ntComponentsLit <?> "literal"
    ntArg <- rule $ asum
        [ uncurry ArgKw <$> ntKey <*> ntExprAtom
        , ArgPos NoExt <$> ntExprAtom
        ] <?> "argument"
    ntExpr1 <- rule $ asum
        [ ExprProcCall NoExt <$> ntProcCall
        , ntExprAtom
        , pure (ExprProcCall NoExt $ ProcCall (Nothing, Complete) (CommandIdOperator OpUnit) [])
        ] <?> "expression"
    ntExpr <- rule $ mkExprGroup ntExpr1 (fst <$> tok _TokenSemicolon)
    ntProcCall <- rule $
      uncurry ProcCall
        <$> (bimap (first Just) CommandIdName <$> ntName)
        <*> some ntArg
        <?> "procedure call"
    ntProcCall0 <- rule $
      (\(s, name) -> ExprProcCall NoExt $ ProcCall (first Just s) (CommandIdName name) [])
        <$> ntName
        <?> "procedure call w/o arguments"
    ntInBrackets <- rule $ fmap XExpr $
      ExprInBrackets
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

mkExprGroup
  :: Alternative f
  => f (Expr ParseTreeExt CommandId components)
  -> f (TokenWithSpace components)
  -> f (Expr ParseTreeExt CommandId components)
mkExprGroup expr sep =
    (Knit.Prelude.foldl' opAndThen)
    <$> expr
    <*> many ((,) <$> sep <*> expr)
  where
    opAndThen e1 (sep', e2) = ExprProcCall NoExt $ ProcCall
      (Just sep', Complete)
      (CommandIdOperator OpAndThen)
      [ArgPos NoExt e1, ArgPos NoExt e2]

pExpr
  :: (AllConstrained (ComponentTokenToLit components) components, KnownSpine components)
  => Bool
  -> Parser Text [TokenWithSpace components] (Expr ParseTreeExt CommandId components)
pExpr canBeIncomplete = parser $ gExpr canBeIncomplete

data ParseError components = ParseError
    { peSource :: Text
    , peReport :: Report Text [TokenWithSpace components]
    }

parseTree
  :: ( KnownSpine components
     , AllConstrained (ComponentTokenizer components) components
     , AllConstrained (ComponentTokenToLit components) components
     )
  => Bool
  -> Text
  -> Either (ParseError components) (Expr ParseTreeExt CommandId components)
parseTree canBeIncomplete str =
    over _Left (ParseError str) $ toEither $ fullParses (pExpr canBeIncomplete) $ snd $ tokenize str
  where
    toEither = \case
      ([] , r) -> Left r
      (a:_, _) -> Right a

parse
  :: ( KnownSpine components
     , AllConstrained (ComponentTokenizer components) components
     , AllConstrained (ComponentTokenToLit components) components
     )
  => Text
  -> Either (ParseError components) (Expr NoExt CommandId components)
parse = second dropParseTreeExt . parseTree False

parseErrorSpans :: ParseError components -> [Span]
parseErrorSpans = List.map (^.twsToken.lSpan) . unconsumed . peReport
