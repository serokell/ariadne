module Knit.Parser where

import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Control.Lens
import Data.Loc
import Data.Monoid (First)
import Text.Earley
import Data.Text
import Data.List.NonEmpty
import Control.Applicative as A
import Data.Foldable

import Knit.Tokenizer
import Knit.Name
import Knit.Syntax

tok :: Getting (First a) (Token components) a -> Prod r e (s, Token components) a
tok p = terminal (preview $ _2 . p)

inBrackets
    :: Getting (First ()) (Token components) BracketSide
    -> Prod r e (s, Token components) a
    -> Prod r e (s, Token components) a
inBrackets p r =
    tok (p . _BracketSideOpening) *> r <* tok (p . _BracketSideClosing)

gExpr :: Grammar r (Prod r Text (s, Token components) (Expr Name components))
gExpr = mdo
    ntName <- rule $ tok _TokenName
    ntKey <- rule $ tok _TokenKey
    -- ntExprLit <- rule $ ExprLit <$> asum
    --     [ LitNumber <$> tok _TokenNumber
    --     , LitString <$> tok _TokenString
    --     , LitAddress <$> tok _TokenAddress
    --     , LitPublicKey <$> tok _TokenPublicKey
    --     , LitStakeholderId <$> tok _TokenStakeholderId
    --     , LitHash <$> tok _TokenHash
    --     , LitBlockVersion <$> tok _TokenBlockVersion
    --     , LitSoftwareVersion <$> tok _TokenSoftwareVersion
    --     , LitFilePath . getFilePath' <$> tok _TokenFilePath
    --     ] <?> "literal"
    ntExprLit <- rule A.empty -- TODO: FIXME
    ntArg <- rule $ asum
        [ ArgKw <$> ntKey <*> ntExprAtom
        , ArgPos <$> ntExprAtom
        ] <?> "argument"
    ntExpr1 <- rule $ asum
        [ ExprProcCall <$> ntProcCall
        , ntExprAtom
        -- , pure ExprUnit -- TODO: FIXME
        ] <?> "expression"
    ntExpr <- rule $ mkExprGroup <$> ntExpr1 `sepBy1` tok _TokenSemicolon
    ntProcCall <- rule $ ProcCall <$> ntName <*> some ntArg <?> "procedure call"
    ntProcCall0 <- rule $
        (\name -> ExprProcCall $ ProcCall name []) <$> ntName
        <?> "procedure call w/o arguments"
    ntExprAtom <- rule $ asum
        [ ntExprLit
        , ntProcCall0
        , inBrackets _TokenParenthesis ntExpr <?> "parenthesized expression"
        ] <?> "atom"
    return ntExpr

-- TODO: FIXME
mkExprGroup :: NonEmpty (Expr Name components) -> Expr Name components
mkExprGroup (a :| _) = a

pExpr :: Parser Text [(s, Token components)] (Expr Name components)
pExpr = parser gExpr

data ParseError components = ParseError
    { peSource :: Text
    , peReport :: Report Text [(Span, Token components)]
    }

parse :: Text -> Either (ParseError components) (Expr Name components)
parse str = over _Left (ParseError str) . toEither . fullParses pExpr . tokenize $ str
  where
    toEither = \case
      ([] , r) -> Left r
      (a:_, _) -> Right a
