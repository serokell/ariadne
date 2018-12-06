module Knit.ParseTreeExt
       ( ParseTreeExt
       , ExprInBrackets(..)
       , dropParseTreeExt
       ) where

import Knit.Prelude
import Knit.Syntax
import Knit.Tokenizer

-- | Stores information about parsed tokens (spans and tokens themselves).
data ParseTreeExt

type instance XExprProcCall ParseTreeExt _ _ = NoExt
type instance XExprLit ParseTreeExt _ components = TokenWithSpace components
type instance XXExpr ParseTreeExt cmd components =
    ExprInBrackets (TokenWithSpace components) (Expr ParseTreeExt cmd components)

type instance XProcCall ParseTreeExt _ (Expr ParseTreeExt _ components) =
    (Maybe (TokenWithSpace components), Completeness)

type instance XArgPos ParseTreeExt _ = NoExt
type instance XArgKw ParseTreeExt (Expr ParseTreeExt _ components) =
    (TokenWithSpace components, Completeness)
type instance XXArg ParseTreeExt _ = Void

data ExprInBrackets br a = ExprInBrackets br a br
    deriving (Show, Eq, Ord)

dropParseTreeExt :: Expr ParseTreeExt cmd components -> Expr NoExt cmd components
dropParseTreeExt (XExpr (ExprInBrackets _ e _)) = dropParseTreeExt e
dropParseTreeExt (ExprLit _ l) = ExprLit NoExt l
dropParseTreeExt (ExprProcCall _ pc) = ExprProcCall NoExt (dropExtPc pc)
  where
    dropExtPc (ProcCall _ cmd args) = ProcCall NoExt cmd (map dropExtArg args)
    dropExtArg (ArgPos _ a) = ArgPos NoExt (dropParseTreeExt a)
    dropExtArg (ArgKw _ name a) = ArgKw NoExt name (dropParseTreeExt a)
    dropExtArg (XArg xxArg) = absurd xxArg
