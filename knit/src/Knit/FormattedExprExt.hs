module Knit.FormattedExprExt
       ( FormattedExprExt
       , ArgPosSkipped(..)
       , ArgKwSkipped(..)

       , parseTreeToFormattedExpr
       , ppFormattedExpr
       ) where

import Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Knit.ParseTreeExt
import Knit.Prelude
import Knit.Printer
import Knit.Syntax
import Knit.Tokenizer

-- | Stores information for printing expressions in their original form.
data FormattedExprExt

type instance XExprProcCall FormattedExprExt _ _ = NoExt
type instance XExprLit FormattedExprExt _ _ = NoExt
-- | Space between the brackets and nested expression.
type instance XXExpr FormattedExprExt cmd components =
    ExprInBrackets Skipped (Expr FormattedExprExt cmd components)

-- | Information about the space between procedure and its arguments and between
-- the arguments is stored with the arguments. Their meaning depends on the
-- context. In the case of 'OpAndThen' space between the arguments and semicolon
-- is stored. In the case of 'CommandIdName' each argument stores the space
-- before itself.
type instance XProcCall FormattedExprExt _ _ = NoExt

type instance XArgPos FormattedExprExt _ = ArgPosSkipped
type instance XArgKw FormattedExprExt _ = ArgKwSkipped
type instance XXArg FormattedExprExt _ = Void

newtype ArgPosSkipped = ArgPosSkipped Skipped
  deriving (Show)

data ArgKwSkipped = ArgKwSkipped
  { aksPrefix :: Skipped
  , aksBetween :: Skipped
  } deriving (Show)

ppSkipped :: Skipped -> Doc
ppSkipped (Skipped str) = PP.string str

procedureWithNoName :: a
procedureWithNoName = error "Core invariant violated: procedure with no name"

opUnitWithAName :: a
opUnitWithAName = error "Core invariant violated: OpUnit with a name"

-- | Converts parse tree to formatted expression alongside with space skipped
-- after parsing provided tree. 'OpUnit' inside brackets is placed right before
-- the closing bracket i.e. "(<space> OpUnit)" instead of "(OpUnit <space>)".
parseTreeToFormattedExpr
  :: forall components.
     Expr ParseTreeExt CommandId components
  -> (Expr FormattedExprExt CommandId components, Skipped)
parseTreeToFormattedExpr =
  \case
    XExpr (ExprInBrackets l e r) ->
      let (e', se) = parseTreeToFormattedExpr e
      in (XExpr $ ExprInBrackets (l^.lSpaceAfter) e' se, r^.lSpaceAfter)
    ExprProcCall NoExt pc -> first (ExprProcCall NoExt) (pcToFormattedPc pc)
    ExprLit tok lit -> (ExprLit NoExt lit, tok^.lSpaceAfter)
  where
    pcToFormattedPc
      :: ProcCall' ParseTreeExt CommandId components
      -> (ProcCall' FormattedExprExt CommandId components, Skipped)
    pcToFormattedPc (ProcCall tok cmd args) =
      case cmd of
        CommandIdName _ ->
          case tok of
            Just tok' -> first (ProcCall NoExt cmd) (argGo (tok'^.lSpaceAfter) args)
            Nothing -> procedureWithNoName
        CommandIdOperator op ->
          case (op, tok, args) of
            (OpAndThen, Just tok', [ArgPos NoExt lhs, ArgPos NoExt rhs]) ->
              let
                (lhs', lSkipped) = parseTreeToFormattedExpr lhs
                lhs'' = ArgPos (ArgPosSkipped lSkipped) lhs'
                (rhs', rSkipped) = parseTreeToFormattedExpr rhs
                rhs'' = ArgPos (ArgPosSkipped $ tok'^.lSpaceAfter) rhs'
              in
                (ProcCall NoExt cmd [lhs'', rhs''], rSkipped)
            (OpAndThen, Nothing, [_, _]) -> procedureWithNoName

            (OpUnit, Nothing, []) -> (ProcCall NoExt cmd [], mempty)
            (OpUnit, Just _, []) -> opUnitWithAName

            _ -> invalidOperatorApplication

    argToFormattedArg
      :: Skipped
      -> Arg' ParseTreeExt CommandId components
      -> (Arg' FormattedExprExt CommandId components, Skipped)
    argToFormattedArg skippedBefore = \case
      XArg xxArg -> absurd xxArg
      ArgPos NoExt a -> first (ArgPos (ArgPosSkipped skippedBefore)) (parseTreeToFormattedExpr a)
      ArgKw nameTok name a ->
        let
          skipped = ArgKwSkipped
            { aksPrefix = skippedBefore
            , aksBetween = nameTok^.lSpaceAfter
            }
        in
          first (ArgKw skipped name) (parseTreeToFormattedExpr a)

    argGo
      :: Skipped
      -> [Arg' ParseTreeExt CommandId components]
      -> ([Arg' FormattedExprExt CommandId components], Skipped)
    argGo skippedBefore [] = ([], skippedBefore)
    argGo skippedBefore (arg : args) =
      let (arg', skippedAfter) = argToFormattedArg skippedBefore arg
      in first (arg' :) (argGo skippedAfter args)

ppFormattedExpr
  :: forall components.
     AllConstrained ComponentPrinter components
  => Expr FormattedExprExt CommandId components
  -> Doc
ppFormattedExpr =
  \case
    ExprLit NoExt l -> ppLit l
    ExprProcCall NoExt p -> ppProcCall p
    XExpr (ExprInBrackets l e r) ->
      PP.parens $ ppSkipped l PP.<> ppFormattedExpr e PP.<> ppSkipped r
  where
    ppProcCall (ProcCall NoExt commandName args) =
      case commandName of
        CommandIdName name -> ppProcedureCall name args
        CommandIdOperator op -> ppOperatorCall op args

    ppOperatorCall OpUnit [] = mempty
    ppOperatorCall OpAndThen [ArgPos (ArgPosSkipped lp) l, ArgPos (ArgPosSkipped rp) r] =
      ppFormattedExpr l PP.<> ppSkipped lp PP.<> PP.char ';' PP.<>
      ppSkipped rp PP.<> ppFormattedExpr r
    ppOperatorCall _ _ = invalidOperatorApplication

    ppProcedureCall procName args = nameToDoc procName PP.<> mconcat (map ppArg args)

    ppArg = \case
      ArgPos (ArgPosSkipped prefix) a -> ppSkipped prefix PP.<> ppFormattedExpr a
      ArgKw ArgKwSkipped{..} name a ->
        ppSkipped aksPrefix PP.<> nameToDoc name PP.<> PP.colon PP.<>
        ppSkipped aksBetween PP.<> ppFormattedExpr a
      XArg xxArg -> absurd xxArg
