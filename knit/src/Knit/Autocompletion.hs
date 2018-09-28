module Knit.Autocompletion where

import Data.Maybe (mapMaybe)

import Knit.FormattedExprExt
import Knit.Prelude
import Knit.Procedure
import Knit.Syntax

suggestProc
  :: forall components.
     [SomeCommandProc components]
  -> Expr FormattedExprExt CommandId components
  -> [Expr FormattedExprExt CommandId components]
suggestProc procs = suggestProcExpr
  where
    suggestProcExpr
      :: Expr FormattedExprExt CommandId components
      -> [Expr FormattedExprExt CommandId components]
    suggestProcExpr =
      \case
        e@(ExprLit NoExt _) -> pure e
        ExprProcCall NoExt p -> ExprProcCall NoExt <$> suggestProcPc p
        XExpr (ExprInParens e) -> XExpr . ExprInParens <$> suggestProcExpr e
        XExpr (ExprWithPadding WithPadding{..}) ->
          XExpr . ExprWithPadding . WithPadding wpPrefix wpPostfix <$> suggestProcExpr wpItem

    suggestProcPc
      :: ProcCall' FormattedExprExt CommandId components
      -> [ProcCall' FormattedExprExt CommandId components]
    suggestProcPc pc@(ProcCall NoExt cmd args) =
      case cmd of
        CommandIdOperator OpUnit -> suggestedProcCalls
        _ ->
          case unsnoc args of
            Nothing -> pure pc
            Just (init', last') -> do
              suggestedLast <- suggestProcArg last'
              pure $ ProcCall NoExt cmd $ init' ++ [suggestedLast]

    suggestProcArg
      :: Arg' FormattedExprExt CommandId components
      -> [Arg' FormattedExprExt CommandId components]
    suggestProcArg =
      \case
        ArgPos NoExt e -> ArgPos NoExt <$> suggestProcExpr e
        ArgKw NoExt name e -> ArgKw NoExt name <$> suggestProcExpr e
        XArg WithPadding{..} -> XArg . WithPadding wpPrefix wpPostfix <$> suggestProcArg wpItem

    suggestedProcCalls =
      flip mapMaybe procs $ \(SomeCommandProc cp) ->
        case cpName cp of
          CommandIdName _ -> Just $ ProcCall NoExt (cpName cp) []
          CommandIdOperator _ -> Nothing
