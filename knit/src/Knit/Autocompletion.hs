module Knit.Autocompletion where

import Data.Maybe (mapMaybe)
import Data.Semigroup (Option(..), option)

import Knit.Argument
import Knit.FormattedExprExt
import Knit.Name
import Knit.ParseTreeExt
import Knit.Prelude
import Knit.Printer
import Knit.Procedure
import Knit.Syntax
import Knit.Tokenizer

suggestionExprs
  :: forall components.
     [SomeCommandProc components]
  -> Word
  -> Expr FormattedExprExt CommandId components
  -> [Expr FormattedExprExt CommandId components]
suggestionExprs procs = skipParensExpr
  where
    didn'tFindParen :: a
    didn'tFindParen = error "Core invariant violated: not enough parentheses"

    skipParensExpr
      :: Word
      -> Expr FormattedExprExt CommandId components
      -> [Expr FormattedExprExt CommandId components]
    skipParensExpr n = \case
      ExprLit NoExt _ -> didn'tFindParen
      ExprProcCall NoExt p -> ExprProcCall NoExt <$> skipParensPc n p
      XExpr (ExprInBrackets l e r) ->
        if n > 0
          then XExpr . exprInBrackets l r <$> skipParensExpr (n - 1) e
          else XExpr . exprInBrackets l mempty <$>
            option (autocompleteExpr e) (flip suggestExpr e) r

    skipParensPc
      :: Word
      -> ProcCall' FormattedExprExt CommandId components
      -> [ProcCall' FormattedExprExt CommandId components]
    skipParensPc n (ProcCall NoExt cmd args) =
      case unsnoc args of
        Nothing -> didn'tFindParen
        Just (init', last') ->
          ProcCall NoExt cmd . snoc init' <$> skipParensArg n last'

    skipParensArg
      :: Word
      -> Arg' FormattedExprExt CommandId components
      -> [Arg' FormattedExprExt CommandId components]
    skipParensArg n = \case
      ArgPos padding e -> ArgPos padding <$> skipParensExpr n e
      ArgKw padding name e -> ArgKw padding name <$> skipParensExpr n e
      XArg xxArg -> absurd xxArg

    autocompleteExpr = const []

    suggestExpr
      :: SSpan
      -> Expr FormattedExprExt CommandId components
      -> [Expr FormattedExprExt CommandId components]
    suggestExpr padding = \case
      ExprLit NoExt _ -> []
      ExprProcCall NoExt p -> ExprProcCall NoExt <$> suggestPc padding p
      XExpr (ExprInBrackets _ _ _) -> []

    suggestPc
      :: SSpan
      -> ProcCall' FormattedExprExt CommandId components
      -> [ProcCall' FormattedExprExt CommandId components]
    suggestPc padding (ProcCall NoExt cmd args) =
      case cmd of
        CommandIdOperator OpUnit -> error "TODO can't happen"
        CommandIdOperator OpAndThen ->
          case args of
            [lhs, ArgPos
              (ArgPosPadding (Option Nothing))
              ( ExprProcCall
                NoExt
                ( ProcCall
                  NoExt
                  (CommandIdOperator OpUnit)
                  []
                )
              )] ->
              ProcCall NoExt cmd
                . snoc [lhs]
                . ArgPos (ArgPosPadding (pure padding))
                . toProcCall
                <$> suggestableProcs
            [lhs, ArgPos padding1 rhs] ->
              ProcCall NoExt cmd
                . snoc [lhs]
                . ArgPos padding1
                <$> suggestExpr padding rhs
            _ -> invalidOperatorApplication
        CommandIdName name ->
          concat
            [ case find ((== name) . fst) suggestableProcs of
                Nothing -> []
                Just (_, params) ->
                  ProcCall NoExt cmd
                    . snoc args
                    . (\param ->
                        ArgKw
                          (ArgKwPadding (pure padding) mempty)
                          param
                          (ExprProcCall NoExt (ProcCall NoExt (CommandIdOperator OpUnit) []))
                      )
                    <$> params
            , ProcCall NoExt cmd
              . snoc args
              . ArgPos (ArgPosPadding (pure padding))
              . toProcCall
              <$> filter (null . snd) suggestableProcs
            ]

    suggestableProcs :: [(Name, [Name])]
    suggestableProcs =
      flip mapMaybe procs $ \(SomeCommandProc CommandProc{..}) ->
        case cpName of
          CommandIdName name -> Just (name, map (^._1) $ getParameters cpArgumentConsumer)
          CommandIdOperator _ -> Nothing

    toProcCall =
      ExprProcCall NoExt
        . (\name -> ProcCall NoExt (CommandIdName name) [])
        . fst
