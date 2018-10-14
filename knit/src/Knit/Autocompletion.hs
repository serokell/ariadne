module Knit.Autocompletion
       ( suggestions
       ) where

import Data.List (isPrefixOf)
import Data.Loc (loc, spanFromTo)
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text.Buildable (build)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (toLazyText)
import Text.Earley (fullParses)

import Knit.Argument
import Knit.FormattedExprExt
import Knit.Name
import Knit.Parser
import Knit.ParseTreeExt
import Knit.Prelude
import Knit.Printer
import Knit.Procedure
import Knit.Syntax
import Knit.Tokenizer

suggestions
  :: forall components proxy.
     ( KnownSpine components
     , AllConstrained (ComponentTokenizer components) components
     , AllConstrained (ComponentTokenToLit components) components
     , AllConstrained (ComponentCommandProcs components) components
     , AllConstrained ComponentPrinter components
     )
  => proxy components
  -> T.Text
  -> [T.Text]
suggestions _ cmd =
  let
    cmd' = "(" <> cmd
    tokens = snd $ tokenize cmd'

    tokenBalance = \case
      TokenParenthesis bs -> withBracketSide 1 (-1) bs
      _ -> 0
    parensBalance = sum $ map (tokenBalance . _lItem) tokens
    cmdLines = T.splitOn "\n" cmd'
    endLoc = (length cmdLines, T.length (last cmdLines) + 1)

    loc' a b = loc (fromIntegral a) (fromIntegral b)
    locatedClosingBracket i t = Located
      { _lSpan = spanFromTo
          (loc' (fst endLoc) (snd endLoc + i))
          (loc' (fst endLoc) (snd endLoc + i + 1))
      , _lSpaceAfter = mempty
      , _lItem = t
      }

    tokens' = tokens ++ zipWith locatedClosingBracket [0..]
      (replicate parensBalance (TokenParenthesis BracketSideClosing))
  in
    case fullParses (pExpr @components) tokens' of
      ([], _) -> []
      (tree:_, _) ->
        map (T.drop 1
          . T.dropEnd parensBalance
          . T.pack
          . show
          . ppFormattedExpr)
        $ suggestionExprs commandProcs parensBalance
        $ fst $ parseTreeToFormattedExpr tree

-- | Returns completion suggestions assuming that the expression was completed
-- with missing parentheses and wrapped into one extra pair of parentheses
-- before parsing.
suggestionExprs
  :: forall components.
     [SomeCommandProc components]
  -> Int -- ^ Number of added closing parentheses.
  -> Expr FormattedExprExt CommandId components
  -> [Expr FormattedExprExt CommandId components]
suggestionExprs procs = skipParensExpr
  where
    didn'tFindParen :: a
    didn'tFindParen = error "Core invariant violated: not enough parentheses"

    paddedOpUnit :: a
    paddedOpUnit = error "Core invariant violated: space between OpUnit and closing paren"

    skipParensExpr
      :: Int
      -> Expr FormattedExprExt CommandId components
      -> [Expr FormattedExprExt CommandId components]
    skipParensExpr n = \case
      ExprLit NoExt _ -> didn'tFindParen
      ExprProcCall NoExt p -> ExprProcCall NoExt <$> skipParensPc n p
      XExpr (ExprInBrackets l e r) ->
        if n > 1
          then XExpr . exprInBrackets l r <$> skipParensExpr (n - 1) e
          else XExpr . exprInBrackets l mempty <$>
            if r == mempty
              then autocompleteExpr e
              else suggestExpr r e

    skipParensPc
      :: Int
      -> ProcCall' FormattedExprExt CommandId components
      -> [ProcCall' FormattedExprExt CommandId components]
    skipParensPc n (ProcCall NoExt cmd args) =
      case unsnoc args of
        Nothing -> didn'tFindParen
        Just (init', last') ->
          ProcCall NoExt cmd . snoc init' <$> skipParensArg n last'

    skipParensArg
      :: Int
      -> Arg' FormattedExprExt CommandId components
      -> [Arg' FormattedExprExt CommandId components]
    skipParensArg n = \case
      ArgPos skipped e -> ArgPos skipped <$> skipParensExpr n e
      ArgKw skipped name e -> ArgKw skipped name <$> skipParensExpr n e
      XArg xxArg -> absurd xxArg

    -- | Completes some unfinished word.
    autocompleteExpr
      :: Expr FormattedExprExt CommandId components
      -> [Expr FormattedExprExt CommandId components]
    autocompleteExpr = \case
      ExprLit NoExt _ -> []
      ExprProcCall NoExt p -> ExprProcCall NoExt <$> autocompletePc p
      XExpr ExprInBrackets{} -> []

    autocompletePc
      :: ProcCall' FormattedExprExt CommandId components
      -> [ProcCall' FormattedExprExt CommandId components]
    autocompletePc (ProcCall NoExt cmd args) =
      case cmd of
        CommandIdOperator OpUnit -> toProcCall <$> suggestableProcs
        CommandIdOperator OpAndThen ->
          case args of
            [lhs, ArgPos skipped rhs] ->
              ProcCall NoExt cmd
              . snoc [lhs]
              . ArgPos skipped
              <$> autocompleteExpr rhs
            _ -> invalidOperatorApplication
        CommandIdName name ->
          case unsnoc args of
            Nothing ->
              toProcCall <$> filter (isPrefixOfNeq (nameStr name) . nameStr . fst) suggestableProcs
            Just (init', last') ->
              ProcCall NoExt cmd . snoc init' <$> autocompleteArg name last'

    autocompleteArg
      :: Name
      -> Arg' FormattedExprExt CommandId components
      -> [Arg' FormattedExprExt CommandId components]
    autocompleteArg cmdName = \case
      ArgPos
          (ArgPosSkipped skipped)
          (ExprProcCall NoExt (ProcCall NoExt (CommandIdName name) [])) ->
        let
          kwargs =
            (\param ->
              ArgKw
                (ArgKwSkipped skipped mempty)
                param
                (ExprProcCall NoExt (ProcCall NoExt (CommandIdOperator OpUnit) []))
            )
            <$> filter (isPrefixOf (nameStr name) . nameStr) (procParams cmdName)
          procCalls =
            ArgPos (ArgPosSkipped skipped)
            . ExprProcCall NoExt
            . toProcCall
            <$> filter
              (\(proc, params) -> null params && isPrefixOfNeq (nameStr name) (nameStr proc))
              suggestableProcs
        in
          kwargs ++ procCalls
      ArgPos skipped e -> ArgPos skipped <$> autocompleteExpr e
      ArgKw skipped name e -> ArgKw skipped name <$> autocompleteExpr e
      XArg xxArg -> absurd xxArg

    -- | Suggests possible keyword arguments and procedures.
    suggestExpr
      :: Skipped
      -> Expr FormattedExprExt CommandId components
      -> [Expr FormattedExprExt CommandId components]
    suggestExpr padding = \case
      ExprLit NoExt _ -> []
      ExprProcCall NoExt p -> ExprProcCall NoExt <$> suggestPc padding p
      XExpr ExprInBrackets{} -> []

    suggestPc
      :: Skipped
      -> ProcCall' FormattedExprExt CommandId components
      -> [ProcCall' FormattedExprExt CommandId components]
    suggestPc padding (ProcCall NoExt cmd args) =
      case cmd of
        CommandIdOperator OpUnit -> paddedOpUnit
        CommandIdOperator OpAndThen ->
          case args of
            [lhs, ArgPos
                (ArgPosSkipped (Skipped ""))
                (ExprProcCall NoExt (ProcCall NoExt (CommandIdOperator OpUnit) []))] ->
              ProcCall NoExt cmd
                . snoc [lhs]
                . ArgPos (ArgPosSkipped padding)
                . ExprProcCall NoExt
                . toProcCall
                <$> suggestableProcs
            [lhs, ArgPos padding1 rhs] ->
              ProcCall NoExt cmd
                . snoc [lhs]
                . ArgPos padding1
                <$> suggestExpr padding rhs
            _ -> invalidOperatorApplication
        CommandIdName cmdName ->
          let
            kwargs =
              ProcCall NoExt cmd
              . snoc args
              . (\param ->
                  ArgKw
                    (ArgKwSkipped padding mempty)
                    param
                    (ExprProcCall NoExt (ProcCall NoExt (CommandIdOperator OpUnit) []))
                )
              <$> procParams cmdName
            procCalls =
              ProcCall NoExt cmd
              . snoc args
              . ArgPos (ArgPosSkipped padding)
              . ExprProcCall NoExt
              . toProcCall
              <$> filter (null . snd) suggestableProcs
          in
            kwargs ++ procCalls

    suggestableProcs :: [(Name, [Name])]
    suggestableProcs =
      flip mapMaybe procs $ \(SomeCommandProc CommandProc{..}) ->
        case cpName of
          CommandIdName name -> Just (name, map (^._1) $ getParameters cpArgumentConsumer)
          CommandIdOperator _ -> Nothing

    procParams :: Name -> [Name]
    procParams cmdName = maybe [] snd $ find ((== cmdName) . fst) suggestableProcs

    toProcCall = (\name -> ProcCall NoExt (CommandIdName name) []) . fst

    nameStr = unpack . toLazyText . build

    isPrefixOfNeq a b = isPrefixOf a b && a /= b

    exprInBrackets l r x = ExprInBrackets l x r
