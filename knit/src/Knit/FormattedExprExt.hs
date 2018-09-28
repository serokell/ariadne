module Knit.FormattedExprExt where

import Control.Monad.Writer.Strict hiding ((<>))
import Data.Semigroup (Option(..), option, (<>))

import qualified Data.Loc as L
import qualified Data.Loc.Span as L
import Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Knit.ParseTreeExt
import Knit.Prelude
import Knit.Printer
import Knit.Syntax
import Knit.Tokenizer

data FormattedExprExt

type instance XExprProcCall FormattedExprExt _ _ = NoExt
type instance XExprLit FormattedExprExt _ _ = NoExt
type instance XXExpr FormattedExprExt cmd components =
    FormattedExpr (Expr FormattedExprExt cmd components)

type instance XProcCall FormattedExprExt _ _ = NoExt

type instance XArgPos FormattedExprExt _ = NoExt
type instance XArgKw FormattedExprExt _ = NoExt
type instance XXArg FormattedExprExt a = WithPadding (Arg FormattedExprExt a)

data FormattedExpr a
    = ExprWithPadding (WithPadding a)
    | ExprInParens a
    deriving (Show, Functor, Foldable, Traversable)

data WithPadding a = WithPadding
    { wpPrefix :: Option SSpan
    , wpPostfix :: Option SSpan
    , wpItem :: a
    } deriving (Show, Functor, Foldable, Traversable)

spanToDoc :: Option SSpan -> Doc
spanToDoc = option mempty spanToDoc'
  where
    spanToDoc' :: SSpan -> Doc
    spanToDoc' (SSpan s) =
      if L.locLine (L.start s) == L.locLine (L.end s)
        then PP.string $ replicate (toInt $ L.locColumn (L.end s) - L.locColumn (L.start s)) ' '
        else PP.string
          $ replicate (toInt $ L.locLine (L.end s) - L.locLine (L.start s)) '\n'
          ++ replicate (toInt $ L.locColumn (L.end s) - 1) ' '

    toInt :: L.ToNat n => n -> Int
    toInt = fromIntegral . L.toNat

spanBetween :: SSpan -> SSpan -> Option SSpan
spanBetween (SSpan a) (SSpan b) =
    maybe mempty (Option . Just . SSpan) $ L.fromToMay (L.end a) (L.start b)

procedureWithNoName :: a
procedureWithNoName = error "Core invariant violated: procedure with no name"

opUnitWithAName :: a
opUnitWithAName = error "Core invariant violated: OpUnit with a name"

parseTreeToFormattedExpr
  :: forall components.
     Expr ParseTreeExt CommandId components
  -> Writer (Option SSpan) (Expr FormattedExprExt CommandId components)
parseTreeToFormattedExpr =
  \case
    XExpr (ExprInBrackets l e r) ->
      let
        decoratedE = parseTreeToFormattedExpr e
        decorate =
          XExpr . ExprInParens . XExpr . ExprWithPadding .
            case getOption $ execWriter decoratedE of
              Just sp -> WithPadding
                (fst l `spanBetween` sp)
                (sp `spanBetween` fst r)
              Nothing -> WithPadding
                (fst l `spanBetween` fst r)
                mempty
      in
        wrapSpan l *> fmap decorate decoratedE <* wrapSpan r
    ExprProcCall NoExt pc -> ExprProcCall NoExt <$> pcToFormattedPc pc
    ExprLit tok lit -> wrapSpan tok $> ExprLit NoExt lit
  where
    wrapSpan (a, b) = writer (b, Option $ Just a)

    pcToFormattedPc
      :: ProcCall' ParseTreeExt CommandId components
      -> Writer (Option SSpan) (ProcCall' FormattedExprExt CommandId components)
    pcToFormattedPc (ProcCall tok cmd args) =
      case cmd of
        CommandIdName _ ->
          case fmap fst tok of
            Just sp -> wrapSpan $ ProcCall NoExt cmd <$> argGo sp args
            Nothing -> procedureWithNoName
        CommandIdOperator op ->
          case (op, tok, args) of
            (OpAndThen, Just tokSp, [lhs, rhs]) ->
              let
                decoratedLhs =
                  decorateWithSpan (argToFormattedArg lhs) $ \sp ->
                    XArg . WithPadding
                      mempty
                      (sp `spanBetween` fst tokSp)
                decoratedRhs =
                  decorateWithSpan (argToFormattedArg rhs) $ \sp ->
                    XArg . WithPadding
                      (fst tokSp `spanBetween` sp)
                      mempty
              in
                ProcCall NoExt cmd <$> liftA3 (\a _ b -> [a, b])
                    decoratedLhs (wrapSpan tokSp) decoratedRhs
            (OpAndThen, Nothing, [_, _]) -> procedureWithNoName

            (OpUnit, Nothing, []) -> writer (ProcCall NoExt cmd [], Option Nothing)
            (OpUnit, Just _, []) -> opUnitWithAName

            _ -> invalidOperatorApplication

    argToFormattedArg
      :: Arg' ParseTreeExt CommandId components
      -> Writer (Option SSpan) (Arg' FormattedExprExt CommandId components)
    argToFormattedArg = \case
      XArg xxArg -> absurd xxArg
      ArgPos NoExt a -> ArgPos NoExt <$> parseTreeToFormattedExpr a
      ArgKw nameTok name a ->
        let
          decoratedExpr = parseTreeToFormattedExpr a
          wrappedExpr = decorateWithSpan decoratedExpr $ \sp ->
            XExpr . ExprWithPadding . WithPadding
              (fst nameTok `spanBetween` sp)
              mempty
        in
          wrapSpan nameTok $> ArgKw NoExt name <*> wrappedExpr

    decorateWithSpan
      :: Writer (Option SSpan) a
      -> (SSpan -> a -> a)
      -> Writer (Option SSpan) a
    decorateWithSpan w decorate = option w (\sp -> decorate sp <$> w) $ execWriter w

    argGo
      :: SSpan
      -> [Arg' ParseTreeExt CommandId components]
      -> (SSpan, [Arg' FormattedExprExt CommandId components])
    argGo prevSp [] = (prevSp, [])
    argGo prevSp (arg : args) =
      let
        decoratedArg = argToFormattedArg arg
        newSp = option prevSp (prevSp <>) $ execWriter decoratedArg
        wpPrefix = option mempty (prevSp `spanBetween`) $ execWriter decoratedArg
        wpPostfix = mempty
        wpItem = fst $ runWriter decoratedArg
      in
        (XArg WithPadding{..} :) <$> argGo newSp args

ppFormattedExpr
  :: forall components.
     AllConstrained ComponentPrinter components
  => Expr FormattedExprExt CommandId components
  -> Doc
ppFormattedExpr =
  \case
    ExprLit NoExt l -> ppLit l
    ExprProcCall NoExt p -> ppProcCall p
    XExpr (ExprInParens e) -> PP.parens $ ppFormattedExpr e
    XExpr (ExprWithPadding WithPadding{..}) ->
      spanToDoc wpPrefix <> ppFormattedExpr wpItem <> spanToDoc wpPostfix
  where
    ppProcCall (ProcCall NoExt commandName args) =
      case commandName of
        CommandIdName name -> ppProcedureCall name args
        CommandIdOperator op -> ppOperatorCall op args

    ppOperatorCall OpUnit [] = mempty
    ppOperatorCall OpAndThen [lhs, rhs] = ppArg lhs <> PP.char ';' <> ppArg rhs
    ppOperatorCall _ _ = invalidOperatorApplication

    ppProcedureCall procName args = nameToDoc procName <> mconcat (map ppArg args)

    ppArg = \case
      ArgPos NoExt a -> ppFormattedExpr a
      ArgKw NoExt name a -> nameToDoc name <> PP.colon <> ppFormattedExpr a
      XArg WithPadding{..} -> spanToDoc wpPrefix <> ppArg wpItem <> spanToDoc wpPostfix
