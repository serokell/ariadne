module Knit.FormattedExprExt where

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
    Formatted (Expr FormattedExprExt cmd components)

type instance XProcCall FormattedExprExt _ _ = NoExt

type instance XArgPos FormattedExprExt _ = NoExt
type instance XArgKw FormattedExprExt _ = NoExt
type instance XXArg FormattedExprExt a = Formatted (Arg FormattedExprExt a)

data Formatted a = Formatted
    { dPrefix :: Doc
    , dPostfix :: Doc
    , dItem :: a
    } deriving (Show, Functor, Foldable, Traversable)

docBetween :: SSpan -> SSpan -> Doc
docBetween (SSpan a) (SSpan b) =
    maybe mempty spanToDoc $ L.fromToMay (L.end a) (L.start b)
  where
    spanToDoc :: L.Span -> Doc
    spanToDoc s =
      if L.locLine (L.start s) == L.locLine (L.end s)
        then PP.string $ replicate (toInt $ L.locColumn (L.end s) - L.locColumn (L.start s)) ' '
        else PP.string
          $ replicate (toInt $ L.locLine (L.end s) - L.locLine (L.start s)) '\n'
          ++ replicate (toInt $ L.locColumn (L.end s) - 1) ' '

    toInt :: L.ToNat n => n -> Int
    toInt = fromIntegral . L.toNat

procedureWithNoName :: a
procedureWithNoName = error "Core invariant violated: procedure with no name"

opUnitWithAName :: a
opUnitWithAName = error "Core invariant violated: OpUnit with a name"

parseTreeToFormattedExpr
  :: Expr ParseTreeExt CommandId components
  -> (Option SSpan, Expr FormattedExprExt CommandId components)
parseTreeToFormattedExpr =
  \case
    XExpr (ExprInBrackets l e r) ->
      let
        decoratedE = parseTreeToFormattedExpr e
        decorate = case getOption $ fst decoratedE of
          Just sp -> Formatted
            (PP.lparen <> fst l `docBetween` sp)
            (sp `docBetween` fst r <> PP.rparen)
          Nothing -> Formatted
            mempty
            (PP.parens $ fst l `docBetween` fst r)
      in
        wrapSpan l *> fmap (XExpr . decorate) decoratedE <* wrapSpan r
    ExprProcCall NoExt pc -> ExprProcCall NoExt <$> pcToFormattedPc pc
    ExprLit tok lit -> wrapSpan tok $> ExprLit NoExt lit
  where
    wrapSpan = first (Option . Just)

    pcToFormattedPc
      :: ProcCall ParseTreeExt CommandId (Expr ParseTreeExt CommandId components)
      -> (Option SSpan, ProcCall FormattedExprExt CommandId (Expr FormattedExprExt CommandId components))
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
                    XArg . Formatted
                      mempty
                      (sp `docBetween` fst tokSp)
                decoratedRhs =
                  decorateWithSpan (argToFormattedArg rhs) $ \sp ->
                    XArg . Formatted
                      (fst tokSp `docBetween` sp)
                      mempty
              in
                ProcCall NoExt cmd <$> liftA3 (\a _ b -> [a, b])
                    decoratedLhs (wrapSpan tokSp) decoratedRhs
            (OpAndThen, Nothing, [_, _]) -> procedureWithNoName

            (OpUnit, Nothing, []) -> (Option Nothing, ProcCall NoExt cmd [])
            (OpUnit, Just _, []) -> opUnitWithAName

            _ -> invalidOperatorApplication

    argToFormattedArg
      :: Arg ParseTreeExt (Expr ParseTreeExt CommandId components)
      -> (Option SSpan, Arg FormattedExprExt (Expr FormattedExprExt CommandId components))
    argToFormattedArg = \case
      XArg xxArg -> absurd xxArg
      ArgPos NoExt a -> ArgPos NoExt <$> parseTreeToFormattedExpr a
      ArgKw nameTok name a ->
        let
          decoratedExpr = parseTreeToFormattedExpr a
          wrappedExpr = decorateWithSpan decoratedExpr $ \sp ->
            XExpr . Formatted
              (fst nameTok `docBetween` sp)
              mempty
        in
          wrapSpan nameTok $> ArgKw NoExt name <*> wrappedExpr

    decorateWithSpan
        :: (Option SSpan, a)
        -> (SSpan -> a -> a)
        -> (Option SSpan, a)
    decorateWithSpan v@(Option Nothing, _) _ = v
    decorateWithSpan v@(Option (Just sp), _) decorate = decorate sp <$> v

    argGo
      :: SSpan
      -> [Arg ParseTreeExt (Expr ParseTreeExt CommandId components)]
      -> (SSpan, [Arg FormattedExprExt (Expr FormattedExprExt CommandId components)])
    argGo prevSp [] = (prevSp, [])
    argGo prevSp (arg : args) =
      let
        decoratedArg = argToFormattedArg arg
        newSp = option prevSp (prevSp <>) $ fst decoratedArg
        dPrefix =
          case getOption $ fst decoratedArg of
            Just argSp -> prevSp `docBetween` argSp
            Nothing -> mempty
        dPostfix = mempty
        dItem = snd decoratedArg
      in
        (XArg Formatted{..} :) <$> argGo newSp args

ppFormattedExpr
  :: forall components.
     AllConstrained ComponentPrinter components
  => Expr FormattedExprExt CommandId components
  -> Doc
ppFormattedExpr =
  \case
    ExprLit NoExt l -> ppLit l
    ExprProcCall NoExt p -> ppProcCall p
    XExpr Formatted{..} -> dPrefix <> ppFormattedExpr dItem <> dPostfix
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
      XArg Formatted{..} -> dPrefix <> ppArg dItem <> dPostfix
