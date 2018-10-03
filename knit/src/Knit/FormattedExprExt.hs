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
    ExprInBrackets (Option SSpan) (Expr FormattedExprExt cmd components)

type instance XProcCall FormattedExprExt _ _ = NoExt

type instance XArgPos FormattedExprExt _ = ArgPosPadding
type instance XArgKw FormattedExprExt _ = ArgKwPadding
type instance XXArg FormattedExprExt _ = Void

exprInBrackets :: br -> br -> a -> ExprInBrackets br a
exprInBrackets l r x = ExprInBrackets l x r

newtype ArgPosPadding = ArgPosPadding (Option SSpan)
  deriving (Show)

data ArgKwPadding = ArgKwPadding
  { akpPrefix :: Option SSpan
  , akpBetween :: Option SSpan
  } deriving (Show)

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
        e' = parseTreeToFormattedExpr e
        decorate =
          XExpr .
            case getOption $ execWriter e' of
              Just sp -> exprInBrackets
                (fst l `spanBetween` sp)
                (sp `spanBetween` fst r)
              Nothing -> exprInBrackets
                (fst l `spanBetween` fst r)
                mempty
      in
        wrapSpan l *> fmap decorate e' <* wrapSpan r
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
            (OpAndThen, Just tokSp, [ArgPos NoExt lhs, ArgPos NoExt rhs]) ->
              let
                lhs' = parseTreeToFormattedExpr lhs
                lhs'' =
                  ArgPos (ArgPosPadding $
                    option mempty (`spanBetween` fst tokSp) $ execWriter lhs')
                  <$> lhs'
                rhs' = parseTreeToFormattedExpr rhs
                rhs'' =
                  ArgPos (ArgPosPadding $
                    option mempty (fst tokSp `spanBetween`) $ execWriter rhs')
                  <$> rhs'
              in
                ProcCall NoExt cmd <$> liftA3 (\a _ b -> [a, b])
                    lhs'' (wrapSpan tokSp) rhs''
            (OpAndThen, Nothing, [_, _]) -> procedureWithNoName

            (OpUnit, Nothing, []) -> writer (ProcCall NoExt cmd [], Option Nothing)
            (OpUnit, Just _, []) -> opUnitWithAName

            _ -> invalidOperatorApplication

    argToFormattedArg
      :: (SSpan -> Option SSpan)
      -> Arg' ParseTreeExt CommandId components
      -> Writer (Option SSpan) (Arg' FormattedExprExt CommandId components)
    argToFormattedArg mkPadding = \case
      XArg xxArg -> absurd xxArg
      ArgPos NoExt a ->
        let
          a' = parseTreeToFormattedExpr a
          padding = ArgPosPadding $ option mempty mkPadding $ execWriter a'
        in
          ArgPos padding <$> a'
      ArgKw nameTok name a ->
        let
          a' = parseTreeToFormattedExpr a
          padding = ArgKwPadding
            { akpPrefix = mkPadding (fst nameTok)
            , akpBetween = option mempty (fst nameTok `spanBetween`) $ execWriter a'
            }
        in
          wrapSpan nameTok $> ArgKw padding name <*> a'

    argGo
      :: SSpan
      -> [Arg' ParseTreeExt CommandId components]
      -> (SSpan, [Arg' FormattedExprExt CommandId components])
    argGo prevSp [] = (prevSp, [])
    argGo prevSp (arg : args) =
      let
        arg' = argToFormattedArg (prevSp `spanBetween`) arg
        newSp = option prevSp (prevSp <>) $ execWriter arg'
      in
        (fst (runWriter arg') :) <$> argGo newSp args

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
      PP.parens $ spanToDoc l <> ppFormattedExpr e <> spanToDoc r
  where
    ppProcCall (ProcCall NoExt commandName args) =
      case commandName of
        CommandIdName name -> ppProcedureCall name args
        CommandIdOperator op -> ppOperatorCall op args

    ppOperatorCall OpUnit [] = mempty
    ppOperatorCall OpAndThen [ArgPos (ArgPosPadding lp) l, ArgPos (ArgPosPadding rp) r] =
      ppFormattedExpr l <> spanToDoc lp <> PP.char ';' <>
      spanToDoc rp <> ppFormattedExpr r
    ppOperatorCall _ _ = invalidOperatorApplication

    ppProcedureCall procName args = nameToDoc procName <> mconcat (map ppArg args)

    ppArg = \case
      ArgPos (ArgPosPadding prefix) a -> spanToDoc prefix <> ppFormattedExpr a
      ArgKw (ArgKwPadding prefix between) name a ->
        spanToDoc prefix <> nameToDoc name <> PP.colon <> spanToDoc between <> ppFormattedExpr a
      XArg xxArg -> absurd xxArg
