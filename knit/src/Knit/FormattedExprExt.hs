module Knit.FormattedExprExt
       ( FormattedExprExt
       , Padding(..)
       , ArgPosPadding(..)
       , ArgKwPadding(..)

       , exprInBrackets
       , noPadding
       , parseTreeToFormattedExpr
       , ppFormattedExpr
       ) where

import Control.Monad.Writer.Strict hiding ((<>))
import Data.Semigroup (Option(..), option, (<>))
import Numeric.Natural

import qualified Data.Loc as L
import qualified Data.Loc.Span as L
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
    ExprInBrackets Padding (Expr FormattedExprExt cmd components)

-- | Information about the space between procedure and its arguments and between
-- the arguments is stored with the arguments. Their meaning depends on the
-- context. In the case of 'OpAndThen' space between the arguments and semicolon
-- is stored. In the case of 'CommandIdName' each argument stores the space
-- before itself.
type instance XProcCall FormattedExprExt _ _ = NoExt

type instance XArgPos FormattedExprExt _ = ArgPosPadding
type instance XArgKw FormattedExprExt _ = ArgKwPadding
type instance XXArg FormattedExprExt _ = Void

exprInBrackets :: br -> br -> a -> ExprInBrackets br a
exprInBrackets l r x = ExprInBrackets l x r

-- | To print space between two entities, we first print some amount of newlines
-- and then print some amount of spaces.
data Padding = Padding
  { pLines :: Natural
  , pColumns :: Natural
  } deriving (Show, Eq, Ord)

noPadding :: Padding
noPadding = Padding 0 0

newtype ArgPosPadding = ArgPosPadding Padding
  deriving (Show)

data ArgKwPadding = ArgKwPadding
  { akpPrefix :: Padding
  , akpBetween :: Padding
  } deriving (Show)

ppPadding :: Padding -> Doc
ppPadding Padding{..} =
  PP.string
    $ replicate (fromIntegral pLines) '\n'
    ++ replicate (fromIntegral pColumns) ' '

paddingBetween :: SSpan -> SSpan -> Padding
paddingBetween (SSpan a) (SSpan b) =
  if L.locLine (L.end a) == L.locLine (L.start b)
    then Padding 0 $ L.toNat (L.locColumn (L.start b)) - L.toNat (L.locColumn (L.end a))
    else Padding
      (L.toNat (L.locLine (L.start b)) - L.toNat (L.locLine (L.end a)))
      (L.toNat (L.locColumn (L.start b)) - 1)

procedureWithNoName :: a
procedureWithNoName = error "Core invariant violated: procedure with no name"

opUnitWithAName :: a
opUnitWithAName = error "Core invariant violated: OpUnit with a name"

-- | Converts parse tree to formatted expression alongside with the span this
-- expression covers. 'OpUnit' inside brackets is placed right before the
-- closing bracket i.e. "(<space> OpUnit)" instead of "(OpUnit <space>)".
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
                (fst l `paddingBetween` sp)
                (sp `paddingBetween` fst r)
              Nothing -> exprInBrackets
                (fst l `paddingBetween` fst r)
                noPadding
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
                    option noPadding (`paddingBetween` fst tokSp) $ execWriter lhs')
                  <$> lhs'
                rhs' = parseTreeToFormattedExpr rhs
                rhs'' =
                  ArgPos (ArgPosPadding $
                    option noPadding (fst tokSp `paddingBetween`) $ execWriter rhs')
                  <$> rhs'
              in
                ProcCall NoExt cmd <$> liftA3 (\a _ b -> [a, b])
                    lhs'' (wrapSpan tokSp) rhs''
            (OpAndThen, Nothing, [_, _]) -> procedureWithNoName

            (OpUnit, Nothing, []) -> writer (ProcCall NoExt cmd [], Option Nothing)
            (OpUnit, Just _, []) -> opUnitWithAName

            _ -> invalidOperatorApplication

    argToFormattedArg
      :: (SSpan -> Padding)
      -> Arg' ParseTreeExt CommandId components
      -> Writer (Option SSpan) (Arg' FormattedExprExt CommandId components)
    argToFormattedArg mkPadding = \case
      XArg xxArg -> absurd xxArg
      ArgPos NoExt a ->
        let
          a' = parseTreeToFormattedExpr a
          padding = ArgPosPadding $ option noPadding mkPadding $ execWriter a'
        in
          ArgPos padding <$> a'
      ArgKw nameTok name a ->
        let
          a' = parseTreeToFormattedExpr a
          padding = ArgKwPadding
            { akpPrefix = mkPadding (fst nameTok)
            , akpBetween = option noPadding (fst nameTok `paddingBetween`) $ execWriter a'
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
        arg' = argToFormattedArg (prevSp `paddingBetween`) arg
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
      PP.parens $ ppPadding l <> ppFormattedExpr e <> ppPadding r
  where
    ppProcCall (ProcCall NoExt commandName args) =
      case commandName of
        CommandIdName name -> ppProcedureCall name args
        CommandIdOperator op -> ppOperatorCall op args

    ppOperatorCall OpUnit [] = mempty
    ppOperatorCall OpAndThen [ArgPos (ArgPosPadding lp) l, ArgPos (ArgPosPadding rp) r] =
      ppFormattedExpr l <> ppPadding lp <> PP.char ';' <>
      ppPadding rp <> ppFormattedExpr r
    ppOperatorCall _ _ = invalidOperatorApplication

    ppProcedureCall procName args = nameToDoc procName <> mconcat (map ppArg args)

    ppArg = \case
      ArgPos (ArgPosPadding prefix) a -> ppPadding prefix <> ppFormattedExpr a
      ArgKw (ArgKwPadding prefix between) name a ->
        ppPadding prefix <> nameToDoc name <> PP.colon <> ppPadding between <> ppFormattedExpr a
      XArg xxArg -> absurd xxArg
