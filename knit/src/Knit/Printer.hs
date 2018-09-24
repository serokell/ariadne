module Knit.Printer where

import Data.List as List
import Data.Semigroup (Option(..), option, (<>))
import qualified Data.Text as T
import Data.Text.Buildable (build)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)

import qualified Data.Loc as L
import qualified Data.Loc.Span as L

import Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Knit.Inflate
import Knit.Name
import Knit.ParseTreeExt
import Knit.Prelude
import Knit.Syntax
import Knit.Tokenizer
import Knit.Value

class ComponentPrinter component where
  componentPpLit :: ComponentLit component -> Doc
  componentPpToken :: ComponentToken component -> Doc

data DecoratorExt

type instance XExprProcCall DecoratorExt _ _ = NoExt
type instance XExprLit DecoratorExt _ _ = NoExt
type instance XXExpr DecoratorExt cmd components =
    Decorated (Expr DecoratorExt cmd components)

type instance XProcCall DecoratorExt _ _ = NoExt

type instance XArgPos DecoratorExt _ = NoExt
type instance XArgKw DecoratorExt _ = NoExt
type instance XXArg DecoratorExt a = Decorated (Arg DecoratorExt a)

data Decorated a = Decorated
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

invalidOperatorApplication :: a
invalidOperatorApplication = error "Core invariant violated: invalid operator application"

procedureWithNoName :: a
procedureWithNoName = error "Core invariant violated: procedure with no name"

opUnitWithAName :: a
opUnitWithAName = error "Core invariant violated: OpUnit with a name"

parseTreeToDecoratedTree
  :: Expr ParseTreeExt CommandId components
  -> (Option SSpan, Expr DecoratorExt CommandId components)
parseTreeToDecoratedTree =
  \case
    XExpr (ExprInBrackets l e r) ->
      let
        decoratedE = parseTreeToDecoratedTree e
        decorate = case getOption $ fst decoratedE of
          Just sp -> Decorated
            (PP.lparen <> fst l `docBetween` sp)
            (sp `docBetween` fst r <> PP.rparen)
          Nothing -> Decorated
            mempty
            (PP.parens $ fst l `docBetween` fst r)
      in
        wrapSpan l *> fmap (XExpr . decorate) decoratedE <* wrapSpan r
    ExprProcCall NoExt pc -> ExprProcCall NoExt <$> pcToDecoratedPc pc
    ExprLit tok lit -> wrapSpan tok $> ExprLit NoExt lit
  where
    wrapSpan = first (Option . Just)

    pcToDecoratedPc
      :: ProcCall ParseTreeExt CommandId (Expr ParseTreeExt CommandId components)
      -> (Option SSpan, ProcCall DecoratorExt CommandId (Expr DecoratorExt CommandId components))
    pcToDecoratedPc (ProcCall tok cmd args) =
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
                  decorateWithSpan (argToDecoratedArg lhs) $ \sp ->
                    XArg . Decorated
                      mempty
                      (sp `docBetween` fst tokSp)
                decoratedRhs =
                  decorateWithSpan (argToDecoratedArg rhs) $ \sp ->
                    XArg . Decorated
                      (fst tokSp `docBetween` sp)
                      mempty
              in
                ProcCall NoExt cmd <$> liftA3 (\a _ b -> [a, b])
                    decoratedLhs (wrapSpan tokSp) decoratedRhs
            (OpAndThen, Nothing, [_, _]) -> procedureWithNoName

            (OpUnit, Nothing, []) -> (Option Nothing, ProcCall NoExt cmd [])
            (OpUnit, Just _, []) -> opUnitWithAName

            _ -> invalidOperatorApplication

    argToDecoratedArg
      :: Arg ParseTreeExt (Expr ParseTreeExt CommandId components)
      -> (Option SSpan, Arg DecoratorExt (Expr DecoratorExt CommandId components))
    argToDecoratedArg = \case
      XArg xxArg -> absurd xxArg
      ArgPos NoExt a -> ArgPos NoExt <$> parseTreeToDecoratedTree a
      ArgKw nameTok name a ->
        let
          decoratedExpr = parseTreeToDecoratedTree a
          wrappedExpr = decorateWithSpan decoratedExpr $ \sp ->
            XExpr . Decorated
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
      -> (SSpan, [Arg DecoratorExt (Expr DecoratorExt CommandId components)])
    argGo prevSp [] = (prevSp, [])
    argGo prevSp (arg : args) =
      let
        decoratedArg = argToDecoratedArg arg
        newSp = option prevSp (prevSp <>) $ fst decoratedArg
        dPrefix =
          case getOption $ fst decoratedArg of
            Just argSp -> prevSp `docBetween` argSp
            Nothing -> mempty
        dPostfix = mempty
        dItem = snd decoratedArg
      in
        (XArg Decorated{..} :) <$> argGo newSp args

ppDecoratedTree
  :: forall components.
     AllConstrained ComponentPrinter components
  => Expr DecoratorExt CommandId components
  -> Doc
ppDecoratedTree =
  \case
    ExprLit NoExt l -> ppLit l
    ExprProcCall NoExt p -> ppProcCall p
    XExpr Decorated{..} -> dPrefix <> ppDecoratedTree dItem <> dPostfix
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
      ArgPos NoExt a -> ppDecoratedTree a
      ArgKw NoExt name a -> nameToDoc name <> PP.colon <> ppDecoratedTree a
      XArg Decorated{..} -> dPrefix <> ppArg dItem <> dPostfix

ppLit
  :: forall components.
     AllConstrained ComponentPrinter components
  => Lit components
  -> Doc
ppLit = ufoldConstrained @ComponentPrinter componentPpLit . getLitUnion

ppToken
  :: forall components.
     AllConstrained ComponentPrinter components
  => Token components
  -> Doc
ppToken = \case
  Token u -> ufoldConstrained @ComponentPrinter componentPpToken u
  TokenSquareBracket _ -> "square bracket"
  TokenParenthesis _ -> "parenthesis"
  TokenEquals -> "equality sign"
  TokenSemicolon -> "semicolon"
  TokenName _ -> "procedure name"
  TokenKey _ -> "key"
  TokenUnknown c -> text ("character '" <> T.singleton c <> "'")

ppExpr
  :: AllConstrained ComponentPrinter components
  => Expr NoExt CommandId components
  -> Doc
ppExpr =
  \case
    ExprLit NoExt l -> ppLit l
    ExprProcCall NoExt p -> ppProcCall p
    XExpr xxExpr -> absurd xxExpr
  where
    ppProcCall (ProcCall NoExt commandName args) =
      case commandName of
        CommandIdName name -> ppProcedureCall name args
        CommandIdOperator op -> ppOperatorCall op args

    ppOperatorCall OpUnit [] = mempty
    ppOperatorCall OpAndThen [ArgPos NoExt a, ArgPos NoExt b] =
      (parensIfSemicolon a (ppExpr a) <> PP.char ';') PP.<$> ppExpr b
    ppOperatorCall _ _ = invalidOperatorApplication

    ppProcedureCall procName args =
      let
        nameDoc = nameToDoc procName
        argsDoc =
          PP.align . PP.cat . PP.punctuate PP.space $ List.map ppArg args
      in
        if List.null args
        then nameDoc
        else nameDoc PP.<+> argsDoc

    ppArg = \case
      ArgPos NoExt a -> parensIfProcCall a (ppExpr a)
      ArgKw NoExt name a -> nameToDoc name PP.<> PP.colon PP.<+>
        parensIfProcCall a (ppExpr a)
      XArg xxArg -> absurd xxArg

    parensIfSemicolon = \case
      ExprProcCall NoExt (ProcCall NoExt (CommandIdOperator OpAndThen) _) -> PP.parens
      _ -> id

    parensIfProcCall = \case
      ExprProcCall NoExt (ProcCall NoExt _ args) | not (List.null args) -> PP.parens
      _ -> id

ppValue :: PrettyPrintValue components => Value components -> Doc
ppValue = ppExpr . inflate

text :: T.Text -> Doc
text = PP.text . T.unpack

nameToDoc :: Name -> Doc
nameToDoc = PP.text . TL.unpack . toLazyText . build

type PrettyPrintValue components =
  ( AllConstrained ComponentPrinter components
  , AllConstrained (ComponentInflate components) components
  )
