module Knit.Printer where

import Data.List as List
import Data.Monoid
import Data.Semigroup (Option(..))
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

spanBetween :: SSpan -> SSpan -> SSpan
spanBetween (SSpan a) (SSpan b) = SSpan $ L.spanFromTo (L.end a) (L.start b)

spanToDoc :: SSpan -> Doc
spanToDoc (SSpan s) =
  if L.locLine (L.start s) == L.locLine (L.end s)
    then PP.string $ replicate (toInt $ L.locColumn (L.end s) - L.locColumn (L.start s)) ' '
    else PP.string
      $ replicate (toInt $ L.locLine (L.end s) - L.locLine (L.start s)) '\n'
      ++ replicate (toInt $ L.locColumn (L.end s) - 1) ' '
  where
    toInt :: L.ToNat n => n -> Int
    toInt = fromIntegral . L.toNat

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
            (PP.lparen <> spanToDoc (fst l `spanBetween` sp))
            (spanToDoc (sp `spanBetween` fst r) <> PP.rparen)
          Nothing -> Decorated
            (text "")
            (PP.parens $ spanToDoc $ fst l `spanBetween` fst r)
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
      ProcCall NoExt cmd <$> argGo (Option $ fmap fst tok) args

    argGo
      :: Option SSpan
      -> [Arg ParseTreeExt (Expr ParseTreeExt CommandId components)]
      -> (Option SSpan, [Arg DecoratorExt (Expr DecoratorExt CommandId components)])
    argGo (Option Nothing) (_ : _) = error "procedure with empty name has arguments"
    argGo prevSp [] = (prevSp, [])
    argGo prevSp'@(Option (Just prevSp)) (arg : args) =
      case arg of
        XArg xxArg -> absurd xxArg
        ArgPos NoExt a ->
          let
            decoratedArg = parseTreeToDecoratedTree a
            newSp = prevSp' <> fst decoratedArg
            dPrefix =
              case getOption $ fst decoratedArg of
                Just argSp -> spanToDoc $ prevSp `spanBetween` argSp
                Nothing -> PP.text ""
            dPostfix = PP.text ""
            dItem = ArgPos NoExt (snd decoratedArg)
          in
            (XArg Decorated{..} :) <$> argGo newSp args
        ArgKw nameTok name a ->
          let
            decoratedArg = parseTreeToDecoratedTree a
            newSp = prevSp' <> Option (Just (fst nameTok)) <> fst decoratedArg
            decorateExpr =
              case getOption $ fst decoratedArg of
                Nothing -> id
                Just exprSp ->
                  XExpr . Decorated
                    (spanToDoc $ fst nameTok `spanBetween` exprSp)
                    (text "")
            dPrefix = spanToDoc $ prevSp `spanBetween` fst nameTok
            dPostfix = PP.text ""
            dItem = ArgKw NoExt name $ decorateExpr $ snd decoratedArg
          in
            (XArg Decorated{..} :) <$> argGo newSp args

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

    ppOperatorCall OpUnit [] = text ""
    ppOperatorCall OpAndThen [ArgPos NoExt a, ArgPos NoExt b] =
      (parensIfSemicolon a (ppExpr a) <> PP.char ';') PP.<$> ppExpr b
    ppOperatorCall _ _ =
      error "Core invariant violated: invalid operator application"

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
