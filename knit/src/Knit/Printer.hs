module Knit.Printer where

import Data.List as List
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Buildable (build)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)

import Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Knit.Inflate
import Knit.Name
import Knit.Prelude
import Knit.Syntax
import Knit.Tokenizer
import Knit.Value

class ComponentPrinter component where
  componentPpLit :: ComponentLit component -> Doc
  componentPpToken :: ComponentToken component -> Doc

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
