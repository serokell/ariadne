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
import Knit.Syntax
import Knit.Tokenizer
import Knit.Value
import Knit.Prelude

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
  TokenUnknown (UnknownChar c) -> text ("character '" <> T.singleton c <> "'")

ppExpr
  :: AllConstrained ComponentPrinter components
  => Expr CommandName components
  -> Doc
ppExpr =
  \case
    ExprLit l -> ppLit l
    ExprProcCall p -> ppProcCall p
  where
    ppProcCall (ProcCall commandName args) =
      case commandName of
        OperatorName opName -> ppOperatorCall opName args
        ProcedureName procName -> ppProcedureCall procName args

    ppOperatorCall OpUnit [] = text ""
    ppOperatorCall OpSemicolon [ArgPos a, ArgPos b] =
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
      ArgPos a -> parensIfProcCall a (ppExpr a)
      ArgKw name a -> nameToDoc name PP.<> PP.colon PP.<+>
        parensIfProcCall a (ppExpr a)

    parensIfSemicolon = \case
      ExprProcCall (ProcCall (OperatorName OpSemicolon) _) -> PP.parens
      _ -> id

    parensIfProcCall = \case
      ExprProcCall (ProcCall _ args) | not (List.null args) -> PP.parens
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
