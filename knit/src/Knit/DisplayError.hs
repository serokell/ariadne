module Knit.DisplayError
    ( ppArgumentError
    , ppEvalError
    , ppTypeError
    , ppTypeName
    , ppParseError
    , ppProcError
    , ppResolveErrors
    ) where


import Control.Applicative ((<|>))
import Control.Lens
import Data.Either
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Loc
import Data.Loc.Span (joinAsc)
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Earley (Report(..))
import Text.PrettyPrint.ANSI.Leijen
  (Doc, bold, char, comma, empty, hcat, indent, punctuate, red, squotes, vcat,
  yellow, (<$>), (<+>))

import Knit.Argument
import Knit.Eval
import Knit.Parser
import Knit.Prelude hiding (span, (<$>))
import Knit.Printer
import Knit.Syntax
import Knit.Tokenizer

highlight :: Doc -> Doc
highlight = bold . yellow

commandIdToDoc :: CommandId -> Doc
commandIdToDoc (CommandIdName name) = nameToDoc name
commandIdToDoc (CommandIdOperator opName) =
  case opName of
    OpAndThen -> "the and-then operator"
    OpUnit -> "()"

ppTypeName :: TypeName -> Doc
ppTypeName (TypeName name)         = text name
ppTypeName (TypeNameEither tn tn') = ppTypeName tn <+> char '|' <+> ppTypeName tn'

ppTypeError
  :: PrettyPrintValue components
  => TypeError components
  -> Doc
ppTypeError TypeError{..} =
        "Couldn't match expected type" <+> (highlight $ ppTypeName teExpectedType)
    <+> "with actual value"            <+> (highlight $ ppValue teActualValue)
    `mappend`  "!"

ppArgumentError :: ArgumentError -> Doc
ppArgumentError ae@ArgumentError{..} =
    if isEmptyArgumentError ae
    then empty
    else vcat $ missingKeysDoc <> irrelevantKeysDoc <> irrelevantPosDoc
  where
    setToDoc = hcat . punctuate comma . map (highlight . nameToDoc) . Set.toList
    missingKeysDoc =
        if null aeMissingKeys
        then []
        else ["Missing keys:" <+> setToDoc aeMissingKeys]
    irrelevantKeysDoc =
        if null aeIrrelevantKeys
        then []
        else ["Irrelevant keys:" <+> setToDoc aeIrrelevantKeys]
    irrelevantPosDoc =
        if aeIrrelevantPos == 0
        then []
        else ["Irrelevant positional arguments:" <+> (highlight . fromString . show) aeIrrelevantPos]

ppProcError :: PrettyPrintValue components => ProcError components -> Doc
ppProcError ProcError{..} = ppArgumentError peArgumentError <$> typeErrorsDoc
  where
    typeErrorsDoc =
        if null peTypeErrors
        then empty
        else "Following type errors occured:" <$>
             (indent 2 . hcat . map ppTypeError . Set.toList) peTypeErrors

ppEvalError :: PrettyPrintValue components => EvalError components -> Doc
ppEvalError (InvalidArguments name procError) =
        "Invalid arguments for" <+> (squotes . highlight . commandIdToDoc) name `mappend` ":"
    <$> indent 2 (ppProcError procError)

ppResolveErrors :: NonEmpty CommandId -> Doc
ppResolveErrors names =
    "Commands not available:" <+>
    hcat (punctuate (text ", ") . map commandIdToDoc $ NE.toList names)

renderLine :: Int -> Int -> Text -> Doc
renderLine start end str = text str
  <$> (bold . red . indent start . hcat . replicate (end - start) $ char '^')

renderFullLine :: Text -> Doc
renderFullLine str = renderLine 0 (T.length str) str

ppParseError
  :: AllConstrained ComponentPrinter components
  => ParseError components
  -> Doc
ppParseError (ParseError str (Report {..})) =
      "Parse error at" <+> text (fromString (show span))
  <$> "Unexpected" <+> unconsumedDesc `mappend` ", expected"
  <+> hcat (punctuate (text ", or ") $ map text expected)
  <$> renderLines
  where
    unconsumedDesc = maybe "end of input" ppToken . listToMaybe . fmap snd $ unconsumed
    strLines = nonEmpty $ take spanLines . drop (spanLineStart - 1) $ T.lines str
    renderLines = case strLines of
        Nothing ->
            -- This can only happen if megaparsec's 'getPosition' somehow
            -- returned line number bigger than the actual amount of
            -- lines in the input text.
            error "ppParseError/renderLines: span is outside of the input bounds"
        Just (line :| []) -> renderLine (spanColumnStart - 1) (spanColumnEnd - 1) line
        Just (line :| (line' : ls')) ->  let ls = line' :| ls' in
                renderLine (spanColumnStart - 1) (T.length line) line
            <$> (vcat . map renderFullLine . NE.init $ ls)
            <$> renderLine 0 (spanColumnEnd - 1) (NE.last ls)

    spanColumnStart = fromIntegral . toNat . locColumn . spanStart $ span
    spanColumnEnd   = fromIntegral . toNat . locColumn . spanEnd   $ span
    spanLineStart   = fromIntegral . toNat . locLine   . spanStart $ span
    spanLineEnd     = fromIntegral . toNat . locLine   . spanEnd   $ span
    spanLines = spanLineEnd - spanLineStart + 1

    strEndLoc = loc 1 (fromInteger . toInteger . T.length $ str)

    addColumn col l = loc (locLine l) (locColumn l + col)

    isTokenUnknown = isRight . matching _TokenUnknown
    unknownSpans :: [Span]
    unknownSpans = map (getSSpan . fst) . takeWhile (isTokenUnknown . snd) $ unconsumed
    span = NE.head $
        case nonEmpty (joinAsc unknownSpans) <|> nonEmpty (map (getSSpan . fst) unconsumed) of
            Nothing -> spanFromTo strEndLoc (addColumn 1 strEndLoc) :|[]
            Just x  -> x
