module Knit.DisplayError
    ( ppArgumentError
    , ppEvalError
    , ppTypeError
    , ppTypeName
    , ppParseError
    , ppProcError
    , ppResolveErrors
    , renderAuxxDoc
    , text
    , nameToDoc
    ) where

import Prelude hiding ((<$>), span)

import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Text.PrettyPrint.ANSI.Leijen
import qualified Data.Set as Set
import Data.Either
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Monoid ((<>))
import Data.Maybe
import Data.String (fromString)
import Control.Lens
import Control.Applicative ((<|>))
import Data.Loc
import Data.Loc.Span (joinAsc)
import Data.Text.Buildable (build)
import Data.Text.Lazy.Builder (toLazyText)
import Text.Earley (Report (..))
import Text.PrettyPrint.ANSI.Leijen
  (Doc, bold, char, comma, empty, hcat, indent,
   punctuate, red, squotes, vcat, yellow, (<$>), (<+>))

import Knit.Argument
import Knit.Eval
import Knit.Tokenizer
import Knit.Name
import Knit.Parser
import Knit.Value

highlight :: Doc -> Doc
highlight = bold . yellow

text :: Text -> Doc
text = Text.PrettyPrint.ANSI.Leijen.text . unpack

nameToDoc :: Name -> Doc
nameToDoc = Text.PrettyPrint.ANSI.Leijen.text . TL.unpack . toLazyText . build

ppTypeName :: TypeName -> Doc
ppTypeName (TypeName name)         = text name
ppTypeName (TypeNameEither tn tn') = ppTypeName tn <+> char '|' <+> ppTypeName tn'

ppTypeError :: Show (Value components) => TypeError components -> Doc
ppTypeError TypeError{..} =
        "Couldn't match expected type" <+> (highlight $ ppTypeName teExpectedType)
    <+> "with actual value"            <+> (highlight $ fromString (show teActualValue)) -- TODO: ppr
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

ppProcError :: Show (Value components) => ProcError components -> Doc
ppProcError ProcError{..} = ppArgumentError peArgumentError <$> typeErrorsDoc
  where
    typeErrorsDoc =
        if null peTypeErrors
        then empty
        else "Following type errors occured:" <$>
             (indent 2 . hcat . map ppTypeError . Set.toList) peTypeErrors

ppEvalError :: Show (Value components) => EvalError components -> Doc
ppEvalError (InvalidArguments name procError) =
        "Invalid arguments for" <+> (squotes . highlight . nameToDoc) name `mappend` ":"
    <$> indent 2 (ppProcError procError)

ppResolveErrors :: NonEmpty Name -> Doc
ppResolveErrors names =
    "Commands not available:" <+>
    hcat (punctuate (text ", ") . map nameToDoc $ NE.toList names)

renderLine :: Int -> Int -> Text -> Doc
renderLine start end str = text str
  <$> (bold . red . indent start . hcat . replicate (end - start) $ char '^')

renderFullLine :: Text -> Doc
renderFullLine str = renderLine 0 (T.length str) str

ppParseError :: Show (Token components) => ParseError components -> Doc
ppParseError (ParseError str (Report {..})) =
      "Parse error at" <+> text (fromString (show span))
  <$> "Unexpected" <+> text unconsumedDesc `mappend` ", expected"
  <+> hcat (punctuate (text ", or ") $ map text expected)
  <$> renderLines
  where
    unconsumedDesc = maybe "end of input" (fromString . show) . listToMaybe . fmap snd $ unconsumed
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
    unknownSpans = map fst . takeWhile (isTokenUnknown . snd) $ unconsumed
    span = NE.head $
        case nonEmpty (joinAsc unknownSpans) <|> nonEmpty (map fst unconsumed) of
            Nothing -> spanFromTo strEndLoc (addColumn 1 strEndLoc) :|[]
            Just x  -> x

renderAuxxDoc :: Doc -> Text
renderAuxxDoc = fromString . show -- it's fine
