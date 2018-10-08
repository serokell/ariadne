module Ariadne.Knit.Help
       ( generateKnitHelp
       ) where

import qualified Universum.Unsafe as Unsafe (tail)

import NType (AllConstrained, KnownSpine)

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Knit
import qualified Text.PrettyPrint.ANSI.Leijen as PP

generateKnitHelp :: forall components.
  ( AllConstrained (Knit.ComponentCommandProcs components) components
  , KnownSpine components)
  => Proxy components -> [PP.Doc]
generateKnitHelp _ =
  let
    procs = Knit.commandProcs @components
    sortedProcs = sortBy (\(Knit.SomeCommandProc p) (Knit.SomeCommandProc p') -> compare (Knit.cpName p) (Knit.cpName p')) procs
  in fmap (PP.text . toString) $ lines $ mkHelpMessage sortedProcs

commandHelp :: Knit.CommandProc components component -> Text
commandHelp Knit.CommandProc{..} =
    let
        parameters = Knit.getParameters cpArgumentConsumer
        name = \case
          Knit.CommandIdName n -> pretty n
          Knit.CommandIdOperator o -> op o
        op = \case
          Knit.OpUnit -> "()"
          Knit.OpAndThen -> "(;)"
        prefixes = name cpName : repeat (Text.replicate (Text.length $ name cpName) " ")
        helpLines = map (\l -> "-- " <> l <> "\n") $ leftAlign 40 cpHelp
        parameterLines =
            if null parameters
            then [""]
            else map parameterHelp parameters
        commandDesc = Text.intercalate "\n" $
            zipWith (\p h -> p <> " " <> h) prefixes parameterLines
    in
        Text.concat helpLines <> commandDesc

parameterHelp :: (Knit.Name, Knit.TypeName, Knit.SomeArgCardinality) -> Text
parameterHelp (name, tn, ac) = pretty name <> ": " <> withArgCardinality ac (withTypeName tn NeedWrap)

data NeedWrap = NeedWrap | DontNeedWrap

withArgCardinality :: Knit.SomeArgCardinality -> Text -> Text
withArgCardinality (Knit.SomeArgCardinality ac) = case ac of
    Knit.ArgCardSingle -> identity
    Knit.ArgCardOpt    -> (<> "?")
    Knit.ArgCardMany   -> (<> "*")
    Knit.ArgCardSome   -> (<> "+")

withTypeName :: Knit.TypeName -> NeedWrap -> Text
withTypeName (Knit.TypeName t) _ = t
withTypeName (Knit.TypeNameEither tn1 tn2) needWrap =
    case needWrap of
        DontNeedWrap -> t'
        NeedWrap     -> wrap t'
  where
    wrap t = "(" <> t <> ")"
    t' = withTypeName tn1 DontNeedWrap <> " | " <>
         withTypeName tn2 DontNeedWrap

mkHelpMessage :: [Knit.SomeCommandProc components] -> Text
mkHelpMessage cps =
    "Available commands:\n \n" <>
    mconcat (map (\(Knit.SomeCommandProc cp) -> commandHelp cp <> "\n \n") cps)

data Line = Line
  { lineWidth :: Int
  , lineWords :: NonEmpty Text
  }

initLine :: Text -> Line
initLine w = Line (Text.length w) (w :| [])

appendLine :: Line -> Line -> Line
appendLine line1 line2 =
  Line
    (lineWidth line1 + 1 + lineWidth line2)
    (lineWords line1 <>    lineWords line2)

nonEmptyTails :: NonEmpty a -> NonEmpty [a]
nonEmptyTails = NonEmpty.fromList . Unsafe.tail . tails . NonEmpty.toList

leftAlign :: Int -> Text -> [Text]
leftAlign desiredLineWidth =
    fmap mergeLine . groupWords . List.map initLine . words
  where
    groupWords :: [Line] -> [Line]
    groupWords = unfoldr (fmap @Maybe groupWords' . nonEmpty)

    groupWords' :: NonEmpty Line -> (Line, [Line])
    groupWords' ls =
        fromMaybe (head groupings) $
            last <$> nonEmpty goodGroupings
      where
        goodGroupings = NonEmpty.takeWhile (fits . fst) groupings
        groupings =
            NonEmpty.zip
                (NonEmpty.scanl1 appendLine ls)
                (nonEmptyTails ls)

    fits :: Line -> Bool
    fits line = lineWidth line <= desiredLineWidth

    mergeLine :: Line -> Text
    mergeLine = unwords . NonEmpty.toList . lineWords
