module Ariadne.Knit.Help (generateKnitHelp) where

import qualified Data.Text as T
import IiExtras
import qualified Knit
import Pos.Util.Justify (leftAlign)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Universum

generateKnitHelp :: forall components.
  ( AllConstrained (Knit.ComponentCommandProcs components) components
  , KnownSpine components)
  => Proxy components -> [PP.Doc]
generateKnitHelp _ =
  let
    procs = Knit.commandProcs @components
  in fmap (PP.text . T.unpack) $ T.lines $ mkHelpMessage procs

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
        prefixes = name cpName : repeat (T.replicate (T.length $ name cpName) " ")
        helpLines = map (\l -> "-- " <> l <> "\n") $ leftAlign 40 cpHelp
        parameterLines =
            if null parameters
            then [""]
            else map parameterHelp parameters
        commandDesc = T.intercalate "\n" $
            zipWith (\p h -> p <> " " <> h) prefixes parameterLines
    in
        T.concat helpLines <> commandDesc

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

mkHelpMessage :: [Some (Elem components) (Knit.CommandProc components)] -> Text
mkHelpMessage cps =
    "Available commands:\n \n" <>
    mconcat (map (\(Some cp) -> commandHelp cp <> "\n \n") cps)
