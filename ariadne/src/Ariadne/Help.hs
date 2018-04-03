module Ariadne.Help where

import Universum
import IiExtras
import Text.PrettyPrint.ANSI.Leijen as PP
import qualified Knit

generateKnitHelp :: forall components.
  ( AllConstrained (Knit.ComponentCommandProcs components) components
  , KnownSpine components)
  => Proxy components -> [PP.Doc]
generateKnitHelp _execCtxs =
  let
    procs = Knit.commandProcs @components
    helps = fmap (\(Some cp) -> Knit.cpName cp) procs
  in fmap (PP.text . show) helps