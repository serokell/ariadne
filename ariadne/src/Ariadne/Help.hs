module Ariadne.Help where

import Universum
import Text.PrettyPrint.ANSI.Leijen as PP

generateKnitHelp :: IO [PP.Doc]
generateKnitHelp = return [PP.text "Cardano documentation goes here..."]