module Ariadne.UI.LayerName where

import Prelude

data LayerName
  = LayerConfig
  | LayerHelp
  | LayerWallet
  deriving (Eq, Ord, Show)
