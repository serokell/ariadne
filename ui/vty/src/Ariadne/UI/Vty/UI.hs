module Ariadne.UI.Vty.UI
       ( BrickName(..)
       ) where

import Universum

-- | Brick-specific ID for viewports, rendering cache, focus etc
data BrickName
  = BrickMenu
  | BrickTree
  | BrickWallet
  | BrickReplOutput
  | BrickReplInput
  | BrickHelp
  | BrickLogs
  deriving (Eq, Ord, Show)
