module Ariadne.UI.Vty.UI
       ( BrickName(..)
       ) where

import Universum

-- | Brick-specific ID for viewports, rendering cache, focus etc
data BrickName
  = BrickMenu
  | BrickWalletTree
  | BrickWalletPane
  | BrickReplOutput
  | BrickReplInput
  | BrickHelp
  | BrickLogs
  deriving (Eq, Ord, Show)
