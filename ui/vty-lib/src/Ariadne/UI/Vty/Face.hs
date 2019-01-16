module Ariadne.UI.Vty.Face
       ( UiFeatures (..)
       , Vty
       , FrontendEvent (..)
       , UiCommandAction (..)
       , UiNewVersionEvent (..)
       , UiCurrency (..)
       , FrontendCommandEvent (..)
       , module CommonFace
       ) where

import Data.Version (Version)

import Ariadne.UI.Common.Face as CommonFace

-- | UI library settings for a particular currency implementation
-- Mostly boolean flags for enabled widgets
data UiFeatures = UiFeatures
  { featureStatus :: !Bool
  , featureExport :: !Bool
  , featureAccounts :: !Bool
  , featureTxHistory :: !Bool
  , featureSecretKeyName :: !Text  -- ^ "Secret key"/"Mnemonic"/etc
  }

data Vty

-- | Events as perceived by the UI. They will be generated from backend-specific
-- events in the 'Glue' module. They must be independent from the backends and
-- capture /what the UI can handle/, not what the backends can generate.
data instance FrontendEvent Vty
  = UiNewVersionEvent UiNewVersionEvent
  | UiCommandAction UiCommandAction

-- UI event triggered by REPL command
data UiCommandAction
  = UiCommandHelp
  | UiCommandLogs

data UiNewVersionEvent = UiNewVersion
  { nvVersion :: Version
  , nvUpdateURL :: Text
  }

data instance UiCurrency Vty
  = UiCurrency
  { getUiCurrency :: Text
  }
  deriving Eq

-- There are no Vty-speciic events yet
data instance FrontendCommandEvent Vty = NoEvents Void
  deriving Show
