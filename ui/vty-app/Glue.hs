-- | Glue code between the frontend and the backends.

module Glue
       ( vtyToUiCurrency
       , putUpdateEventToUI
       , module CommonGlue
       ) where

import Ariadne.UI.Vty.Face
import Ariadne.UI.Common.Glue as CommonGlue
import Data.Version (Version)

import qualified Ariadne.Cardano.Knit as Knit

vtyToUiCurrency :: Text -> Knit.Currency -> UiCurrency Vty
vtyToUiCurrency amount unit = UiCurrency . Just $ amount <> " " <> show unit

putUpdateEventToUI :: UiFace Vty -> Version -> Text -> IO ()
putUpdateEventToUI UiFace{..} ver updateURL = putUiEvent $ UiFrontendEvent $ UiNewVersionEvent $ UiNewVersion ver updateURL
