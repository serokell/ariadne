module Main where

import Control.Concurrent.Async
import IiExtras
import Prelude

import Ariadne.Cardano.Backend
import Ariadne.Glue
import Ariadne.Knit.Backend
import Ariadne.UI.Vty
import Ariadne.UI.Vty.Face
import Ariadne.Help

import qualified Ariadne.Cardano.Knit as Knit
import qualified Knit

main :: IO ()
main = do
  (uiFace, uiAction) <- createAriadneUI
  (knitFace, knitAction) <- createKnitBackend
  (Nat runCardanoMode, cardanoAction) <- createCardanoBackend
  let
    -- The list of components is inferred from this list.
    knitExecContext =
      Knit.CoreExecCtx :&
      Knit.CardanoExecCtx runCardanoMode :&
      RNil
    helpData = generateKnitHelp $ relemsproxy knitExecContext
  putUiEvent uiFace $ UiHelpUpdateData helpData
  uiAction (knitFaceToUI knitFace) `race_`
    knitAction knitExecContext (putKnitEventToUI uiFace) `race_`
    cardanoAction (putLogMessage uiFace)
