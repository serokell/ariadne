module Main where

import Prelude
import Control.Concurrent.Async
import Data.Vinyl.Core

import Ariadne.UI
import Ariadne.Knit.Backend
import Ariadne.Cardano.Backend
import Ariadne.Glue

import qualified Knit
import qualified Ariadne.Cardano.Knit as Knit

main :: IO ()
main = do
  (uiFace, uiAction) <- createAriadneUI
  (knitFace, knitAction) <- createKnitBackend
  (cardanoCtx, cardanoAction) <- createCardanoBackend
  let
    -- The list of components is inferred from this list.
    knitExecContext =
      Knit.CoreExecCtx :&
      Knit.CardanoExecCtx cardanoCtx :&
      RNil
  uiAction (knitFaceToUI knitFace) `race_`
    knitAction knitExecContext (putKnitEventToUI uiFace) `race_`
    cardanoAction
