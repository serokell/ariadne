module Main where

import Prelude
import Control.Concurrent.Async
import Data.Vinyl.Core

import Ariadne.UI
import Ariadne.Face
import Ariadne.Knit.Backend
import Ariadne.Knit.Cardano (ComponentExecContext(CardanoExecCtx))
import Ariadne.CardanoBackend
import Knit.Core (ComponentExecContext(CoreExecCtx))

main :: IO ()
main = do
  (uiFace, uiAction) <- createAriadneUI
  (knitFace, knitAction) <- createKnitBackend
  (cardanoCtx, cardanoAction) <- createCardanoBackend
  let
    putKnitEvent ev = putUiEvent uiFace (UiKnitEvent ev)
    knitExecContext =
      CoreExecCtx :&
      CardanoExecCtx cardanoCtx :&
      RNil
  uiAction knitFace `race_`
    knitAction knitExecContext putKnitEvent `race_`
    cardanoAction
