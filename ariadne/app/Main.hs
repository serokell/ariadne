module Main where

import Prelude
import Control.Concurrent.Async
import Data.Vinyl.Core

import Ariadne.UI
import Ariadne.Face
import Ariadne.Knit.Backend
import Ariadne.CardanoBackend

import qualified Knit
import qualified Ariadne.Knit.Cardano as Knit

main :: IO ()
main = do
  (uiFace, uiAction) <- createAriadneUI
  (knitFace, knitAction) <- createKnitBackend
  (cardanoCtx, cardanoAction) <- createCardanoBackend
  let
    putKnitEvent ev = putUiEvent uiFace (UiKnitEvent ev)
    knitExecContext =
      Knit.CoreExecCtx :&
      Knit.CardanoExecCtx cardanoCtx :&
      RNil
  uiAction knitFace `race_`
    knitAction knitExecContext putKnitEvent `race_`
    cardanoAction
