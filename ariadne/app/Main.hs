module Main where

import Prelude
import Control.Concurrent.Async

import Ariadne.UI
import Ariadne.AuxxBackend

main :: IO ()
main = do
  (uiFace, uiAction) <- createAriadneUI
  (auxxFace, auxxAction) <- createAuxxBackend
  race_ (uiAction auxxFace) (auxxAction uiFace)
