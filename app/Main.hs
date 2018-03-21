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

{-
import Prelude
import Control.Monad (void)

import UI.Wallet (initialAppState, runBySt, menu, focusRing, repl, wallets
                 , clicked, lastReportedClick, Menu(..), AppState(..)
                 )
import UI.Config (runConfigForm)
import UI.Help (runHelp)
import Brick.Widgets.Dialog (dialogSelection)
import Lens.Micro ((^.))

main :: IO ()
main = do
  runConfigForm
  d <- runBySt initialAppState
  let currentState = AppState  
                     (d ^. menu) 
                     (" ") 
                     (d ^. focusRing) 
                     (d ^. repl) 
                     (d ^. wallets)
                     (d ^. clicked)
                     (d ^. lastReportedClick)
  let runLayout = void $ runBySt currentState
  case (dialogSelection $ currentState ^. menu) of
    Nothing -> runLayout
    Just m -> case m of
      Exit -> return ()
      Help -> do
        runHelp
        runLayout
      Config -> do
        runConfigForm
        runLayout
      _ -> runLayout
-}
