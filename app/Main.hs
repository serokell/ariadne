module Main where

import Control.Monad (void)

import Ariadne.WalletLayout (initialState, runBySt, menu,
                             focusRing, repl, Menu(..), St(..))
import Ariadne.ConfigForm (runConfigForm)
import Ariadne.Help (runHelp)
import Ariadne.Settings (runSettings)
import Brick.Widgets.Dialog (dialogSelection)
import Lens.Micro ((^.))


main :: IO ()
main = do
  runConfigForm
  d <- runBySt initialState
  let currentState = St (d ^. menu) (" ") (d ^. focusRing) (d ^. repl)
  let runLayout = void $ runBySt currentState
  case (dialogSelection $ currentState ^. menu) of
    Nothing -> runLayout
    Just m -> case m of
      Help -> do
        runHelp
        runLayout
      Exit -> return ()
      Configurations -> do
        runConfigForm
        runLayout
      Settings -> do
        runSettings
        runLayout
      _ -> runLayout
