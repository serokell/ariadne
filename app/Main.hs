module Main where

import Control.Monad (void)

import Ariadne.WalletLayout (initialState, runBySt, menu, Menu(..))
import Ariadne.ConfigForm (runConfigForm)
import Ariadne.Help (runHelp)
import Brick.Widgets.Dialog (dialogSelection)
import Lens.Micro ((^.))


main :: IO ()
main = do
  runConfigForm
  d <- runBySt initialState
  case (dialogSelection $ d ^. menu) of
    Nothing -> void (return d)
    Just m -> case m of
      Help -> do
        runHelp
        void $ runBySt initialState
      Exit -> return ()
      Configurations -> do
        runConfigForm
        void $ runBySt initialState
      _ -> void (return d)
