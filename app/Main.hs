module Main where

import Ariadne.WalletLayout (initialState, runBySt)
import Ariadne.ConfigForm (runConfigForm)

main :: IO ()
main = do
  runConfigForm
  runBySt initialState
