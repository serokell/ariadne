module Main where

import Ariadne.WalletLayout (initialState, runBySt)
import Ariadne.ConfigForm (runConfigForm)

main :: IO ()
main = do
  runConfigForm
  -- TODO: if config form is non-empty then runBySt initialState else runConfigForm--
  runBySt initialState
