module Main where

import Ariadne.WalletLayout (app, initialState)
import Control.Monad (void)
import Brick.Main (defaultMain)

main :: IO ()
main = void $ defaultMain app initialState
