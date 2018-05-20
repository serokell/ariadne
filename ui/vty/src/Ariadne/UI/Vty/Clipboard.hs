-- | Clipboard operations.

module Ariadne.UI.Vty.Clipboard
       ( copySelected
       ) where

import Universum

import Control.Exception (Exception(..))
import System.Hclip (setClipboard)

import Ariadne.UI.Vty.Face

data NoSelection = NoSelection deriving (Show)

instance Exception NoSelection where
    displayException NoSelection = "Nothing is selected"

copySelected :: UiSelectedItem -> IO ()
copySelected UiNoSelection = throwM NoSelection
copySelected selection = setClipboard . toString $ formattedSelection
  where
    formattedSelection =
        case selection of
            UiNoSelection -> error "UiNoSelection is unreachable"
            UiSelectedWallet name -> name
            UiSelectedAccount name -> name
            UiSelectedAddress addr -> addr
