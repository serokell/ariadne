module Ariadne.UI.Vty.Theme where

import Brick
import Data.Semigroup
import Graphics.Vty

defaultAttrMap :: AttrMap
defaultAttrMap = attrMap
  currentAttr
  [ ("default", white `on` black)
  , ("focused", white `on` black `withStyle` bold)
  , ("menu", black `on` white)
  , ("menu.selected", white `on` black)
  , ("menu.key", currentAttr `withForeColor` red)
  , ("status", black `on` white)
  , ("status" <> "content", fg blue)
  ]
