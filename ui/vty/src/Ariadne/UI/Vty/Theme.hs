module Ariadne.UI.Vty.Theme where

import Brick
import Data.Semigroup
import Graphics.Vty

defaultAttrMap :: AttrMap
defaultAttrMap = attrMap
  (white `on` black)
  [ ("menu", black `on` white)
  , ("menu" <> "key", fg red)
  , ("menu" <> "selected", white `on` black)
  , ("menu" <> "selected" <> "key", fg red)
  ]
