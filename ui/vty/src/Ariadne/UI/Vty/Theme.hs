module Ariadne.UI.Vty.Theme where

import Brick
import Data.Semigroup
import Graphics.Vty

defaultAttrMap :: AttrMap
defaultAttrMap = attrMap
  (white `on` black)
  [ ("focused", white `on` black `withStyle` bold)
  , ("menu", black `on` white)
  , ("menu" <> "key", fg red)
  , ("menu" <> "selected", white `on` black)
  , ("menu" <> "selected" <> "key", fg red)
  , ("status", black `on` white)
  , ("status" <> "content", fg blue)
  ]
