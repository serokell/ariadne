module Ariadne.UI.Vty.Theme where

import Brick
import Graphics.Vty

defaultAttrMap :: AttrMap
defaultAttrMap = attrMap
  currentAttr
  [ ("default", white `on` black)
  , ("border", white `on` black)
  , ("focused", white `on` black `withStyle` bold)
  , ("selected", currentAttr `withStyle` standout)
  , ("menu", black `on` white)
  , ("menu.selected", white `on` black)
  , ("menu.key", currentAttr `withForeColor` red)
  , ("status", black `on` white)
  , ("status.content", currentAttr `withForeColor` blue)
  ]
