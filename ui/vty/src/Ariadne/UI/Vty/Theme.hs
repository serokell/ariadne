module Ariadne.UI.Vty.Theme where

import Brick
import Graphics.Vty

-- Fun facts about Vty attributes and Brick themes:
-- 1. The following rules work in Attr:
--      _ <> Default     = Default  -- defAttr
--      a <> KeepCurrent = a        -- currentAttr
-- 2. fg red = defAttr `withForeColor` red
-- 3. If we have `AttrMap def attrs`, then:
--      attrMapLookup ("foo" <> "bar") =
--        default <>
--        lookup "foo" attrs <>
--        lookup ("foo" <> "bar") attrs
--
-- So, the only way to define red foreground with whatever background
-- there was before, is to have `currentAttr` as the "default" attr
-- and have actual default style in map with explicit key.
--
-- Also, fun fact #4: hBorder and vBorder use hard-coded attribute key
-- "border"
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
