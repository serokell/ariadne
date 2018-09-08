module Ariadne.UI.Vty.Focus
     ( withFocusIndicator
     ) where

import qualified Brick as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Widget

withFocusIndicator :: WidgetName -> WidgetName -> Char -> Int -> B.Widget WidgetName -> B.Widget WidgetName
withFocusIndicator focus name char pad w
    | focus == [WidgetNameMenu] = add "focus.key" char
    | take (length name) focus == name = B.withAttr "focused" $ add "focus" '•'
    | otherwise = w
  where
    add attrName c = B.Widget (B.hSize w) (B.vSize w) $ do
      ctx <- B.getContext
      result <- B.render w
      let
        attrMap = ctx ^. B.ctxAttrMapL
        attr = B.attrMapLookup attrName attrMap
        width = result ^. B.imageL & V.imageWidth
        img = V.horizJoin (V.pad 0 pad 0 0 $ V.char attr c) (V.cropLeft (width - 1) $ result ^. B.imageL)
      return $ result & B.imageL .~ img
