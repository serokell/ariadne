module Ariadne.UI.Vty.Widget.Menu
       ( MenuWidgetElem(..)
       , initMenuWidget
       ) where

import Control.Lens (makeLensesWith)
import Data.Char (toLower)
import Data.Maybe (fromJust)

import qualified Brick as B
import qualified Brick.Focus as B
import qualified Brick.Widgets.Center as B
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.Util

data MenuWidgetElem a =
  MenuWidgetElem
    { menuWidgetElemSelector :: !a
    , menuWidgetElemText :: !Text
    , menuWidgetElemKey :: !Char
    }

data MenuWidgetState p a =
  MenuWidgetState
    { menuWidgetElems :: ![MenuWidgetElem a]
    , menuWidgetFocusRing :: !(B.FocusRing a)
    , menuWidgetSelectionLens :: ReifiedLens' p a
    }

makeLensesWith postfixLFields ''MenuWidgetState

initMenuWidget
  :: Eq a
  => NonEmpty (MenuWidgetElem a)
  -> Lens' p a
  -> Widget p
initMenuWidget xs lens =
  initWidget $ do
    setWidgetDrawWithFocused drawMenuWidget
    setWidgetHandleMouseDown handleMenuWidgetMouseDown
    setWidgetHandleKey handleMenuWidgetKey
    setWidgetState MenuWidgetState
      { menuWidgetElems = NonEmpty.toList xs
      , menuWidgetFocusRing = B.focusRing $ menuWidgetElemSelector <$> NonEmpty.toList xs
      , menuWidgetSelectionLens = Lens lens
      }

drawMenuWidget :: Eq a => Bool -> MenuWidgetState p a -> WidgetDrawM (MenuWidgetState p a) p (B.Widget WidgetName)
drawMenuWidget focused MenuWidgetState{..} = do
  widgetName <- getWidgetName
  selection <- viewWidgetLens menuWidgetSelectionLens

  return $
    B.withAttr "menu" $
    B.hCenter $
    B.clickable widgetName $
    B.Widget
      { B.hSize = B.Fixed
      , B.vSize = B.Fixed
      , B.render = render selection
      }
  where
    render selection = do
      rdrCtx <- B.getContext

      let
        defAttr = rdrCtx ^. B.attrL
        attrMap = rdrCtx ^. B.ctxAttrMapL

        drawElem menuElem = V.horizCat
          [ V.text' elemAttr $ " " <> beforeKey
          , V.text' keyAttr $ T.singleton key
          , V.text' elemAttr $ afterKey <> " "
          ]
          where
            elemAttr = if selection == menuWidgetElemSelector menuElem
              then defAttr <> B.attrMapLookup "menu.selected" attrMap
              else defAttr
            keyAttr = if focused
              then elemAttr <> B.attrMapLookup "menu.key" attrMap
              else elemAttr
            elemText = menuWidgetElemText menuElem
            elemKey = menuWidgetElemKey menuElem
            (beforeKey, atKey) = T.break ((== elemKey) . toLower) elemText
            (key, afterKey)
              | T.null atKey = (elemKey, atKey)
              | toLower (T.head atKey) == elemKey = (T.head atKey, T.tail atKey)
              | otherwise = (elemKey, atKey)

        img =
          V.horizCat $
          intersperse (V.text' defAttr " ") $
          drawElem <$> menuWidgetElems

      return $
        B.emptyResult
          & B.imageL .~ img

handleMenuWidgetMouseDown
  :: B.Location
  -> WidgetEventM (MenuWidgetState p a) p WidgetEventResult
handleMenuWidgetMouseDown (B.Location (col, _)) = do
    MenuWidgetState{..} <- get
    whenJust (colToSelection menuWidgetElems col) (assignWidgetLens menuWidgetSelectionLens)
    widgetEvent WidgetEventMenuSelected
    return WidgetEventHandled
  where
    colToSelection :: [MenuWidgetElem a] -> Int -> Maybe a
    colToSelection [] _ = Nothing
    colToSelection (MenuWidgetElem{..}:els) col'
      | col' < 1 = Nothing -- 1-char space between items
      | col' <= w = Just menuWidgetElemSelector
      | otherwise = colToSelection els (col' - w - 1)
      where
        -- Width of menu item, including 1-char padding on both sides
        w = 2 + T.length menuWidgetElemText

handleMenuWidgetKey
  :: Eq a
  => KeyboardEvent
  -> WidgetEventM (MenuWidgetState p a) p WidgetEventResult
handleMenuWidgetKey key = do
  MenuWidgetState{..} <- get
  let
    rotate dir = do
      selection <- useWidgetLens menuWidgetSelectionLens
      assignWidgetLens menuWidgetSelectionLens $
        fromJust . B.focusGetCurrent . dir . B.focusSetCurrent selection $ menuWidgetFocusRing
      return WidgetEventHandled
    done = do
      widgetEvent WidgetEventMenuSelected
      return WidgetEventHandled
  case key of
    KeyChar ' ' -> done
    KeyEnter -> done
    KeyLeft -> rotate B.focusPrev
    KeyRight -> rotate B.focusNext
    _ -> return WidgetEventNotHandled
