module Ariadne.UI.Vty.Widget.Form.List
       ( initListWidget
       ) where

import Universum

import Control.Lens (makeLensesWith, (%=), (.=))

import qualified Brick as B

import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.Util

data ListWidgetState p a =
  ListWidgetState
    { listWidgetLocation :: !Int
    , listWidgetItemsGetter :: !(p -> [a])
    , listWidgetDrawItem :: !(Bool -> a -> B.Widget WidgetName)
    }

makeLensesWith postfixLFields ''ListWidgetState

initListWidget
  :: (p -> [a])
  -> (Bool -> a -> B.Widget WidgetName)
  -> Widget p
initListWidget itemsGetter drawItem =
  initWidget $ do
    setWidgetDrawWithFocused drawListWidget
    setWidgetHandleKey handleListWidgetKey
    setWidgetHandleMouseDown handleListWidgetMouseDown
    setWidgetState ListWidgetState
      { listWidgetLocation = 0
      , listWidgetItemsGetter = itemsGetter
      , listWidgetDrawItem = drawItem
      }

drawListWidget :: Bool -> ListWidgetState p a -> WidgetDrawM (ListWidgetState p a) p (B.Widget WidgetName)
drawListWidget focused ListWidgetState{..} = do
  widgetName <- getWidgetName
  items <- listWidgetItemsGetter <$> lift ask
  let
    location = clampToList items listWidgetLocation
    drawItem (idx, item) = listWidgetDrawItem (focused && idx == location) item
  return $
    B.clickable widgetName $
    B.vBox $
    map drawItem $ zip [0..] items

handleListWidgetKey
  :: KeyboardEvent
  -> WidgetEventM (ListWidgetState p a) p WidgetEventResult
handleListWidgetKey key
  | key `elem` [KeyEnter, KeyChar ' '] = do
      ListWidgetState{..} <- get
      parentState <- lift $ lift get
      let items = listWidgetItemsGetter parentState
      widgetEvent $ WidgetEventListSelected $ clampToList items listWidgetLocation
      return WidgetEventHandled
  | KeyUp <- key = do
      listWidgetLocationL %= max 0 . pred . max 0
      return WidgetEventHandled
  | KeyDown <- key = do
      ListWidgetState{..} <- get
      parentState <- lift $ lift get
      let items = listWidgetItemsGetter parentState
      listWidgetLocationL %= clampToList items . succ . clampToList items
      return WidgetEventHandled
  | otherwise = do
      return WidgetEventNotHandled

handleListWidgetMouseDown
  :: B.Location
  -> WidgetEventM (ListWidgetState p a) p WidgetEventResult
handleListWidgetMouseDown (B.Location (_, row)) = do
  listWidgetLocationL .= row
  widgetEvent $ WidgetEventListSelected row
  return WidgetEventHandled

clampToList :: [a] -> Int -> Int
clampToList xs = max 0 . min (length xs - 1)
