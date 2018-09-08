module Ariadne.UI.Vty.Widget.Form.Button
       ( initButtonWidget
       ) where

import Control.Lens (makeLensesWith)

import qualified Brick as B

import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.Util

data ButtonWidgetState =
  ButtonWidgetState
    { buttonWidgetTitle :: !Text
    }

makeLensesWith postfixLFields ''ButtonWidgetState

initButtonWidget :: Text -> Widget p
initButtonWidget title =
  initWidget $ do
    setWidgetDrawWithFocused drawButtonWidget
    setWidgetHandleKey handleButtonWidgetKey
    setWidgetHandleMouseDown handleButtonWidgetMouseDown
    setWidgetState ButtonWidgetState
      { buttonWidgetTitle = title
      }

drawButtonWidget :: Bool -> ButtonWidgetState -> WidgetDrawM ButtonWidgetState p (B.Widget WidgetName)
drawButtonWidget focused ButtonWidgetState{..} = do
  widgetName <- getWidgetName
  return $
    B.clickable widgetName $
    (if focused then B.withAttr "selected" else id) $
    B.txt $
    "[ " <> buttonWidgetTitle <> " ]"

handleButtonWidgetKey
  :: KeyboardEvent
  -> WidgetEventM ButtonWidgetState p WidgetEventResult
handleButtonWidgetKey key = if
  | key `elem` [KeyEnter, KeyChar ' '] -> do
      widgetEvent WidgetEventButtonPressed
      return WidgetEventHandled
  | otherwise ->
      return WidgetEventNotHandled

handleButtonWidgetMouseDown
  :: B.Location
  -> WidgetEventM ButtonWidgetState p WidgetEventResult
handleButtonWidgetMouseDown _ = do
  widgetEvent WidgetEventButtonPressed
  return WidgetEventHandled
