module Ariadne.UI.Vty.Widget.Form.Checkbox
       ( initCheckboxWidget
       ) where

import Control.Lens (makeLensesWith)

import qualified Brick as B

import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.Util

data CheckboxWidgetState p =
  CheckboxWidgetState
    { checkboxWidgetTitle :: !Text
    , checkboxWidgetLens :: !(ReifiedLens' p Bool)
    }

makeLensesWith postfixLFields ''CheckboxWidgetState

initCheckboxWidget :: Text -> Lens' p Bool -> Widget p
initCheckboxWidget title lens =
  initWidget $ do
    setWidgetDrawWithFocused drawCheckboxWidget
    setWidgetHandleKey handleCheckboxWidgetKey
    setWidgetHandleMouseDown handleCheckboxWidgetMouseDown
    setWidgetState CheckboxWidgetState
      { checkboxWidgetTitle = title
      , checkboxWidgetLens = Lens lens
      }

drawCheckboxWidget :: Bool -> CheckboxWidgetState p -> WidgetDrawM (CheckboxWidgetState p) p WidgetDrawing
drawCheckboxWidget focused CheckboxWidgetState{..} = do
  widgetName <- getWidgetName
  checked <- viewWidgetLens checkboxWidgetLens
  return . singleDrawing $
    B.clickable widgetName $
    (if focused then B.withAttr "selected" else id) $
    B.hBox
      [ B.txt (if checked then "[X] " else "[ ] ")
      , B.txtWrap $ checkboxWidgetTitle
      ]  

handleCheckboxWidgetKey
  :: KeyboardEvent
  -> WidgetEventM (CheckboxWidgetState p) p WidgetEventResult
handleCheckboxWidgetKey key = if
  | key `elem` [KeyEnter, KeyChar ' '] -> do
      toggle
      return WidgetEventHandled
  | otherwise ->
      return WidgetEventNotHandled

handleCheckboxWidgetMouseDown
  :: B.Location
  -> WidgetEventM (CheckboxWidgetState p) p WidgetEventResult
handleCheckboxWidgetMouseDown _ = do
  toggle
  return WidgetEventHandled

toggle :: WidgetEventM (CheckboxWidgetState p) p ()
toggle = do
  CheckboxWidgetState{..} <- get
  useWidgetLens checkboxWidgetLens >>= assignWidgetLens checkboxWidgetLens . not
