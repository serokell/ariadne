module Ariadne.UI.Vty.Widget.Form.Checkbox
       ( TypeOfCheckField(..)
       , initCheckboxWidget
       ) where

import Control.Lens (makeLensesWith)

import qualified Brick as B

import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.Util

data TypeOfCheckField = Check | Expand

data CheckboxWidgetState p =
  CheckboxWidgetState
    { checkboxWidgetTitle :: !(ReifiedLens' p Text)
    , checkboxWidgetLens  :: !(ReifiedLens' p Bool)
    , checkBoxWidgetType  :: !TypeOfCheckField
    }

makeLensesWith postfixLFields ''CheckboxWidgetState

initCheckboxWidget :: TypeOfCheckField -> Lens' p Text -> Lens' p Bool -> Widget p
initCheckboxWidget typeofCheckField lensTitle lensChecked =
  initWidget $ do
    setWidgetDrawWithFocused drawCheckboxWidget
    setWidgetHandleKey handleCheckboxWidgetKey
    setWidgetHandleMouseDown handleCheckboxWidgetMouseDown
    setWidgetState CheckboxWidgetState
      { checkboxWidgetTitle = Lens lensTitle
      , checkboxWidgetLens = Lens lensChecked
      , checkBoxWidgetType = typeofCheckField
      }

drawCheckboxWidget :: Bool -> CheckboxWidgetState p -> WidgetDrawM (CheckboxWidgetState p) p WidgetDrawing
drawCheckboxWidget focused CheckboxWidgetState{..} = do
  widgetName <- getWidgetName
  title <- viewWidgetLens checkboxWidgetTitle
  checked <- viewWidgetLens checkboxWidgetLens
  let checkField = case checkBoxWidgetType of
          Check  -> if checked then "[X] " else "[ ] "
          Expand -> if checked then "[-] " else "[+] "
  return . singleDrawing $
    B.clickable widgetName $
    (if focused then B.withAttr "selected" else id) $
    B.hBox
      [ B.txt checkField
      , B.txtWrap title
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
  widgetEvent WidgetEventCheckboxToggled
  CheckboxWidgetState{..} <- getWidgetState
  useWidgetLens checkboxWidgetLens >>= assignWidgetLens checkboxWidgetLens . not
