module Ariadne.UI.Vty.Widget.Status where

import Control.Lens (makeLensesWith, (.=))
import Data.List as List
import Data.Text as Text
import Universum

import qualified Brick as B
import qualified Brick.Widgets.Border as B

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Util

data StatusWidgetState =
  StatusWidgetState
    { statusWidgetTipHeaderHash :: Text
    , statusWidgetTipSlot :: Text
    , statusWidgetSlot :: Text
    }

makeLensesWith postfixLFields ''StatusWidgetState

initStatusWidget :: StatusWidgetState
initStatusWidget = StatusWidgetState
    { statusWidgetTipHeaderHash = "<unknown>"
    , statusWidgetTipSlot = "<unknown>"
    , statusWidgetSlot = "<unknown>"
    }

drawStatusWidget
  :: StatusWidgetState
  -> B.Widget name
drawStatusWidget statusWidgetState =
  B.padTop (B.Pad 1) $
    B.updateAttrMap (B.mapAttrName "status" B.borderAttr) $
    B.withAttr "status" $
    B.vLimit 1 $
    B.padRight B.Max $
    B.hBox $
    List.intersperse B.vBorder $
    List.map drawItem items
  where
    items :: [(Text, Text)]
    items =
      [ ("Tip hash", statusWidgetState ^. statusWidgetTipHeaderHashL)
      , ("Tip slot", statusWidgetState ^. statusWidgetTipSlotL)
      , ("Current slot", statusWidgetState ^. statusWidgetSlotL)
      ]

    drawItem :: (Text, Text) -> B.Widget name
    drawItem (title, content) = B.padLeftRight 1 $ B.hBox
      [ B.txt $ title <> ": "
      , B.withAttr "status.content" $ B.txt content
      ]

data StatusWidgetEvent
  = StatusUpdateEvent UiCardanoStatusUpdate

handleStatusWidgetEvent
  :: StatusWidgetEvent
  -> StateT StatusWidgetState (B.EventM n) ()
handleStatusWidgetEvent = \case
  StatusUpdateEvent UiCardanoStatusUpdate{..} -> do
    statusWidgetTipHeaderHashL .= tipHeaderHash
    statusWidgetTipSlotL .= tipSlot
    statusWidgetSlotL .= currentSlot
