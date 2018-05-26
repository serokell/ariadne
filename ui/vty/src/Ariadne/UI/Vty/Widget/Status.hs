module Ariadne.UI.Vty.Widget.Status
       ( StatusWidgetState
       , initStatusWidget
       , drawStatusWidget

       , StatusWidgetEvent(..)
       , handleStatusWidgetEvent
       ) where

import Control.Lens (makeLensesWith, (.=))
import Data.List as List
import Data.Text as Text
import Data.Version (Version, showVersion)
import Universum

import qualified Brick as B
import qualified Brick.Widgets.Border as B

import IiExtras

import Ariadne.UI.Vty.Face

data StatusWidgetState =
  StatusWidgetState
    { statusWidgetTipHeaderHash :: !Text
    , statusWidgetTipSlot :: !Text
    , statusWidgetSlot :: !Text
    , statusWidgetNewVersion :: !(Maybe Version)
    }

makeLensesWith postfixLFields ''StatusWidgetState

initStatusWidget :: StatusWidgetState
initStatusWidget = StatusWidgetState
    { statusWidgetTipHeaderHash = "<unknown>"
    , statusWidgetTipSlot = "<unknown>"
    , statusWidgetSlot = "<unknown>"
    , statusWidgetNewVersion = Nothing
    }

drawStatusWidget
  :: StatusWidgetState
  -> B.Widget name
drawStatusWidget statusWidgetState =
  B.padTop (B.Pad 1) $
    B.updateAttrMap (B.mapAttrName "status" B.borderAttr) $
    B.withAttr "status" $
    (case statusWidgetState ^. statusWidgetNewVersionL of
      Just ver -> (B.<=> B.padLeftRight 1 (updateNotification ver))
      Nothing -> identity
    ) $
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

    updateNotification :: Version -> B.Widget name
    updateNotification ver = B.txt $
      "Version " <> (fromString $ showVersion ver) <> " is available! Download it at https://ariadnewallet.io/"

    drawItem :: (Text, Text) -> B.Widget name
    drawItem (title, content) = B.padLeftRight 1 $ B.hBox
      [ B.txt $ title <> ": "
      , B.withAttr "status.content" $ B.txt content
      ]

data StatusWidgetEvent
  = StatusUpdateEvent UiCardanoStatusUpdate
  | StatusNewVersionEvent Version

handleStatusWidgetEvent
  :: StatusWidgetEvent
  -> StateT StatusWidgetState (B.EventM n) ()
handleStatusWidgetEvent = \case
  StatusUpdateEvent UiCardanoStatusUpdate{..} -> do
    statusWidgetTipHeaderHashL .= tipHeaderHash
    statusWidgetTipSlotL .= tipSlot
    statusWidgetSlotL .= currentSlot
  StatusNewVersionEvent ver -> statusWidgetNewVersionL .= Just ver
