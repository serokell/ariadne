module Ariadne.UI.Vty.Widget.Status
       ( initStatusWidget
       ) where

import Universum

import Control.Lens (makeLensesWith, (.=))
import Data.List as List
import Data.Version (Version, showVersion)
import Named (Named(..))

import qualified Brick as B
import qualified Brick.Widgets.Border as B

import IiExtras

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Widget

data StatusWidgetState =
  StatusWidgetState
    { statusWidgetSyncProgress :: !(Maybe Text)
    , statusWidgetBlockchainLocal :: !Text
    , statusWidgetBlockchainNetwork :: !Text
    , statusWidgetNewVersion :: !(Maybe Version)
    , statusWidgetAriadneURL :: !Text
    }

makeLensesWith postfixLFields ''StatusWidgetState

initStatusWidget :: Text `Named` "ariadne_url" -> Widget p
initStatusWidget (Named ariadneURL) =
  initWidget $ do
    setWidgetDraw drawStatusWidget
    setWidgetHandleEvent handleStatusWidgetEvent
    setWidgetState StatusWidgetState
      { statusWidgetSyncProgress = Nothing
      , statusWidgetBlockchainLocal = "<unknown>"
      , statusWidgetBlockchainNetwork = "<unknown>"
      , statusWidgetNewVersion = Nothing
      , statusWidgetAriadneURL = ariadneURL
      }

drawStatusWidget :: StatusWidgetState -> WidgetDrawM StatusWidgetState p (B.Widget WidgetName)
drawStatusWidget StatusWidgetState{..} =
  return $
    B.padTop (B.Pad 1) $
    B.updateAttrMap (B.mapAttrName "status" B.borderAttr) $
    B.withAttr "status" $
    maybe identity (\ver -> (B.<=> updateNotification ver)) statusWidgetNewVersion $
    maybe identity (\_ -> (B.<=> inaccurateNotification)) statusWidgetSyncProgress $
    B.vLimit 1 $
    B.padRight B.Max $
    B.hBox $
    List.intersperse B.vBorder $
    List.map drawItem $ catMaybes items
  where
    items :: [Maybe (Text, Text)]
    items =
      [ statusWidgetSyncProgress >>= \progress -> Just ("Syncing", progress)
      , Just ("Local", statusWidgetBlockchainLocal)
      , Just ("Network", statusWidgetBlockchainNetwork)
      ]

    inaccurateNotification :: B.Widget name
    inaccurateNotification = B.padLeftRight 1 . B.txt $
      "Blockchain not synced yet, balances may be inaccurate."

    updateNotification :: Version -> B.Widget name
    updateNotification ver = B.padLeftRight 1 . B.txt $
      "Version " <> (fromString $ showVersion ver) <>
      " is available! Download it at " <> statusWidgetAriadneURL

    drawItem :: (Text, Text) -> B.Widget name
    drawItem (title, content) = B.padLeftRight 1 $ B.hBox
      [ B.txt $ title <> ": "
      , B.withAttr "status.content" $ B.txt content
      ]

handleStatusWidgetEvent
  :: UiEvent
  -> WidgetEventM StatusWidgetState p ()
handleStatusWidgetEvent = \case
  UiCardanoEvent (UiCardanoStatusUpdateEvent UiCardanoStatusUpdate{..}) -> do
    statusWidgetSyncProgressL .= syncProgress
    statusWidgetBlockchainLocalL .= blockchainLocal
    statusWidgetBlockchainNetworkL .= blockchainNetwork
  UiNewVersionEvent ver -> do
    statusWidgetNewVersionL .= Just ver
  _ ->
    return ()
