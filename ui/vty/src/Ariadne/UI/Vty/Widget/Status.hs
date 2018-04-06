module Ariadne.UI.Vty.Widget.Status where

import Control.Lens (makeLensesWith, (.=))
import Data.Text as Text
import Universum

import qualified Brick as B
import qualified Control.Monad.Trans.State as State
import qualified Graphics.Vty as V

import IiExtras

import Ariadne.UI.Vty.Face

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
  B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Fixed
    , B.render = render
    }
  where
    render = do
      rdrCtx <- B.getContext
      let
        pad = (rdrCtx ^. B.availWidthL) - (V.imageWidth img)

        backStatusAttr =
          V.defAttr
            `V.withForeColor` V.black
            `V.withBackColor` V.white

        fill n = V.charFill @Int backStatusAttr ' ' n 1

        img = V.text' backStatusAttr content

        content = Text.intercalate " │ "
          [ "Tip hash: " <> (statusWidgetState ^. statusWidgetTipHeaderHashL)
          , "Tip slot: " <> (statusWidgetState ^. statusWidgetTipSlotL)
          , "Current slot: " <> (statusWidgetState ^. statusWidgetSlotL)]

        img' = V.horizCat [img, fill pad]

      return $
        B.emptyResult
          & B.imageL .~ img'

data StatusWidgetEvent
  = StatusUpdateEvent UiCardanoStatusUpdate

handleStatusWidgetEvent
  :: StatusWidgetEvent
  -> State.StateT StatusWidgetState IO ()
handleStatusWidgetEvent = \case
  StatusUpdateEvent UiCardanoStatusUpdate{..} -> do
    statusWidgetTipHeaderHashL .= tipHeaderHash
    statusWidgetTipSlotL .= tipSlot
    statusWidgetSlotL .= currentSlot