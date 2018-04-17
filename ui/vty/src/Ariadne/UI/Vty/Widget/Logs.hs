module Ariadne.UI.Vty.Widget.Logs where

import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.Scrolling
import Control.Lens
import Control.Monad.Trans.State
import Data.Text as Text
import IiExtras
import Prelude

import qualified Brick as B
import qualified Graphics.Vty as V

newtype LogMessage = LogMessage Text

data LogsWidgetState =
  LogsWidgetState
    { logsWidgetMessages :: [LogMessage]
    , logsWidgetScrollingOffset :: ScrollingOffset
    }

makeLensesWith postfixLFields ''LogsWidgetState

initLogsWidget :: LogsWidgetState
initLogsWidget = LogsWidgetState
  {
    logsWidgetMessages = []
  , logsWidgetScrollingOffset = scrollingOffsetFollowing
  }

drawLogsWidget :: LogsWidgetState -> B.Widget name
drawLogsWidget logsWidgetState =
  B.padTop B.Max $ B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    outElems = Prelude.reverse (logsWidgetMessages logsWidgetState)
    render = do
      rdrCtx <- B.getContext
      let
        viewportHeight = (rdrCtx ^. B.availHeightL)
        width = rdrCtx ^. B.availWidthL
        img =
          cropScrolling viewportHeight (logsWidgetState ^. logsWidgetScrollingOffsetL) $
          V.vertCat $
          fmap drawLogMessage outElems
        drawLogMessage (LogMessage message) =
          V.cropRight width $
          V.vertCat $
          fmap csiToVty $
          Text.lines message
      return $
        B.emptyResult
          & B.imageL .~ img

data LogsWidgetEvent
  = LogsScrollingEvent ScrollingAction
  | LogsMessage Text

handleLogsWidgetEvent
  :: LogsWidgetEvent
  -> StateT LogsWidgetState IO ()
handleLogsWidgetEvent = \case
  LogsScrollingEvent event -> do
    zoom logsWidgetScrollingOffsetL $ modify $ handleScrollingEvent event
  LogsMessage message -> do
    zoom logsWidgetMessagesL $ modify $ (Prelude.take 1000) . (LogMessage message:)
