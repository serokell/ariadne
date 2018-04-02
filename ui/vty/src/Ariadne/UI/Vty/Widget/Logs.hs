module Ariadne.UI.Vty.Widget.Logs where

import Control.Lens
import Control.Monad.Trans.State
import IiExtras
import Prelude
import Data.Text as Text
import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.Scrolling

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
  , logsWidgetScrollingOffset = defaultScrollingOffset
  }

drawLogsWidget :: LogsWidgetState -> B.Widget name
drawLogsWidget logsWidgetState =
  B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    outElems = Prelude.reverse (logsWidgetMessages logsWidgetState)
    render = do
      rdrCtx <- B.getContext
      let
        viewportHeight = (rdrCtx ^. B.availHeightL) - 1
        width = rdrCtx ^. B.availWidthL
        img =
          cropScrolling viewportHeight (logsWidgetState ^. logsWidgetScrollingOffsetL) $
          V.vertCat $
          fmap drawLogMessage outElems
        drawLogMessage (LogMessage message) = V.cropRight width (csiToVty message)
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
    zoom logsWidgetMessagesL $ modify (LogMessage message:)
