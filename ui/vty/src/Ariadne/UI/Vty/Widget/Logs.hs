module Ariadne.UI.Vty.Widget.Logs where

import Control.Lens
import Control.Monad.Trans.State
import IiExtras
import Prelude
import Data.Text as Text

import qualified Brick as B
import qualified Graphics.Vty as V

-- TODO (thatguy): use the fancy `named` library suggested by @int-index.
newtype Width = Width { unWidth :: Int }

newtype LogMessage = LogMessage Text

data ScrollingOffset
    = OffsetFollowing
    | OffsetFixed Int

data LogsWidgetState =
  LogsWidgetState
    { logsWidgetMessages :: [LogMessage]
    , logsWidgetScrollingOffset :: Int -> Int -> ScrollingOffset
    -- ^ viewport height -> full image height -> offset from top
    -- TODO (thatguy): use `named`
    }

makeLensesWith postfixLFields ''LogsWidgetState

initLogsWidget :: LogsWidgetState
initLogsWidget = LogsWidgetState
  {
    logsWidgetMessages = []
  , logsWidgetScrollingOffset = \_ _ -> OffsetFollowing
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
          crop viewportHeight (logsWidgetState ^. logsWidgetScrollingOffsetL) $
          V.vertCat $
          fmap drawLogMessage outElems
        drawLogMessage (LogMessage message) = V.cropRight width (V.text' V.defAttr message)
      return $
        B.emptyResult
          & B.imageL .~ img
    crop :: Int -> (Int -> Int -> ScrollingOffset) -> V.Image -> V.Image
    crop viewportHeight mkPos image =
      let imageHeight = V.imageHeight image in
      case mkPos viewportHeight imageHeight of
        OffsetFollowing ->
          V.cropTop viewportHeight image
        OffsetFixed pos ->
          V.cropBottom viewportHeight $ V.cropTop (imageHeight - pos) image

data LogsCompleted = LogsCompleted | LogsInProgress

data LogsWidgetEvent
  = LogsExit
  | LogsScrollDown
  | LogsScrollUp
  | LogsMessage Text

handleLogsWidgetEvent
  :: LogsWidgetEvent
  -> StateT LogsWidgetState IO LogsCompleted
handleLogsWidgetEvent ev = do
  case ev of
    LogsExit ->
      return LogsCompleted
    LogsScrollUp ->
      return LogsInProgress
    LogsScrollDown ->
      return LogsInProgress
    LogsMessage message -> do
      zoom logsWidgetMessagesL $ modify (LogMessage message:)
      return LogsInProgress
