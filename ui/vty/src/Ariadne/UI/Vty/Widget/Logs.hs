module Ariadne.UI.Vty.Widget.Logs where

import Prelude
import Control.Lens
import Control.Monad.Trans.State

import qualified Brick as B
import qualified Graphics.Vty as V

data LogsWidgetState = LogsWidgetState

initLogsWidget :: LogsWidgetState
initLogsWidget = LogsWidgetState

drawLogsWidget :: LogsWidgetState -> B.Widget name
drawLogsWidget LogsWidgetState =
  B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    render = do
      let
        img = V.text' V.defAttr "Cardano logs go here..."
      return $
        B.emptyResult
          & B.imageL .~ img

data LogsCompleted = LogsCompleted | LogsInProgress

data LogsWidgetEvent
  = LogsExit
  | LogsScrollDown
  | LogsScrollUp

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
