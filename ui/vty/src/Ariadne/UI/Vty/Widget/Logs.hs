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
        drawLogMessage (LogMessage message) = V.cropRight width (csiToVty message)
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
  | LogsScrollingEvent ScrollingAction
  | LogsMessage Text

data ScrollingDistance = OneLine | Page

handleLogsWidgetEvent
  :: LogsWidgetEvent
  -> StateT LogsWidgetState IO LogsCompleted
handleLogsWidgetEvent = \case
  LogsExit ->
    return LogsCompleted
  LogsScrollingEvent ScrollingLineUp -> do
    zoom logsWidgetScrollingOffsetL $ modify $ goUp OneLine
    return LogsInProgress
  LogsScrollingEvent ScrollingLineDown -> do
    zoom logsWidgetScrollingOffsetL $ modify $ goDown OneLine
    return LogsInProgress
  LogsScrollingEvent ScrollingPgUp -> do
    zoom logsWidgetScrollingOffsetL $ modify $ goUp Page
    return LogsInProgress
  LogsScrollingEvent ScrollingPgDown -> do
    zoom logsWidgetScrollingOffsetL $ modify $ goDown Page
    return LogsInProgress
  LogsMessage message -> do
    zoom logsWidgetMessagesL $ modify (LogMessage message:)
    return LogsInProgress
  where
    goUp :: ScrollingDistance -> (Int -> Int -> ScrollingOffset) -> Int -> Int -> ScrollingOffset
    goUp distance mkPos viewportHeight imageHeight =
      let numLines = toNumLines viewportHeight distance
          prev = mkPos viewportHeight imageHeight
          pos = unwrapOffset (imageHeight - viewportHeight) prev
      in OffsetFixed $ max 0 (pos - numLines)
    goDown :: ScrollingDistance -> (Int -> Int -> ScrollingOffset) -> Int -> Int -> ScrollingOffset
    goDown distance mkPos viewportHeight imageHeight =
      let numLines = toNumLines viewportHeight distance in
      case mkPos viewportHeight imageHeight of
        OffsetFollowing -> OffsetFollowing
        OffsetFixed pos ->
          if pos >= imageHeight - viewportHeight - numLines then OffsetFollowing
          else OffsetFixed $ pos + numLines
    toNumLines :: Int -> ScrollingDistance -> Int
    toNumLines viewportHeight = \case
      OneLine -> 1
      Page -> viewportHeight
    unwrapOffset :: Int -> ScrollingOffset -> Int
    unwrapOffset def = \case
      OffsetFollowing -> def
      OffsetFixed pos -> pos
