module Ariadne.UI.Vty.Widget.Logs
       ( LogsWidgetState
       , initLogsWidget
       , drawLogsWidget

       , LogsWidgetEvent(..)
       , handleLogsWidgetEvent
       ) where

import Universum

import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.UI
import Control.Lens (makeLensesWith, zoom, (+=), (.=))
import qualified Data.Text as Text
import IiExtras

import qualified Brick as B
import qualified Graphics.Vty as V

newtype LogMessage = LogMessage Text

data LogsWidgetState =
  LogsWidgetState
    { logsWidgetMessages :: ![LogMessage]
    , logsWidgetLinesRendered :: !Int
    , logsWidgetLinesTotal :: !Int
    , logsWidgetFollow :: !Bool
    }

makeLensesWith postfixLFields ''LogsWidgetState

widgetName :: BrickName
widgetName = BrickLogs

initLogsWidget :: LogsWidgetState
initLogsWidget = LogsWidgetState
  { logsWidgetMessages = []
  , logsWidgetLinesRendered = 0
  , logsWidgetLinesTotal = 0
  , logsWidgetFollow = True
  }

drawLogsWidget :: LogsWidgetState -> B.Widget BrickName
drawLogsWidget LogsWidgetState{..} =
  fixedViewport widgetName B.Both $
    B.cached widgetName B.Widget
      { B.hSize = B.Fixed
      , B.vSize = B.Fixed
      , B.render = render
      }
  where
    render = do
      rdrCtx <- B.getContext
      let
        attr = rdrCtx ^. B.attrL
        height = rdrCtx ^. B.availHeightL
        drawLogMessage (LogMessage message) =
          V.vertCat $
          csiToVty attr <$> Text.lines message
        img =
          (if logsWidgetFollow then V.cropTop height else identity) $
          V.vertCat $
          reverse $
          fmap drawLogMessage $
          (if logsWidgetFollow then take height else identity) logsWidgetMessages
      return $
        B.emptyResult
          & B.imageL .~ img

data LogsWidgetEvent
  = LogsScrollingEvent ScrollingAction
  | LogsMessage Text

handleLogsWidgetEvent
  :: LogsWidgetEvent
  -> StateT LogsWidgetState (B.EventM BrickName) ()
handleLogsWidgetEvent ev = do
  rendered <- use logsWidgetLinesRenderedL
  total <- use logsWidgetLinesTotalL
  follow <- use logsWidgetFollowL

  whenJustM (lift $ B.lookupViewport widgetName) $ \vp -> do
    when (not follow && vp ^. B.vpTop + vp ^. B.vpSize ^. _2 >= rendered) $ do
      lift $ B.invalidateCacheEntry widgetName
      when (rendered == total) $ do
        lift $ B.vScrollToBeginning $ B.viewportScroll widgetName
        logsWidgetFollowL .= True
      logsWidgetLinesRenderedL .= total
  follow' <- use logsWidgetFollowL

  case ev of
    LogsScrollingEvent action -> do
      when (not follow' && action == ScrollingEnd) $ do
        lift $ B.invalidateCacheEntry widgetName
        logsWidgetLinesRenderedL .= total
      when (follow' && action `elem` [ScrollingLineUp, ScrollingPgUp, ScrollingHome]) $ do
        logsWidgetFollowL .= False
        lift $ B.invalidateCacheEntry widgetName
        lift $ scrollToEnd widgetName
      lift $ handleScrollingEvent widgetName action
    LogsMessage message -> do
      zoom logsWidgetMessagesL $ modify (LogMessage message:)
      let msgHeight = length (Text.lines message)
      logsWidgetLinesTotalL += msgHeight
      when follow' $ do
        lift $ B.invalidateCacheEntry widgetName
        logsWidgetLinesRenderedL += msgHeight
