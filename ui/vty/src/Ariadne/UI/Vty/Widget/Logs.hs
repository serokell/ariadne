module Ariadne.UI.Vty.Widget.Logs where

import Universum

import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.Scrolling
import Control.Lens (makeLensesWith, zoom, (+=), (.=))
import qualified Data.Text as Text
import IiExtras

import qualified Brick as B
import qualified Graphics.Vty as V

newtype LogMessage = LogMessage Text

data LogsWidgetState n =
  LogsWidgetState
    { logsWidgetMessages :: [LogMessage]
    , logsWidgetLinesRendered :: Int
    , logsWidgetLinesTotal :: Int
    , logsWidgetFollow :: Bool
    , logsWidgetBrickName :: n
    }

makeLensesWith postfixLFields ''LogsWidgetState

initLogsWidget
  :: (Ord n, Show n)
  => n
  -> LogsWidgetState n
initLogsWidget name = LogsWidgetState
  { logsWidgetMessages = []
  , logsWidgetLinesRendered = 0
  , logsWidgetLinesTotal = 0
  , logsWidgetFollow = True
  , logsWidgetBrickName = name
  }

drawLogsWidget
  :: (Ord n, Show n)
  => LogsWidgetState n
  -> B.Widget n
drawLogsWidget LogsWidgetState{..} =
  fixedViewport name B.Both $
    B.cached name B.Widget
      { B.hSize = B.Fixed
      , B.vSize = B.Fixed
      , B.render = render
      }
  where
    name = logsWidgetBrickName
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
  :: (Ord n, Show n)
  => LogsWidgetEvent
  -> StateT (LogsWidgetState n) (B.EventM n) ()
handleLogsWidgetEvent ev = do
  rendered <- use logsWidgetLinesRenderedL
  total <- use logsWidgetLinesTotalL
  follow <- use logsWidgetFollowL
  name <- use logsWidgetBrickNameL

  whenJustM (lift $ B.lookupViewport name) $ \vp -> do
    when (not follow && vp ^. B.vpTop + vp ^. B.vpSize ^. _2 >= rendered) $ do
      lift $ B.invalidateCacheEntry name
      when (rendered == total) $ do
        lift $ B.vScrollToBeginning $ B.viewportScroll name
        logsWidgetFollowL .= True
      logsWidgetLinesRenderedL .= total
  follow' <- use logsWidgetFollowL

  case ev of
    LogsScrollingEvent action -> do
      when (not follow' && action == ScrollingEnd) $ do
        lift $ B.invalidateCacheEntry name
        logsWidgetLinesRenderedL .= total
      when (follow' && action `elem` [ScrollingLineUp, ScrollingPgUp, ScrollingHome]) $ do
        logsWidgetFollowL .= False
        lift $ B.invalidateCacheEntry name
        lift $ scrollToEnd name
      lift $ handleScrollingEvent name action
    LogsMessage message -> do
      zoom logsWidgetMessagesL $ modify (LogMessage message:)
      let msgHeight = length (Text.lines message)
      logsWidgetLinesTotalL += msgHeight
      when follow' $ do
        lift $ B.invalidateCacheEntry name
        logsWidgetLinesRenderedL += msgHeight
