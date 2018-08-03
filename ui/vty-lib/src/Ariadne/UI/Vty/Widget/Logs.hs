module Ariadne.UI.Vty.Widget.Logs
       ( initLogsWidget
       ) where

import Universum

import Control.Lens (makeLensesWith, zoom, (+=), (.=))
import qualified Data.Text as Text
import IiExtras

import qualified Brick as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Widget

newtype LogMessage = LogMessage Text

data LogsWidgetState =
  LogsWidgetState
    { logsWidgetMessages :: ![LogMessage]
    , logsWidgetLinesRendered :: !Int
    , logsWidgetLinesTotal :: !Int
    , logsWidgetFollow :: !Bool
    }

makeLensesWith postfixLFields ''LogsWidgetState

initLogsWidget :: Widget p
initLogsWidget =
  initWidget $ do
    setWidgetDraw drawLogsWidget
    setWidgetHandleScroll handleLogsWidgetScroll
    setWidgetHandleEvent handleLogsWidgetEvent
    setWidgetState LogsWidgetState
      { logsWidgetMessages = []
      , logsWidgetLinesRendered = 0
      , logsWidgetLinesTotal = 0
      , logsWidgetFollow = True
      }

drawLogsWidget :: LogsWidgetState -> WidgetDrawM LogsWidgetState p (B.Widget WidgetName)
drawLogsWidget LogsWidgetState{..} = do
  widgetName <- getWidgetName
  return $
    fixedViewport widgetName B.Both $
    B.cached widgetName $
    B.Widget
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

handleLogsWidgetScroll
  :: ScrollingAction
  -> WidgetEventM LogsWidgetState p WidgetEventResult
handleLogsWidgetScroll action = do
  widgetName <- B.getName <$> lift get
  rendered <- use logsWidgetLinesRenderedL
  total <- use logsWidgetLinesTotalL
  follow <- use logsWidgetFollowL

  whenJustM (liftBrick $ B.lookupViewport widgetName) $ \vp -> do
    when (not follow && vp ^. B.vpTop + vp ^. B.vpSize ^. _2 >= rendered) $ do
      liftBrick $ B.invalidateCacheEntry widgetName
      when (rendered == total) $ do
        liftBrick $ B.vScrollToBeginning $ B.viewportScroll widgetName
        logsWidgetFollowL .= True
      logsWidgetLinesRenderedL .= total
  follow' <- use logsWidgetFollowL

  when (not follow' && action == ScrollingEnd) $ do
    liftBrick $ B.invalidateCacheEntry widgetName
    logsWidgetLinesRenderedL .= total
  when (follow' && action `elem` [ScrollingLineUp, ScrollingPgUp, ScrollingHome]) $ do
    logsWidgetFollowL .= False
    liftBrick $ B.invalidateCacheEntry widgetName
    liftBrick $ scrollToEnd widgetName
  liftBrick $ handleScrollingEvent widgetName action
  return WidgetEventHandled

handleLogsWidgetEvent
  :: UiEvent
  -> WidgetEventM LogsWidgetState p ()
handleLogsWidgetEvent = \case
  UiBackendEvent (UiBackendLogEvent message) -> do
    widgetName <- B.getName <$> lift get
    rendered <- use logsWidgetLinesRenderedL
    total <- use logsWidgetLinesTotalL
    follow <- use logsWidgetFollowL

    whenJustM (liftBrick $ B.lookupViewport widgetName) $ \vp -> do
      when (not follow && vp ^. B.vpTop + vp ^. B.vpSize ^. _2 >= rendered) $ do
        liftBrick $ B.invalidateCacheEntry widgetName
        when (rendered == total) $ do
          liftBrick $ B.vScrollToBeginning $ B.viewportScroll widgetName
          logsWidgetFollowL .= True
        logsWidgetLinesRenderedL .= total
    follow' <- use logsWidgetFollowL

    zoom logsWidgetMessagesL $ modify (LogMessage message:)
    let msgHeight = length (Text.lines message)
    logsWidgetLinesTotalL += msgHeight
    when follow' $ do
      liftBrick $ B.invalidateCacheEntry widgetName
      logsWidgetLinesRenderedL += msgHeight
  _ ->
    return ()