module Ariadne.UI.Vty.Widget.Logs
       ( initLogsWidget
       ) where

import Control.Lens (makeLensesWith, (%=), (+=), (.=))

import qualified Brick as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Widget
import Ariadne.Util

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

drawLogsWidget :: LogsWidgetState -> WidgetDrawM LogsWidgetState p WidgetDrawing
drawLogsWidget LogsWidgetState{..} = do
  widgetName <- getWidgetName
  return . singleDrawing $
    (if logsWidgetFollow then id else (B.<+> B.padTop B.Max (B.str " v"))) $
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
          csiToVty attr <$> lines message
        img =
          (if logsWidgetFollow then V.cropTop height else identity) $
          V.vertCat $
          reverse $
          drawLogMessage <$>
          (if logsWidgetFollow then take height else identity) logsWidgetMessages
      return $
        B.emptyResult
          & B.imageL .~ img

handleLogsWidgetScroll
  :: ScrollingAction
  -> WidgetEventM LogsWidgetState p WidgetEventResult
handleLogsWidgetScroll action = do
  widgetName <- B.getName <$> get
  total <- use (widgetStateL . logsWidgetLinesTotalL)
  follow <- updateFollow widgetName

  when (not follow && action == ScrollingEnd) $ do
    liftBrick $ B.invalidateCacheEntry widgetName
    widgetStateL . logsWidgetLinesRenderedL .= total
  when (follow && action `elem` [ScrollingLineUp, ScrollingPgUp, ScrollingHome]) $ do
    widgetStateL . logsWidgetFollowL .= False
    liftBrick $ B.invalidateCacheEntry widgetName
    liftBrick $ scrollToEnd widgetName
  liftBrick $ handleScrollingEvent widgetName action
  return WidgetEventHandled

handleLogsWidgetEvent
  :: UiEvent
  -> WidgetEventM LogsWidgetState p ()
handleLogsWidgetEvent = \case
  UiBackendEvent (UiBackendLogEvent message) -> do
    widgetName <- B.getName <$> get
    follow <- updateFollow widgetName

    let msgHeight = length (lines message)
    zoomWidgetState $ do
      logsWidgetMessagesL %= (LogMessage message:)
      logsWidgetLinesTotalL += msgHeight
    when follow $ do
      liftBrick $ B.invalidateCacheEntry widgetName
      widgetStateL . logsWidgetLinesRenderedL += msgHeight
  _ ->
    pass

updateFollow :: WidgetName -> WidgetEventM LogsWidgetState p Bool
updateFollow widgetName = do
  rendered <- use (widgetStateL . logsWidgetLinesRenderedL)
  total <- use (widgetStateL . logsWidgetLinesTotalL)
  follow <- use (widgetStateL . logsWidgetFollowL)

  whenJustM (liftBrick $ B.lookupViewport widgetName) $ \vp -> do
    when (not follow && vp ^. B.vpTop + vp ^. (B.vpSize . _2) >= rendered) $ do
      liftBrick $ B.invalidateCacheEntry widgetName
      when (rendered == total) $ do
        liftBrick $ B.vScrollToBeginning $ B.viewportScroll widgetName
        widgetStateL . logsWidgetFollowL .= True
      widgetStateL . logsWidgetLinesRenderedL .= total
  use (widgetStateL . logsWidgetFollowL)
