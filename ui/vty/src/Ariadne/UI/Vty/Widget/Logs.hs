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
    , logsWidgetLineCount :: Int
    , logsWidgetScrolled :: Bool
    , logsWidgetBrickName :: n
    }

makeLensesWith postfixLFields ''LogsWidgetState

initLogsWidget
  :: (Ord n, Show n)
  => n
  -> LogsWidgetState n
initLogsWidget name = LogsWidgetState
  { logsWidgetMessages = []
  , logsWidgetLineCount = 0
  , logsWidgetScrolled = False
  , logsWidgetBrickName = name
  }

drawLogsWidget
  :: (Ord n, Show n)
  => LogsWidgetState n
  -> B.Widget n
drawLogsWidget logsWidgetState =
  B.viewport name B.Both $
    B.cached name B.Widget
      { B.hSize = B.Fixed
      , B.vSize = B.Fixed
      , B.render = render
      }
  where
    name = logsWidgetState ^. logsWidgetBrickNameL
    outElems = reverse (logsWidgetMessages logsWidgetState)
    render = do
      rdrCtx <- B.getContext
      let
        attr = rdrCtx ^. B.attrL
        img =
          V.vertCat $
          fmap drawLogMessage outElems
        drawLogMessage (LogMessage message) =
          V.vertCat $
          csiToVty attr <$> Text.lines message
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
  name <- use logsWidgetBrickNameL
  case ev of
    LogsScrollingEvent action -> do
      lift $ handleScrollingEvent name action
      logsWidgetScrolledL .= True
    LogsMessage message -> do
      lineCount <- use logsWidgetLineCountL
      scrolled <- use logsWidgetScrolledL
      lift $ if scrolled
        then keepScrollingToEnd name lineCount
        else scrollToEnd name

      zoom logsWidgetMessagesL $ modify (LogMessage message:)
      logsWidgetLineCountL += length (Text.lines message)
      lift $ B.invalidateCacheEntry name
