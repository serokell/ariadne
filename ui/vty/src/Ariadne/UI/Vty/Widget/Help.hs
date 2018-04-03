module Ariadne.UI.Vty.Widget.Help where

import Control.Lens
import Control.Monad.Trans.State
import IiExtras
import Prelude
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.AnsiToVty

import qualified Brick as B
import qualified Graphics.Vty as V
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data HelpWidgetState =
  HelpWidgetState
    { helpWidgetData :: [PP.Doc]
    , helpWidgetScrollingOffset :: ScrollingOffset
    }

makeLensesWith postfixLFields ''HelpWidgetState

initHelpWidget :: HelpWidgetState
initHelpWidget = HelpWidgetState
  {
    helpWidgetData = []
  , helpWidgetScrollingOffset = scrollingOffsetBeginning
  }

drawHelpWidget :: HelpWidgetState -> B.Widget name
drawHelpWidget helpWidgetState =
  B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    render = do
      rdrCtx <- B.getContext
      let
        viewportHeight = (rdrCtx ^. B.availHeightL) - 1
        width = rdrCtx ^. B.availWidthL
        img =
          cropScrolling viewportHeight (helpWidgetState ^. helpWidgetScrollingOffsetL) $
          V.vertCat $
          fmap drawDoc (helpWidgetState ^. helpWidgetDataL)
        drawDoc = pprDoc width
      return $
        B.emptyResult
          & B.imageL .~ img

pprDoc :: Int -> PP.Doc -> V.Image
pprDoc w s = ansiToVty $ PP.renderSmart 0.985 w s

data HelpCompleted = HelpCompleted | HelpInProgress

data HelpWidgetEvent
  = HelpScrollingEvent ScrollingAction
  | HelpData [PP.Doc]

handleHelpWidgetEvent
  :: HelpWidgetEvent
  -> StateT HelpWidgetState IO ()
handleHelpWidgetEvent = \case
  HelpScrollingEvent event -> do
    zoom helpWidgetScrollingOffsetL $ modify $ handleScrollingEvent event
  HelpData doc -> do
    zoom helpWidgetDataL $ modify $ const doc
