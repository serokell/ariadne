module Ariadne.UI.Vty.Widget.Help where

import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.Scrolling
import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import IiExtras
import Prelude

import qualified Brick as B
import qualified Graphics.Vty as V
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data HelpWidgetState n =
  HelpWidgetState
    { helpWidgetData :: [PP.Doc]
    , helpWidgetBrickName :: n
    }

makeLensesWith postfixLFields ''HelpWidgetState

initHelpWidget
  :: (Ord n, Show n)
  => n
  -> HelpWidgetState n
initHelpWidget name = HelpWidgetState
  { helpWidgetData = []
  , helpWidgetBrickName = name
  }

drawHelpWidget
  :: (Ord n, Show n)
  => HelpWidgetState n
  -> B.Widget n
drawHelpWidget helpWidgetState =
  B.viewport name B.Vertical $
    B.cached name B.Widget
      { B.hSize = B.Fixed
      , B.vSize = B.Fixed
      , B.render = render
      }
  where
    name = helpWidgetState ^. helpWidgetBrickNameL
    render = do
      rdrCtx <- B.getContext
      let
        attr = rdrCtx ^. B.attrL
        width = rdrCtx ^. B.availWidthL
        img =
          V.vertCat $
          fmap drawDoc (helpWidgetState ^. helpWidgetDataL)
        drawDoc = pprDoc attr width
      return $
        B.emptyResult
          & B.imageL .~ img

data HelpCompleted = HelpCompleted | HelpInProgress

data HelpWidgetEvent
  = HelpScrollingEvent ScrollingAction
  | HelpData [PP.Doc]

handleHelpWidgetEvent
  :: (Ord n, Show n)
  => HelpWidgetEvent
  -> StateT (HelpWidgetState n) (B.EventM n) ()
handleHelpWidgetEvent ev = do
  name <- use helpWidgetBrickNameL
  case ev of
    HelpScrollingEvent action ->
      lift $ handleScrollingEvent name action
    HelpData doc -> do
      zoom helpWidgetDataL $ modify $ const doc
      lift $ B.invalidateCacheEntry name
