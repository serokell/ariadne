module Ariadne.UI.Vty.Widget.Help
       ( HelpWidgetState
       , initHelpWidget
       , drawHelpWidget

       , HelpWidgetEvent(..)
       , handleHelpWidgetEvent
       ) where

import Universum

import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.UI
import Control.Lens (makeLensesWith)
import IiExtras

import qualified Brick as B
import qualified Graphics.Vty as V
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data HelpWidgetState =
  HelpWidgetState
    { helpWidgetData :: ![PP.Doc]
    }

makeLensesWith postfixLFields ''HelpWidgetState

widgetName :: BrickName
widgetName = BrickHelp

initHelpWidget :: UiLangFace -> HelpWidgetState
initHelpWidget UiLangFace{..} = HelpWidgetState
  { helpWidgetData = langGetHelp
  }

drawHelpWidget :: HelpWidgetState -> B.Widget BrickName
drawHelpWidget helpWidgetState =
  B.viewport widgetName B.Vertical $
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

handleHelpWidgetEvent
  :: HelpWidgetEvent
  -> StateT HelpWidgetState (B.EventM BrickName) ()
handleHelpWidgetEvent ev = do
  case ev of
    HelpScrollingEvent action ->
      lift $ handleScrollingEvent widgetName action
