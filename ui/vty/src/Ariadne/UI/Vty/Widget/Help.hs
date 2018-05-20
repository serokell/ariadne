module Ariadne.UI.Vty.Widget.Help where

import Universum

import Ariadne.UI.Vty.AnsiToVty
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Scrolling
import Control.Lens (makeLensesWith)
import IiExtras

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
  => UiLangFace
  -> n
  -> HelpWidgetState n
initHelpWidget UiLangFace{..} name = HelpWidgetState
  { helpWidgetData = langGetHelp
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

handleHelpWidgetEvent
  :: (Ord n, Show n)
  => HelpWidgetEvent
  -> StateT (HelpWidgetState n) (B.EventM n) ()
handleHelpWidgetEvent ev = do
  name <- use helpWidgetBrickNameL
  case ev of
    HelpScrollingEvent action ->
      lift $ handleScrollingEvent name action
