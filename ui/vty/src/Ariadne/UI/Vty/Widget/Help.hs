module Ariadne.UI.Vty.Widget.Help where

import Control.Lens
import Control.Monad.Trans.State
import IiExtras
import Prelude
import Ariadne.UI.Vty.Scrolling

import qualified Brick as B
import qualified Graphics.Vty as V

data HelpMessage = HelpMessage

data HelpWidgetState =
  HelpWidgetState
    { helpWidgetMessages :: [HelpMessage]
    , helpWidgetScrollingOffset :: ScrollingOffset
    }

makeLensesWith postfixLFields ''HelpWidgetState

initHelpWidget :: HelpWidgetState
initHelpWidget = HelpWidgetState
  {
    helpWidgetMessages = []
  , helpWidgetScrollingOffset = defaultScrollingOffset
  }

drawHelpWidget :: HelpWidgetState -> B.Widget name
drawHelpWidget _helpWidgetState =
  B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    render = do
      let
        img = V.text' V.defAttr "Ariadne documentation goes here..."
      return $
        B.emptyResult
          & B.imageL .~ img

data HelpCompleted = HelpCompleted | HelpInProgress

data HelpWidgetEvent
  = HelpScrollingEvent ScrollingAction

handleHelpWidgetEvent
  :: HelpWidgetEvent
  -> StateT HelpWidgetState IO ()
handleHelpWidgetEvent = \case
  HelpScrollingEvent event -> do
    zoom helpWidgetScrollingOffsetL $ modify $ handleScrollingEvent event
