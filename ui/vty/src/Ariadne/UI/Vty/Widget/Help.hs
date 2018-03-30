module Ariadne.UI.Vty.Widget.Help where

import Prelude
import Control.Lens
import Control.Monad.Trans.State

import qualified Brick as B
import qualified Graphics.Vty as V

data HelpWidgetState = HelpWidgetState

initHelpWidget :: HelpWidgetState
initHelpWidget = HelpWidgetState

drawHelpWidget :: HelpWidgetState -> B.Widget name
drawHelpWidget HelpWidgetState =
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
  = HelpExit
  | HelpScrollDown
  | HelpScrollUp

handleHelpWidgetEvent
  :: HelpWidgetEvent
  -> StateT HelpWidgetState IO HelpCompleted
handleHelpWidgetEvent ev = do
  case ev of
    HelpExit ->
      return HelpCompleted
    HelpScrollUp ->
      return HelpInProgress
    HelpScrollDown ->
      return HelpInProgress
