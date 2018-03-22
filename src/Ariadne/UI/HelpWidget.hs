module Ariadne.UI.HelpWidget where

import Prelude
import Control.Monad.Trans.State

import qualified Brick as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import qualified Graphics.Vty as V

data HelpWidgetState = HelpWidgetState

initHelpWidget :: HelpWidgetState
initHelpWidget = HelpWidgetState

drawHelpWidget :: HelpWidgetState -> B.Widget name
drawHelpWidget HelpWidgetState =
  B.withBorderStyle B.unicode $
  B.borderWithLabel (B.str "Help") $
  B.center (B.str "Some help") B.<+> B.vBorder B.<+> B.center (B.str "Some help")

data HelpCompleted = HelpCompleted | HelpInProgress

handleHelpWidgetEvent
  :: B.BrickEvent name ev
  -> StateT HelpWidgetState (B.EventM name) HelpCompleted
handleHelpWidgetEvent ev = do
  case ev of
    B.VtyEvent (V.EvKey V.KEsc []) ->
      return HelpCompleted
    B.VtyEvent (V.EvKey V.KEnter []) ->
      return HelpCompleted
    _ ->
      return HelpInProgress
