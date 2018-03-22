module Ariadne.UI.HelpWidget where

import Prelude
import Control.Monad.Trans.Writer

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
  -> HelpWidgetState
  -> WriterT HelpCompleted (B.EventM name) HelpWidgetState
handleHelpWidgetEvent ev helpWidgetState =
  WriterT $ case ev of
    B.VtyEvent (V.EvKey V.KEsc []) ->
      return (helpWidgetState, HelpCompleted)
    B.VtyEvent (V.EvKey V.KEnter []) ->
      return (helpWidgetState, HelpCompleted)
    _ ->
      return (helpWidgetState, HelpInProgress)
