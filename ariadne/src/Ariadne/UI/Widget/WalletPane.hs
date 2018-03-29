module Ariadne.UI.Widget.WalletPane where

import Prelude
import Control.Lens
import Control.Monad.Trans.State

import qualified Brick as B
import qualified Graphics.Vty as V

data WalletPaneWidgetState = WalletPaneWidgetState

initWalletPaneWidget :: WalletPaneWidgetState
initWalletPaneWidget = WalletPaneWidgetState

drawWalletPaneWidget
  :: Bool
  -> WalletPaneWidgetState
  -> B.Widget name
drawWalletPaneWidget _hasFocus WalletPaneWidgetState =
  B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    render = do
      let
        img = V.text' V.defAttr " Wallet info pane."
      return $
        B.emptyResult
          & B.imageL .~ img

data WalletPaneCompleted = WalletPaneCompleted | WalletPaneInProgress

data WalletPaneWidgetEvent
  = WalletPaneExit
  | WalletPaneScrollDown
  | WalletPaneScrollUp

handleWalletPaneWidgetEvent
  :: WalletPaneWidgetEvent
  -> StateT WalletPaneWidgetState IO WalletPaneCompleted
handleWalletPaneWidgetEvent ev = do
  case ev of
    WalletPaneExit ->
      return WalletPaneCompleted
    WalletPaneScrollUp ->
      return WalletPaneInProgress
    WalletPaneScrollDown ->
      return WalletPaneInProgress
