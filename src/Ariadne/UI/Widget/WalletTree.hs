module Ariadne.UI.Widget.WalletTree where

import Prelude
import Control.Lens
import Control.Monad.Trans.State

import qualified Brick as B
import qualified Graphics.Vty as V

data WalletTreeWidgetState = WalletTreeWidgetState

initWalletTreeWidget :: WalletTreeWidgetState
initWalletTreeWidget = WalletTreeWidgetState

drawWalletTreeWidget
  :: Bool
  -> WalletTreeWidgetState
  -> B.Widget name
drawWalletTreeWidget _hasFocus WalletTreeWidgetState =
  B.Widget
    { B.hSize = B.Fixed
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    render = do
      let
        selAttr =
          V.defAttr
            `V.withForeColor` V.black
            `V.withBackColor` V.white
        img =
          V.vertCat
            [ V.text' V.defAttr "root1 ADA"
            , V.text' V.defAttr "├── 1-1"
            , V.text' V.defAttr "├── 1-2"
            , V.text' V.defAttr "│   └── "
                `V.horizJoin` V.text' selAttr  "1-2-2"
            , V.text' V.defAttr "│"
            , V.text' V.defAttr "root2 ADA"
            , V.text' V.defAttr "└── 2-1"
            ]
      return $
        B.emptyResult
          & B.imageL .~ img

data WalletTreeCompleted = WalletTreeCompleted | WalletTreeInProgress

data WalletTreeWidgetEvent
  = WalletTreeExit
  | WalletTreeScrollDown
  | WalletTreeScrollUp

handleWalletTreeWidgetEvent
  :: WalletTreeWidgetEvent
  -> StateT WalletTreeWidgetState IO WalletTreeCompleted
handleWalletTreeWidgetEvent ev = do
  case ev of
    WalletTreeExit ->
      return WalletTreeCompleted
    WalletTreeScrollUp ->
      return WalletTreeInProgress
    WalletTreeScrollDown ->
      return WalletTreeInProgress
