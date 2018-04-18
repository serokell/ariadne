module Ariadne.UI.Vty.Widget.WalletPane where

import Universum hiding (StateT, (.~))

import Control.Lens
import Control.Monad.Trans.State
import IiExtras

import qualified Brick as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face

data WalletPaneWidgetState =
  WalletPaneWidgetState
    { walletPaneItemInfo :: Maybe UiWalletPaneInfo
    , walletPaneInitialized :: Bool
    }

makeLensesWith postfixLFields ''WalletPaneWidgetState

initWalletPaneWidget :: WalletPaneWidgetState
initWalletPaneWidget = WalletPaneWidgetState Nothing False

drawWalletPaneWidget
  :: Bool
  -> WalletPaneWidgetState
  -> B.Widget name
drawWalletPaneWidget _hasFocus wpws =
  B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Greedy
    , B.render = render
    }
  where
    WalletPaneWidgetState{..} = wpws
    render = do
      let
        img = case walletPaneItemInfo of
          Nothing ->
            V.text' V.defAttr "Select a wallet, an account, or an address"
          Just (UiWalletPaneWalletInfo name) ->
            V.text' V.defAttr ("Wallet " <> pretty name)
          Just (UiWalletPaneAccountInfo name) ->
            V.text' V.defAttr ("Account " <> pretty name)
          Just UiWalletPaneAddressInfo ->
            V.text' V.defAttr "Address"
        imgOrLoading
          | walletPaneInitialized = img
          | otherwise = V.text V.defAttr "Loading..."
      return $
        B.emptyResult
          & B.imageL .~ imgOrLoading

data WalletPaneWidgetEvent
  = WalletPaneUpdateEvent (Maybe UiWalletPaneInfo)

handleWalletPaneWidgetEvent
  :: WalletPaneWidgetEvent
  -> StateT WalletPaneWidgetState (B.EventM n) ()
handleWalletPaneWidgetEvent ev = do
  case ev of
    WalletPaneUpdateEvent itemInfo -> do
      walletPaneInitializedL .= True
      walletPaneItemInfoL .= itemInfo
