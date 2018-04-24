module Ariadne.UI.Vty.Widget.WalletPane where

import Universum

import Control.Lens (makeLensesWith, (.=), to, _Just, _Left)
import IiExtras

import qualified Brick as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face


data WalletPaneCommandEvent
  = WalletPaneCommandEvent UiCommandId UiCommandEvent

data WalletPaneInfo
  = WalletBalance (Either UiCommandId Text)

data WalletPaneWidgetState =
  WalletPaneWidgetState
    { walletPaneItemInfo :: Maybe WalletPaneInfo
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
      rdrCtx <- B.getContext
      let
        attr = rdrCtx ^. B.attrL
        img = case walletPaneItemInfo of
          Nothing ->
            V.text' attr "Select a wallet, an account, or an address"
          Just (WalletBalance e) -> V.text' attr $ "Total balance: " <> case e of
            Left _ -> "Calculating..."
            Right bal -> pretty bal
        imgOrLoading
          | walletPaneInitialized = img
          | otherwise = V.text attr "Loading..."
      return $
        B.emptyResult
          & B.imageL .~ imgOrLoading

data WalletPaneWidgetEvent
  = WalletPaneUpdateEvent (Maybe UiWalletPaneUpdateInfo)

handleWalletPaneCommandEvent
  :: WalletPaneCommandEvent
  -> StateT WalletPaneWidgetState (B.EventM n) ()
handleWalletPaneCommandEvent (WalletPaneCommandEvent commandId commandEvent) = do
  commandId' <- (^? walletPaneItemInfoL . _Just . to (\(WalletBalance bal) -> bal) . _Left) <$> get

  when (Just (cmdIdEqObject commandId) == fmap cmdIdEqObject commandId') $ case commandEvent of
    UiCommandSuccess d -> walletPaneItemInfoL .= Just (WalletBalance $ Right (show d))
    UiCommandFailure _ -> pure ()
    UiCommandOutput _ -> pure ()

handleWalletPaneWidgetEvent
  :: UiLangFace
  -> WalletPaneWidgetEvent
  -> StateT WalletPaneWidgetState (B.EventM n) ()
handleWalletPaneWidgetEvent UiLangFace{..} ev = do
  case ev of
    WalletPaneUpdateEvent itemInfo -> do
      walletPaneInitializedL .= True
      case itemInfo of
        Nothing -> pure ()
        Just UiWalletPaneRefreshBalance -> do
          mCommandId <- (^? walletPaneItemInfoL . _Just . to (\(WalletBalance bal) -> bal) . _Left) <$> get

          let putExpr = liftIO . langPutCommand . langMkExpr

          -- Kill previous command
          whenJust (mCommandId >>= cmdTaskId) (void . putExpr . UiKill)

          commandId <- putExpr UiBalance
          walletPaneItemInfoL .= Just (WalletBalance $ Left commandId)
