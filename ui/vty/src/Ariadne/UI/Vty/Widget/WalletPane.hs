module Ariadne.UI.Vty.Widget.WalletPane where

import Universum

import Control.Lens (makeLensesWith, makePrisms, (.=), _Just, zoom, forOf_)
import IiExtras

import qualified Brick as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face


data WalletPaneCommandEvent
  = WalletPaneCommandEvent UiCommandId UiCommandEvent

data BalancePromise = WaitingBalance UiCommandId | FailedBalance Text | Balance Text
makePrisms ''BalancePromise

data WalletPaneInfo
  = WalletPaneWalletInfo
    { balance :: BalancePromise
    }
  | WalletPaneAccountInfo
    { balance :: BalancePromise
    }
  | WalletPaneAddressInfo
    { balance :: BalancePromise
    }

makeLensesWith postfixLFields ''WalletPaneInfo

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
          Just info -> V.vertCat
            [ case info of
                WalletPaneWalletInfo{} -> V.text' attr "Wallet"
                WalletPaneAccountInfo{} -> V.text' attr "Account"
                WalletPaneAddressInfo{} -> V.text' attr "Address"
            , V.text' attr $ "Total balance: " <> case info ^. balanceL of
                WaitingBalance _ -> "Calculating..."
                FailedBalance e -> pretty e
                Balance bal -> pretty bal
            ]
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
 zoom (walletPaneItemInfoL . _Just . balanceL) $ do
   cmdOrBal <- get
   forOf_ _WaitingBalance cmdOrBal $ \commandId' ->
     when (cmdIdEqObject commandId == cmdIdEqObject commandId') $
       case commandEvent of
         UiCommandSuccess d -> put $ Balance (show d)
         UiCommandFailure _ -> put $ FailedBalance "Could not retrieve balance"
         UiCommandOutput _ -> pure ()

handleWalletPaneWidgetEvent
  :: UiLangFace
  -> WalletPaneWidgetEvent
  -> StateT WalletPaneWidgetState (B.EventM n) ()
handleWalletPaneWidgetEvent UiLangFace{..} ev = do
  case ev of
    WalletPaneUpdateEvent itemInfo -> do
      walletPaneInitializedL .= True
      whenJust itemInfo $ \case
        UiWalletPaneRefreshWalletBalance -> refreshBalance WalletPaneWalletInfo
        UiWalletPaneRefreshAccountBalance -> refreshBalance WalletPaneAccountInfo
        UiWalletPaneRefreshAddressBalance -> refreshBalance WalletPaneAddressInfo
  where
    refreshBalance info = do
      mCommandId <- (^? walletPaneItemInfoL . _Just . balanceL . _WaitingBalance) <$> get

      let putExpr = liftIO . langPutCommand . langMkExpr

      -- Kill previous command
      whenJust (mCommandId >>= cmdTaskId) (void . putExpr . UiKill)

      commandId <- putExpr UiBalance
      walletPaneItemInfoL .= Just (info $ WaitingBalance commandId)
