module Ariadne.UI.Vty.Widget.WalletPane where

import Universum

import Control.Lens (makeLensesWith, makePrisms, (.=), _Just, zoom, forOf_)
import qualified Data.Text as T
import IiExtras

import qualified Brick as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face


data WalletPaneCommandEvent
  = WalletPaneCommandEvent UiCommandId UiCommandResultEvent

data BalancePromise = WaitingBalance UiCommandId | FailedBalance Text | Balance Text
makePrisms ''BalancePromise

data WalletPaneInfo
  = WalletPaneWalletInfo
    { label :: Text
    , derivationPath :: [Word32]
    , balance :: BalancePromise
    }
  | WalletPaneAccountInfo
    { label :: Text
    , derivationPath :: [Word32]
    , balance :: BalancePromise
    }
  | WalletPaneAddressInfo
    { label :: Text
    , derivationPath :: [Word32]
    , balance :: BalancePromise
    }

makeLensesWith postfixLFields ''WalletPaneInfo

data WalletPaneWidgetState n =
  WalletPaneWidgetState
    { walletPaneItemInfo :: Maybe WalletPaneInfo
    , walletPaneInitialized :: Bool
    , walletPaneBrickName :: n
    }

makeLensesWith postfixLFields ''WalletPaneWidgetState

initWalletPaneWidget
  :: (Ord n, Show n)
  => n
  -> WalletPaneWidgetState n
initWalletPaneWidget name =
  WalletPaneWidgetState
    { walletPaneItemInfo = Nothing
    , walletPaneInitialized = False
    , walletPaneBrickName = name
    }

drawWalletPaneWidget
  :: (Ord n, Show n)
  => Bool
  -> WalletPaneWidgetState n
  -> B.Widget n
drawWalletPaneWidget _hasFocus wpws =
  B.viewport walletPaneBrickName B.Both B.Widget
    { B.hSize = B.Fixed
    , B.vSize = B.Fixed
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
          Just info -> V.vertCat $
            [ case info of
                WalletPaneWalletInfo{} -> V.text' attr "Wallet"
                WalletPaneAccountInfo{} -> V.text' attr "Account"
                WalletPaneAddressInfo{} -> V.text' attr "Address"
            , V.text' attr $ info ^. labelL
            , V.text' attr $ "Total balance: " <> case info ^. balanceL of
                WaitingBalance _ -> "Calculating..."
                FailedBalance e -> pretty e
                Balance bal -> pretty bal
            ] ++ case info of
              WalletPaneWalletInfo{} -> []
              x -> [V.text' attr $ "Derivation path: " <> T.intercalate "-" (map pretty $ derivationPath x)]
        imgOrLoading
          | walletPaneInitialized = img
          | otherwise = V.text attr "Loading..."
      return $
        B.emptyResult
          & B.imageL .~ imgOrLoading

data WalletPaneWidgetEvent
  = WalletPaneUpdateEvent (Maybe UiWalletPaneInfo)

handleWalletPaneCommandEvent
  :: WalletPaneCommandEvent
  -> StateT (WalletPaneWidgetState n) (B.EventM n) ()
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
  -> StateT (WalletPaneWidgetState n) (B.EventM n) ()
handleWalletPaneWidgetEvent UiLangFace{..} ev = do
  case ev of
    WalletPaneUpdateEvent itemInfo -> do
      walletPaneInitializedL .= True
      whenJust itemInfo $ \UiWalletPaneInfo{..} -> case wpiType of
        Just UiWalletPaneInfoWallet -> setInfo WalletPaneWalletInfo [] wpiLabel
        Just (UiWalletPaneInfoAccount derPath) -> setInfo WalletPaneAccountInfo derPath wpiLabel
        Just (UiWalletPaneInfoAddress derPath) -> setInfo WalletPaneAddressInfo derPath wpiLabel
        _ -> walletPaneItemInfoL .= Nothing
  where
    setInfo info derPath label = do
      balancePromise <- refreshBalance
      walletPaneItemInfoL .= Just (info (fromMaybe "" label) derPath balancePromise)
    refreshBalance = do
      mCommandId <- (^? walletPaneItemInfoL . _Just . balanceL . _WaitingBalance) <$> get

      let putExpr = liftIO . langPutCommand . langMkExpr

      -- Kill previous command
      whenJust (mCommandId >>= cmdTaskId) (void . putExpr . UiKill)

      commandId <- putExpr UiBalance
      return $ WaitingBalance commandId
