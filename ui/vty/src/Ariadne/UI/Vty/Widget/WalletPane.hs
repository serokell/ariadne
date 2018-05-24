module Ariadne.UI.Vty.Widget.WalletPane where

import Universum

import Control.Lens (forOf_, makeLensesWith, makePrisms, zoom, (.=), _Just)
import IiExtras

import qualified Brick as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard


data WalletPaneCommandEvent
  = WalletPaneCommandEvent UiCommandId UiCommandResultEvent

data BalancePromise = WaitingBalance UiCommandId | FailedBalance Text | Balance Text
makePrisms ''BalancePromise

data WalletPaneInfo
  = WalletPaneWalletInfo
    { label :: Text
    , balance :: BalancePromise
    }
  | WalletPaneAccountInfo
    { label :: Text
    , balance :: BalancePromise
    }
  | WalletPaneAddressInfo
    { label :: Text
    , balance :: BalancePromise
    }

makeLensesWith postfixLFields ''WalletPaneInfo
makePrisms ''WalletPaneInfo

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
drawWalletPaneWidget hasFocus wpws =
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
        selAttr = attr <> B.attrMapLookup "selected" (rdrCtx ^. B.ctxAttrMapL)
        img = case walletPaneItemInfo of
          Nothing ->
            V.text' attr "Select a wallet, an account, or an address"
          Just info -> V.vertCat $
            [ case info of
                WalletPaneWalletInfo{} -> V.text' attr "Wallet"
                WalletPaneAccountInfo{} -> V.text' attr "Account"
                WalletPaneAddressInfo{} -> V.text' attr "Address"
            , V.text' attr $ info ^. labelL
            ] ++
              case info of
                WalletPaneAddressInfo{} -> [V.text' (if hasFocus then selAttr else attr) copyButtonText]
                _ -> []
            ++
            [ V.text' attr $ "Total balance: " <> case info ^. balanceL of
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
  = WalletPaneUpdateEvent (Maybe UiWalletPaneInfo)
  | WalletPaneMouseDownEvent B.Location
  | WalletPaneCopySelectionEvent

keyToWalletPaneEvent :: KeyboardEvent -> Maybe WalletPaneWidgetEvent
keyToWalletPaneEvent = \case
  KeyEnter -> Just WalletPaneCopySelectionEvent
  _ -> Nothing

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
        Just UiWalletPaneInfoWallet -> setInfo WalletPaneWalletInfo wpiLabel
        Just UiWalletPaneInfoAccount -> setInfo WalletPaneAccountInfo wpiLabel
        Just UiWalletPaneInfoAddress -> setInfo WalletPaneAddressInfo wpiLabel
        _ -> walletPaneItemInfoL .= Nothing
    WalletPaneMouseDownEvent coords -> when (isCopyButtonClick coords) $
      void $ putExpr UiCopySelection
    WalletPaneCopySelectionEvent ->
      zoom (walletPaneItemInfoL . _Just . _WalletPaneAddressInfo) $
        void $ putExpr UiCopySelection
  where
    setInfo info label = do
      balancePromise <- refreshBalance
      walletPaneItemInfoL .= Just (info (fromMaybe "" label) balancePromise)

    putExpr :: UiOperation -> StateT s (B.EventM n) UiCommandId
    putExpr = liftIO . langPutCommand . langMkExpr

    refreshBalance = do
      mCommandId <- (^? walletPaneItemInfoL . _Just . balanceL . _WaitingBalance) <$> get

      -- Kill previous command
      whenJust (mCommandId >>= cmdTaskId) (void . putExpr . UiKill)

      commandId <- putExpr UiBalance
      return $ WaitingBalance commandId

copyButtonText :: Text
copyButtonText = "[ Copy ]"

isCopyButtonClick :: B.Location -> Bool
isCopyButtonClick (B.Location (col,row)) = row == 2 && col >= 0 && col <= length copyButtonText
