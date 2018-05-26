module Ariadne.UI.Vty.Widget.WalletPane
       ( WalletPaneWidgetState
       , initWalletPaneWidget
       , drawWalletPaneWidget

       , WalletPaneCommandEvent(..)
       , WalletPaneWidgetEvent(..)
       , keyToWalletPaneEvent
       , handleWalletPaneCommandEvent
       , handleWalletPaneWidgetEvent
       ) where

import Universum

import Control.Lens (forOf_, makeLensesWith, makePrisms, zoom, (.=), _Just)
import IiExtras

import qualified Data.Text as T
import qualified Brick as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.UI

data WalletPaneCommandEvent
  = WalletPaneCommandEvent UiCommandId UiCommandResultEvent

data BalancePromise = WaitingBalance UiCommandId | FailedBalance Text | Balance Text
makePrisms ''BalancePromise

data WalletPaneInfo
  = WalletPaneWalletInfo
    { label :: !Text
    , balance :: !BalancePromise
    }
  | WalletPaneAccountInfo
    { label :: !Text
    , derivationPath :: ![Word32]
    , balance :: !BalancePromise
    }
  | WalletPaneAddressInfo
    { label :: !Text
    , derivationPath :: ![Word32]
    , balance :: !BalancePromise
    }

makeLensesWith postfixLFields ''WalletPaneInfo
makePrisms ''WalletPaneInfo

data WalletPaneWidgetState =
  WalletPaneWidgetState
    { walletPaneItemInfo :: !(Maybe WalletPaneInfo)
    , walletPaneInitialized :: !Bool
    }

makeLensesWith postfixLFields ''WalletPaneWidgetState

widgetName :: BrickName
widgetName = BrickWalletPane

initWalletPaneWidget :: WalletPaneWidgetState
initWalletPaneWidget =
  WalletPaneWidgetState
    { walletPaneItemInfo = Nothing
    , walletPaneInitialized = False
    }

drawWalletPaneWidget :: Bool -> WalletPaneWidgetState -> B.Widget BrickName
drawWalletPaneWidget hasFocus wpws =
  B.viewport widgetName B.Both B.Widget
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
            ++ case info of
                WalletPaneWalletInfo{} -> []
                x -> [ V.text' attr . ("Derviation path: "<>)
                     . T.intercalate "-" $ map pretty $ derivationPath x
                     ]
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
  -> StateT WalletPaneWidgetState (B.EventM BrickName) ()
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
  -> StateT WalletPaneWidgetState (B.EventM BrickName) ()
handleWalletPaneWidgetEvent UiLangFace{..} ev = do
  case ev of
    WalletPaneUpdateEvent itemInfo -> do
      walletPaneInitializedL .= True
      whenJust itemInfo $ \UiWalletPaneInfo{..} -> let label = fromMaybe "" wpiLabel in case wpiType of
        Just UiWalletPaneInfoWallet -> setInfo (WalletPaneWalletInfo label)
        Just (UiWalletPaneInfoAccount dp) -> setInfo (WalletPaneAccountInfo label dp)
        Just (UiWalletPaneInfoAddress dp) -> setInfo (WalletPaneAddressInfo label dp)
        _ -> walletPaneItemInfoL .= Nothing
    WalletPaneMouseDownEvent coords -> when (isCopyButtonClick coords) $
      void $ putExpr UiCopySelection
    WalletPaneCopySelectionEvent ->
      zoom (walletPaneItemInfoL . _Just . _WalletPaneAddressInfo) $
        void $ putExpr UiCopySelection
  where
    setInfo info = do
      balancePromise <- refreshBalance
      walletPaneItemInfoL .= Just (info balancePromise)

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
