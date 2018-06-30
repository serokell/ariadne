module Ariadne.UI.Vty.Widget.Account
       ( AccountWidgetState
       , initAccountWidget
       , drawAccountWidget

       , AccountWidgetEvent(..)
       , keyToAccountEvent
       , handleAccountFocus
       , handleAccountFocusIn
       , handleAccountWidgetEvent
       ) where

import Universum

import Control.Lens (each, forOf_, makeLensesWith, makePrisms, zoom, (.=), _Just)
import IiExtras

import qualified Brick as B
-- import qualified Brick.Focus as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.UI

data BalancePromise = WaitingBalance UiCommandId | FailedBalance Text | Balance Text
makePrisms ''BalancePromise

data WalletInfo
  = WalletAccountInfo
    { label :: !Text
    , derivationPath :: ![Word32]
    , addresses :: ![(Word32, Text)]
    , balance :: !BalancePromise
    }

makeLensesWith postfixLFields ''WalletInfo

data AccountWidgetState =
  AccountWidgetState
    { walletItemInfo :: !(Maybe WalletInfo)
    , walletInitialized :: !Bool
    }

makeLensesWith postfixLFields ''AccountWidgetState

initAccountWidget :: AccountWidgetState
initAccountWidget =
  AccountWidgetState
    { walletItemInfo = Nothing
    , walletInitialized = False
    }

drawAccountWidget :: Bool -> AccountWidgetState -> B.Widget BrickName
drawAccountWidget _hasFocus widgetState@AccountWidgetState{..} =
  B.Widget
    { B.hSize = B.Fixed
    , B.vSize = B.Fixed
    , B.render = render
    }
  where
    render = do
      rdrCtx <- B.getContext
      let
        attr = rdrCtx ^. B.attrL
        selAttr = attr <> B.attrMapLookup "selected" (rdrCtx ^. B.ctxAttrMapL)
        img = case walletItemInfo of
          Nothing ->
            V.text' attr "Select a wallet, an account, or an address"
          Just info -> drawAccountDetail attr selAttr info widgetState
        imgOrLoading
          | walletInitialized = img
          | otherwise = V.text attr "Loading..."
      return $
        B.emptyResult
          & B.imageL .~ imgOrLoading

drawAccountDetail :: V.Attr -> V.Attr -> WalletInfo -> AccountWidgetState -> V.Image
drawAccountDetail attr _selAttr info AccountWidgetState{..} = V.vertCat $
  [ V.text' attr $ "Account name:     " <> info ^. labelL
  , V.text' attr $ "Total balance:    " <> case info ^. balanceL of
      WaitingBalance _ -> "Calculating..."
      FailedBalance e -> pretty e
      Balance bal -> pretty bal
  , V.text' attr ""
  , V.text' attr "Addresses:"
  , V.text' attr ""
  ] ++ (map (V.text' attr) $ info ^.. addressesL . each . _2)

data AccountWidgetEvent
  = AccountUpdateEvent (Maybe UiWalletInfo)
  | AccountBalanceCommandResult UiCommandId UiBalanceCommandResult
  | AccountMouseDownEvent B.Location
  | AccountCopySelectionEvent

keyToAccountEvent :: KeyboardEvent -> Maybe AccountWidgetEvent
keyToAccountEvent = \case
  KeyEnter -> Just AccountCopySelectionEvent
  _ -> Nothing

handleAccountFocus
  :: Bool
  -> StateT AccountWidgetState (B.EventM BrickName) Bool
handleAccountFocus _back = do
  return False
--   newFocus <- accountFocusRingL <%= if back then B.focusPrev else B.focusNext
--   return $ B.focusGetCurrent newFocus /= Just BrickNone

handleAccountFocusIn
  :: Bool
  -> StateT AccountWidgetState (B.EventM BrickName) ()
handleAccountFocusIn _back = do
  return ()
--   accountFocusRingL %= (if back then B.focusPrev else B.focusNext) . B.focusSetCurrent BrickNone

handleAccountWidgetEvent
  :: UiLangFace
  -> AccountWidgetEvent
  -> StateT AccountWidgetState (B.EventM BrickName) ()
handleAccountWidgetEvent UiLangFace{..} ev = do
  case ev of
    AccountUpdateEvent itemInfo -> do
      walletInitializedL .= True
      whenJust itemInfo $ \UiWalletInfo{..} -> let label = fromMaybe "" wpiLabel in case wpiType of
        Just (UiWalletInfoAccount dp) -> setInfo (WalletAccountInfo label dp wpiAddresses)
        _ -> walletItemInfoL .= Nothing
    AccountBalanceCommandResult commandId result -> do
      zoom (walletItemInfoL . _Just . balanceL) $ do
        cmdOrBal <- get
        forOf_ _WaitingBalance cmdOrBal $ \commandId' ->
          when (cmdIdEqObject commandId == cmdIdEqObject commandId') $
            case result of
              UiBalanceCommandSuccess balance ->
                put $ Balance balance
              UiBalanceCommandFailure err -> do
                put $ FailedBalance err
    AccountMouseDownEvent coords -> when (isCopyButtonClick coords) $
      void $ putExpr UiCopySelection
    AccountCopySelectionEvent ->
      zoom (walletItemInfoL . _Just) $
        void $ putExpr UiCopySelection
  where
    setInfo info = do
      balancePromise <- refreshBalance
      walletItemInfoL .= Just (info balancePromise)

    putExpr :: UiCommand -> StateT s (B.EventM n) (Either Text UiCommandId)
    putExpr = liftIO . langPutUiCommand

    refreshBalance = do
      mCommandId <- (^? walletItemInfoL . _Just . balanceL . _WaitingBalance) <$> get

      -- Kill previous command
      whenJust (mCommandId >>= cmdTaskId) (void . putExpr . UiKill)

      Right commandId <- putExpr UiBalance
      return $ WaitingBalance commandId

copyButtonText :: Text
copyButtonText = "[ Copy ]"

isCopyButtonClick :: B.Location -> Bool
isCopyButtonClick (B.Location (col,row)) = row == 2 && col >= 0 && col <= length copyButtonText
