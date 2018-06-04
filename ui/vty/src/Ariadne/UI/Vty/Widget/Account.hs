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

import Control.Lens (forOf_, makeLensesWith, makePrisms, zoom, (.=), _Just)
import IiExtras

import qualified Data.Text as T
import qualified Brick as B
-- import qualified Brick.Focus as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.UI

data BalancePromise = WaitingBalance UiCommandId | FailedBalance Text | Balance Text
makePrisms ''BalancePromise

data WalletInfo
  = WalletWalletInfo
    { label :: !Text
    , balance :: !BalancePromise
    }
  | WalletAccountInfo
    { label :: !Text
    , derivationPath :: ![Word32]
    , balance :: !BalancePromise
    }
  | WalletAddressInfo
    { label :: !Text
    , derivationPath :: ![Word32]
    , balance :: !BalancePromise
    }

makeLensesWith postfixLFields ''WalletInfo
makePrisms ''WalletInfo

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
drawAccountWidget hasFocus AccountWidgetState{..} =
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
          Just info -> V.vertCat $
            [ case info of
                WalletWalletInfo{} -> V.text' attr "Wallet"
                WalletAccountInfo{} -> V.text' attr "Account"
                WalletAddressInfo{} -> V.text' attr "Address"
            , V.text' attr $ info ^. labelL
            ] ++
              case info of
                WalletAddressInfo{} -> [V.text' (if hasFocus then selAttr else attr) copyButtonText]
                _ -> []
            ++ case info of
                WalletWalletInfo{} -> []
                x -> [ V.text' attr . ("Derivation path: "<>)
                     . T.intercalate "-" $ map pretty $ derivationPath x
                     ]
            ++
            [ V.text' attr $ "Total balance: " <> case info ^. balanceL of
                WaitingBalance _ -> "Calculating..."
                FailedBalance e -> pretty e
                Balance bal -> pretty bal
            ]
        imgOrLoading
          | walletInitialized = img
          | otherwise = V.text attr "Loading..."
      return $
        B.emptyResult
          & B.imageL .~ imgOrLoading

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
        Just UiWalletInfoWallet -> setInfo (WalletWalletInfo label)
        Just (UiWalletInfoAccount dp) -> setInfo (WalletAccountInfo label dp)
        Just (UiWalletInfoAddress dp) -> setInfo (WalletAddressInfo label dp)
        _ -> walletItemInfoL .= Nothing
    AccountBalanceCommandResult commandId result -> do
      zoom (walletItemInfoL . _Just . balanceL) $ do
        cmdOrBal <- get
        forOf_ _WaitingBalance cmdOrBal $ \commandId' ->
          when (cmdIdEqObject commandId == cmdIdEqObject commandId') $
            case result of
              UiBalanceCommandSuccess balance ->
                put $ Balance (show balance)
              UiBalanceCommandFailure err -> do
                put $ FailedBalance err
    AccountMouseDownEvent coords -> when (isCopyButtonClick coords) $
      void $ putExpr UiCopySelection
    AccountCopySelectionEvent ->
      zoom (walletItemInfoL . _Just . _WalletAddressInfo) $
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
