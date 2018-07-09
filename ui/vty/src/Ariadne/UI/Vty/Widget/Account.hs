module Ariadne.UI.Vty.Widget.Account
       ( initAccountWidget
       ) where

import Universum

import Control.Lens (each, forOf_, makeLensesWith, makePrisms, zoom, (.=), _Just)
import IiExtras

import qualified Brick as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Widget

data BalancePromise = WaitingBalance UiCommandId | FailedBalance Text | Balance Text
makePrisms ''BalancePromise

data AccountInfo
  = AccountInfo
    { label :: !Text
    , derivationPath :: ![Word32]
    , addresses :: ![(Word32, Text)]
    , balance :: !BalancePromise
    }

makeLensesWith postfixLFields ''AccountInfo

data AccountWidgetState =
  AccountWidgetState
    { walletLangFace :: !UiLangFace
    , walletItemInfo :: !(Maybe AccountInfo)
    , walletInitialized :: !Bool
    }

makeLensesWith postfixLFields ''AccountWidgetState

initAccountWidget :: UiLangFace -> Widget p
initAccountWidget langFace =
  initWidget $ do
    setWidgetDraw drawAccountWidget
    setWidgetScrollable
    setWidgetHandleEvent handleAccountWidgetEvent
    setWidgetState AccountWidgetState
      { walletLangFace = langFace
      , walletItemInfo = Nothing
      , walletInitialized = False
      }

drawAccountWidget :: AccountWidgetState -> WidgetDrawM AccountWidgetState p (B.Widget WidgetName)
drawAccountWidget widgetState@AccountWidgetState{..} = do
  widgetName <- getWidgetName
  return $
    B.viewport widgetName B.Vertical $
    B.padAll 1 $
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
        img = case walletItemInfo of
          Nothing ->
            V.text' attr "Select a wallet, an account, or an address"
          Just info -> drawAccountDetail attr info widgetState
        imgOrLoading
          | walletInitialized = img
          | otherwise = V.text attr "Loading..."
      return $
        B.emptyResult
          & B.imageL .~ imgOrLoading

drawAccountDetail :: V.Attr -> AccountInfo -> AccountWidgetState -> V.Image
drawAccountDetail attr info AccountWidgetState{..} = V.vertCat $
  [ V.text' attr $ "Account name:     " <> info ^. labelL
  , V.text' attr $ "Total balance:    " <> case info ^. balanceL of
      WaitingBalance _ -> "Calculating..."
      FailedBalance e -> pretty e
      Balance bal -> pretty bal
  , V.text' attr ""
  , V.text' attr "Addresses:"
  , V.text' attr ""
  ] ++ (map (V.text' attr) $ info ^.. addressesL . each . _2)

handleAccountWidgetEvent
  :: UiEvent
  -> WidgetEventM AccountWidgetState p ()
handleAccountWidgetEvent = \case
    UiWalletEvent UiWalletUpdate{..} -> do
      walletInitializedL .= True
      whenJust wuPaneInfoUpdate $ \UiWalletInfo{..} -> let label = fromMaybe "" wpiLabel in case wpiType of
        Just (UiWalletInfoAccount dp) -> setInfo (AccountInfo label dp wpiAddresses)
        _ -> walletItemInfoL .= Nothing
    UiCommandResult commandId (UiBalanceCommandResult result) -> do
      zoom (walletItemInfoL . _Just . balanceL) $ do
        cmdOrBal <- get
        forOf_ _WaitingBalance cmdOrBal $ \commandId' ->
          when (cmdIdEqObject commandId == cmdIdEqObject commandId') $
            case result of
              UiBalanceCommandSuccess balance ->
                put $ Balance balance
              UiBalanceCommandFailure err -> do
                put $ FailedBalance err
    _ ->
      return ()
  where
    setInfo info = do
      balancePromise <- refreshBalance
      walletItemInfoL .= Just (info balancePromise)

    refreshBalance = do
      mCommandId <- (^? walletItemInfoL . _Just . balanceL . _WaitingBalance) <$> get

      -- Kill previous command
      whenJust (mCommandId >>= cmdTaskId) (void . performCommand . UiKill)

      Right commandId <- performCommand UiBalance
      return $ WaitingBalance commandId

performCommand
  :: UiCommand
  -> WidgetEventM AccountWidgetState p (Either Text UiCommandId)
performCommand command = do
  UiLangFace{..} <- use walletLangFaceL
  liftIO . langPutUiCommand $ command
