module Ariadne.UI.App where

import Prelude
import Control.Lens
import Control.Monad.Trans.Writer

import qualified Brick as B
import qualified Brick.Focus as B
import qualified Brick.Themes as B
import qualified Brick.Forms as B
import qualified Graphics.Vty as V

import Ariadne.Face (AuxxFace, AuxxEvent)
import Ariadne.UI.LayerName
import Ariadne.Util

import qualified Ariadne.UI.ConfigWidget as CW
import qualified Ariadne.UI.HelpWidget as HW
import qualified Ariadne.UI.WalletWidget as WW

-- Selectors (unique names) for parts of the widgets in the application.
data AppSelector
  = AppSelectorConfig CW.ConfigWidgetSelector
  | AppSelectorWallet WW.WalletWidgetSelector
  deriving (Eq, Ord, Show)

makePrisms ''AppSelector

instance HasReview AppSelector CW.ConfigWidgetSelector where
  reviewOf = _AppSelectorConfig

instance HasPrism AppSelector CW.ConfigWidgetSelector where
  prismOf = _AppSelectorConfig

instance HasReview AppSelector WW.WalletWidgetSelector where
  reviewOf = _AppSelectorWallet

instance HasPrism AppSelector WW.WalletWidgetSelector where
  prismOf = _AppSelectorWallet

data AppState =
  AppState
    { appStateConfig :: CW.ConfigWidgetState AuxxEvent AppSelector
    , appStateHelp :: HW.HelpWidgetState
    , appStateWallet :: WW.WalletWidgetState AppSelector
    , appStateLayerFocus :: B.FocusRing LayerName
    }

makeLensesWith postfixLFields ''AppState

initialAppState :: AppState
initialAppState =
  AppState
    { appStateConfig = CW.initConfigWidget CW.defaultUserInfo
    , appStateHelp = HW.initHelpWidget
    , appStateWallet = WW.initWalletWidget
    , appStateLayerFocus =
        B.focusSetCurrent LayerConfig $
        B.focusRing [LayerWallet, LayerHelp, LayerConfig]
    }

currentFocus :: B.FocusRing LayerName -> LayerName
currentFocus focusRing =
  case B.focusGetCurrent focusRing of
    Nothing -> error "currentFocus: impossible, no focused layer"
    Just a -> a

-- The Ariadne UI view and controller a single record.
app :: AuxxFace -> B.App AppState AuxxEvent AppSelector
app _ = B.App{..} where

  appDraw
    :: AppState
    -> [B.Widget AppSelector]
  appDraw AppState{..} =
    case currentFocus appStateLayerFocus of
      LayerWallet -> [WW.drawWalletWidget appStateWallet]
      LayerHelp -> [HW.drawHelpWidget appStateHelp]
      LayerConfig -> [CW.drawConfigWidget appStateConfig]

  appChooseCursor
    :: AppState
    -> [B.CursorLocation AppSelector]
    -> Maybe (B.CursorLocation AppSelector)
  appChooseCursor appState@AppState{..} =
    case currentFocus appStateLayerFocus of
      LayerWallet -> WW.walletWidgetCursor appStateWallet
      LayerHelp -> const Nothing
      LayerConfig -> const Nothing

  appHandleEvent
    :: AppState
    -> B.BrickEvent AppSelector AuxxEvent
    -> B.EventM AppSelector (B.Next AppState)
  appHandleEvent appState@AppState{..} ev =
    case ev of
      B.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) ->
        B.halt appState
      _ ->
        case currentFocus appStateLayerFocus of
          LayerWallet -> do
            (appState', completed) <- runWriterT $
              appStateWalletL (WW.handleWalletWidgetEvent ev) appState
            case completed of
              WW.WalletInProgress -> B.continue appState'
              WW.WalletCompleted -> B.halt appState'
              WW.WalletToLayer layerName ->
                B.continue $ appState' &
                  appStateLayerFocusL %~ B.focusSetCurrent layerName
          LayerHelp -> do
            (appState', completed) <- runWriterT $
              appStateHelpL (HW.handleHelpWidgetEvent ev) appState
            let
              updateFocus = case completed of
                HW.HelpInProgress -> id
                HW.HelpCompleted ->
                  appStateLayerFocusL %~ B.focusSetCurrent LayerWallet
            B.continue (updateFocus appState')
          LayerConfig -> do
            (appState', completed) <- runWriterT $
              appStateConfigL (CW.handleConfigWidgetEvent ev) appState
            let
              updateFocus = case completed of
                CW.ConfigInProgress -> id
                CW.ConfigCompleted ->
                  appStateLayerFocusL %~ B.focusSetCurrent LayerWallet
            B.continue (updateFocus appState')

  appStartEvent
    :: AppState
    -> B.EventM AppSelector AppState
  appStartEvent = return

  appAttrMap
    :: AppState
    -> B.AttrMap
  appAttrMap AppState{..} =
    case currentFocus appStateLayerFocus of
      LayerWallet -> WW.walletWidgetAttrMap
      LayerHelp -> B.attrMap V.defAttr []
      LayerConfig -> B.themeToAttrMap $ CW.theme $ B.formState appStateConfig
