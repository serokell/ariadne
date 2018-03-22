module Ariadne.UI.App where

import Prelude
import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import qualified Brick as B
import qualified Brick.Focus as B
import qualified Brick.Themes as B
import qualified Brick.Forms as B
import qualified Graphics.Vty as V

import Ariadne.Face (AuxxFace, AuxxEvent)
import Ariadne.UI.LayerName
import Ariadne.Util

import qualified Ariadne.UI.Widget.Config as CW
import qualified Ariadne.UI.Widget.Help   as HW
import qualified Ariadne.UI.Widget.Wallet as WW

-- Selectors (unique names) for parts of the widgets in the application.
data AppSelector
  = AppSelectorConfig CW.ConfigWidgetSelector
  | AppSelectorWallet WW.WalletWidgetSelector
  deriving (Eq, Ord, Show)

makePrisms ''AppSelector

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
    { appStateConfig = CW.initConfigWidget AppSelectorConfig CW.defaultUserInfo
    , appStateHelp = HW.initHelpWidget
    , appStateWallet = WW.initWalletWidget AppSelectorWallet
    , appStateLayerFocus =
        B.focusSetCurrent LayerConfig $
        B.focusRing [LayerWallet, LayerHelp, LayerConfig]
    }

-- The Ariadne UI view and controller a single record.
app :: AuxxFace -> B.App AppState AuxxEvent AppSelector
app auxxFace = B.App{..} where

  appDraw
    :: AppState
    -> [B.Widget AppSelector]
  appDraw AppState{..} =
    case currentFocus appStateLayerFocus of
      LayerWallet -> [WW.drawWalletWidget appStateWallet]
      LayerHelp -> [HW.drawHelpWidget appStateHelp]
      LayerConfig -> [CW.drawConfigWidget AppSelectorConfig appStateConfig]

  appChooseCursor
    :: AppState
    -> [B.CursorLocation AppSelector]
    -> Maybe (B.CursorLocation AppSelector)
  appChooseCursor = B.showFirstCursor

  appHandleEvent
    :: AppState
    -> B.BrickEvent AppSelector AuxxEvent
    -> B.EventM AppSelector (B.Next AppState)
  appHandleEvent appState ev = do
    (completed, appState') <-
      runStateT (handleAppEvent auxxFace ev) appState
    case completed of
      AppCompleted -> B.halt appState'
      AppInProgress -> B.continue appState'

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

data AppCompleted = AppCompleted | AppInProgress

handleAppEvent
  :: AuxxFace
  -> B.BrickEvent AppSelector AuxxEvent
  -> StateT AppState (B.EventM AppSelector) AppCompleted
handleAppEvent _ (B.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) =
  return AppCompleted
handleAppEvent auxxFace ev = do
  appStateLayerFocus <- use appStateLayerFocusL
  case currentFocus appStateLayerFocus of
    LayerWallet -> do
      completed <- zoom appStateWalletL $
        WW.handleWalletWidgetEvent auxxFace ev
      case completed of
        WW.WalletInProgress -> return AppInProgress
        WW.WalletCompleted -> return AppCompleted
        WW.WalletToLayer layerName -> do
          zoom appStateLayerFocusL $ modify (B.focusSetCurrent layerName)
          return AppInProgress
    LayerHelp -> mapStateT liftIO $ do
      completed <- zoom appStateHelpL $
        HW.handleHelpWidgetEvent ev
      case completed of
        HW.HelpInProgress -> return ()
        HW.HelpCompleted ->
          zoom appStateLayerFocusL $
            modify (B.focusSetCurrent LayerWallet)
      return AppInProgress
    LayerConfig -> do
      completed <- zoom appStateConfigL $
        CW.handleConfigWidgetEvent AppSelectorConfig ev
      case completed of
        CW.ConfigInProgress -> return ()
        CW.ConfigCompleted ->
          zoom appStateLayerFocusL $
            modify (B.focusSetCurrent LayerWallet)
      return AppInProgress
