module Ariadne.UI.Vty.App
  ( AppState
  , initialAppState
  , app
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List.NonEmpty
import Data.Void
import IiExtras
import Prelude

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.CommandHistory
import Ariadne.UI.Vty.Scrolling

import Ariadne.UI.Vty.Widget.Repl
  (InputModification(..), NavAction(..), CommandAction(..), ReplCompleted(..), ReplInputEvent(..),
  ReplOutputEvent(..), ReplWidgetState(..),
  drawReplInputWidget, drawReplOutputWidget, handleReplInputEvent,
  handleReplOutputEvent, initReplWidget)

import Ariadne.UI.Vty.Widget.Menu
  (MenuWidgetEvent(..), MenuWidgetState, drawMenuWidget, handleMenuWidgetEvent,
  initMenuWidget, menuWidgetSel)

import Ariadne.UI.Vty.Widget.Help
  (HelpWidgetEvent(..), HelpWidgetState, drawHelpWidget, initHelpWidget, handleHelpWidgetEvent)

import Ariadne.UI.Vty.Widget.Logs
  (LogsWidgetEvent(..), LogsWidgetState, drawLogsWidget, initLogsWidget, handleLogsWidgetEvent)

import Ariadne.UI.Vty.Widget.WalletPane
  (WalletPaneWidgetState, drawWalletPaneWidget, initWalletPaneWidget)

import Ariadne.UI.Vty.Widget.WalletTree
  (WalletTreeWidgetState, drawWalletTreeWidget, initWalletTreeWidget)

data AppSelector
  = AppSelectorReplInput
  | AppSelectorReplOutput
  | AppSelectorWalletTree
  | AppSelectorWalletPane
  | AppSelectorHelp
  | AppSelectorLogs
  deriving (Eq)

data AppState =
  AppState
    { appStateRepl :: ReplWidgetState
    , appStateMenu :: MenuWidgetState AppSelector
    , appStateHelp :: HelpWidgetState
    , appStateLogs :: LogsWidgetState
    , appStateWalletTree :: WalletTreeWidgetState
    , appStateWalletPane :: WalletPaneWidgetState
    }

makeLensesWith postfixLFields ''AppState

initialAppState :: UiLangFace -> CommandHistory -> AppState
initialAppState langFace history =
  AppState
    { appStateRepl = initReplWidget langFace history
    , appStateMenu = initMenuWidget appSelectors 0
    , appStateHelp = initHelpWidget
    , appStateLogs = initLogsWidget
    , appStateWalletTree = initWalletTreeWidget
    , appStateWalletPane = initWalletPaneWidget
    }
  where
    appSelectors :: NonEmpty AppSelector
    appSelectors =
      AppSelectorReplInput  :|
      AppSelectorReplOutput :
      AppSelectorWalletTree :
      AppSelectorWalletPane :
      AppSelectorHelp       :
      AppSelectorLogs       : []

data AppCompleted = AppCompleted | AppInProgress

-- The Ariadne UI view and controller a single record.
app :: UiLangFace -> B.App AppState UiEvent Void
app langFace = B.App{..} where

  appDraw :: AppState -> [B.Widget Void]
  appDraw = drawAppWidget

  -- We do not use this feature of Brick.
  appChooseCursor
    :: AppState
    -> [B.CursorLocation Void]
    -> Maybe (B.CursorLocation Void)
  appChooseCursor = B.showFirstCursor

  appHandleEvent
    :: AppState
    -> B.BrickEvent Void UiEvent
    -> B.EventM Void (B.Next AppState)
  appHandleEvent appState ev = do
    (completed, appState') <- liftIO $
      runStateT (handleAppEvent langFace ev) appState
    case completed of
      AppCompleted -> B.halt appState'
      AppInProgress -> B.continue appState'

  -- We do not use this feature of Brick.
  appStartEvent :: AppState -> B.EventM Void AppState
  appStartEvent = return

  -- We do not use this feature of Brick.
  appAttrMap :: AppState -> B.AttrMap
  appAttrMap _ = B.attrMap V.defAttr []

drawAppWidget :: AppState -> [B.Widget Void]
drawAppWidget AppState{..} =
  let
    drawMenu =
      drawMenuWidget
        (\case
          AppSelectorReplInput -> "^R REPL"
          AppSelectorReplOutput -> "^O Output"
          AppSelectorWalletTree -> "^T Tree"
          AppSelectorWalletPane -> "^G Pane"
          AppSelectorHelp -> "^H Help"
          AppSelectorLogs -> "^L Logs")
        appStateMenu
    drawReplInput =
      drawReplInputWidget
        (menuWidgetSel appStateMenu == AppSelectorReplInput)
        appStateRepl
    drawReplOutput =
      drawReplOutputWidget
        (menuWidgetSel appStateMenu == AppSelectorReplOutput)
        appStateRepl
    drawRepl =
      B.vBox
        [ drawReplOutput
        , B.hBorder
        , drawReplInput
        ]
    drawWalletTree =
      drawWalletTreeWidget
        (menuWidgetSel appStateMenu == AppSelectorWalletTree)
        appStateWalletTree
    drawWalletPane =
      drawWalletPaneWidget
        (menuWidgetSel appStateMenu == AppSelectorWalletPane)
        appStateWalletPane
    padLR =
      B.padLeft (B.Pad 1) . B.padRight (B.Pad 1)
    drawDefaultView =
      B.vBox
        [ drawMenu
        , B.hBox
            [ padLR drawWalletTree
            , B.vBorder
            , padLR drawWalletPane
            ]
        , B.hBorder
        , drawRepl
        ]
    drawHelp =
      drawHelpWidget appStateHelp
    drawHelpView =
      B.vBox [drawMenu, drawHelp]
    drawLogs =
      drawLogsWidget appStateLogs
    drawLogsView =
      B.vBox [drawMenu, drawLogs]
  in
    case (menuWidgetSel appStateMenu) of
      AppSelectorHelp -> [drawHelpView]
      AppSelectorLogs -> [drawLogsView]
      _ -> [drawDefaultView]

handleAppEvent
  :: UiLangFace
  -> B.BrickEvent Void UiEvent
  -> StateT AppState IO AppCompleted
handleAppEvent langFace ev = do
  sel <- uses appStateMenuL menuWidgetSel
  case ev of
    B.VtyEvent vtyEv
      | V.EvKey (V.KChar 'c') [V.MCtrl] <- vtyEv ->
          return AppCompleted
      | V.EvKey (V.KChar '\t') [] <- vtyEv -> do
          zoom appStateMenuL $ handleMenuWidgetEvent MenuNextEvent
          return AppInProgress
      | V.EvKey V.KBackTab [] <- vtyEv -> do
          zoom appStateMenuL $ handleMenuWidgetEvent MenuPrevEvent
          return AppInProgress
      | V.EvKey (V.KChar c) [V.MCtrl] <- vtyEv,
        Just appSel <- charAppSel c -> do
          zoom appStateMenuL $ handleMenuWidgetEvent (MenuSelectEvent (==appSel))
          return AppInProgress
      | Just replEv <- toReplInputEv vtyEv,
        AppSelectorReplInput <- sel -> do
          completed <- zoom appStateReplL $
            handleReplInputEvent langFace replEv
          return $ case completed of
            ReplCompleted -> AppCompleted
            ReplInProgress -> AppInProgress
      | Just scrollAction <- eventToScrollingAction vtyEv,
        AppSelectorReplOutput <- sel -> do
            zoom appStateReplL $ handleReplOutputEvent $ ReplOutputScrollingEvent scrollAction
            return AppInProgress
      | Just scrollAction <- eventToScrollingAction vtyEv,
        AppSelectorHelp <- sel -> do
            zoom appStateHelpL $ handleHelpWidgetEvent $ HelpScrollingEvent scrollAction
            return AppInProgress
      | Just scrollAction <- eventToScrollingAction vtyEv,
        AppSelectorLogs <- sel -> do
            zoom appStateLogsL $ handleLogsWidgetEvent $ LogsScrollingEvent scrollAction
            return AppInProgress
    B.AppEvent (UiCommandEvent commandId commandEvent) -> do
        completed <- zoom appStateReplL $
          handleReplInputEvent langFace $
            ReplCommandEvent commandId commandEvent
        return $ case completed of
          ReplCompleted -> AppCompleted
          ReplInProgress -> AppInProgress
    B.AppEvent (UiHelpUpdateData doc) -> do
        zoom appStateHelpL $ handleHelpWidgetEvent $ HelpData doc
        return AppInProgress
    B.AppEvent (UiCardanoLogEvent message) -> do
        zoom appStateLogsL $ handleLogsWidgetEvent $ LogsMessage message
        return AppInProgress
    _ ->
      return AppInProgress

charAppSel :: Char -> Maybe AppSelector
charAppSel = \case
  'r' -> Just AppSelectorReplInput
  'o' -> Just AppSelectorReplOutput
  't' -> Just AppSelectorWalletTree
  'g' -> Just AppSelectorWalletPane
  'h' -> Just AppSelectorHelp
  'l' -> Just AppSelectorLogs
  _ -> Nothing

toReplInputEv :: V.Event -> Maybe ReplInputEvent
toReplInputEv = \case
  V.EvKey V.KLeft [] ->
    Just $ ReplInputNavigationEvent NavArrowLeft
  V.EvKey V.KRight [] ->
    Just $ ReplInputNavigationEvent NavArrowRight
  V.EvKey V.KUp [] ->
    Just $ ReplInputNavigationEvent NavArrowUp
  V.EvKey V.KDown [] ->
    Just $ ReplInputNavigationEvent NavArrowDown
  V.EvKey V.KBS [] ->
    Just $ ReplInputModifyEvent DeleteBackwards
  V.EvKey V.KDel [] ->
    Just $ ReplInputModifyEvent DeleteForwards
  V.EvKey V.KEnter [] ->
    Just $ ReplSmartEnterEvent
  V.EvKey (V.KChar 'd') [V.MCtrl] ->
    Just ReplQuitEvent
  V.EvKey (V.KChar 'n') [V.MCtrl] ->
    Just $ ReplCommandNavigationEvent NextCommand
  V.EvKey (V.KChar 'p') [V.MCtrl] ->
    Just $ ReplCommandNavigationEvent PrevCommand
  V.EvKey (V.KChar c) _ ->
    Just $ ReplInputModifyEvent (InsertChar c)
  _ -> Nothing
