module Ariadne.UI.App
  ( AppState
  , initialAppState
  , app
  , Components
  ) where

import Prelude
import Data.Void
import Data.List.NonEmpty
import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty as V

import Ariadne.Face
import Ariadne.Util

import Ariadne.UI.Widget.Repl
  ( ReplWidgetState, initReplWidget, drawReplOutputWidget, drawReplInputWidget,
    ReplCompleted(..), handleReplWidgetEvent, ReplWidgetEvent(..),
    NavAction(..), InputModification(..), Components )

import Ariadne.UI.Widget.Menu
  ( MenuWidgetState, initMenuWidget, drawMenuWidget, menuWidgetSel,
    handleMenuWidgetEvent, MenuWidgetEvent(..) )

import Ariadne.UI.Widget.Help
  ( HelpWidgetState, initHelpWidget, drawHelpWidget )

import Ariadne.UI.Widget.Logs
  ( LogsWidgetState, initLogsWidget, drawLogsWidget )

import Ariadne.UI.Widget.WalletPane
  ( WalletPaneWidgetState, initWalletPaneWidget, drawWalletPaneWidget )

import Ariadne.UI.Widget.WalletTree
  ( WalletTreeWidgetState, initWalletTreeWidget, drawWalletTreeWidget )

data AppSelector
  = AppSelectorReplInput
  | AppSelectorReplOutput
  | AppSelectorWalletTree
  | AppSelectorWalletPane
  | AppSelectorHelp
  | AppSelectorLogs
  deriving (Eq)

data AppState components =
  AppState
    { appStateRepl :: ReplWidgetState components
    , appStateMenu :: MenuWidgetState AppSelector
    , appStateHelp :: HelpWidgetState
    , appStateLogs :: LogsWidgetState
    , appStateWalletTree :: WalletTreeWidgetState
    , appStateWalletPane :: WalletPaneWidgetState
    }

makeLensesWith postfixLFields ''AppState

initialAppState :: Components components => AppState components
initialAppState =
  AppState
    { appStateRepl = initReplWidget
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
app
  :: forall components.
     Components components
  => KnitFace components
  -> B.App (AppState components) (UiEvent components) Void
app knitFace = B.App{..} where

  appDraw
    :: AppState components
    -> [B.Widget Void]
  appDraw = drawAppWidget

  -- We do not use this feature of Brick.
  appChooseCursor
    :: AppState components
    -> [B.CursorLocation Void]
    -> Maybe (B.CursorLocation Void)
  appChooseCursor = B.showFirstCursor

  appHandleEvent
    :: AppState components
    -> B.BrickEvent Void (UiEvent components)
    -> B.EventM Void (B.Next (AppState components))
  appHandleEvent appState ev = do
    (completed, appState') <- liftIO $
      runStateT (handleAppEvent knitFace ev) appState
    case completed of
      AppCompleted -> B.halt appState'
      AppInProgress -> B.continue appState'

  -- We do not use this feature of Brick.
  appStartEvent :: AppState components -> B.EventM Void (AppState components)
  appStartEvent = return

  -- We do not use this feature of Brick.
  appAttrMap :: AppState components -> B.AttrMap
  appAttrMap _ = B.attrMap V.defAttr []

drawAppWidget
    :: AppState components
    -> [B.Widget Void]
drawAppWidget AppState{..} =
  let
    drawMenu =
      drawMenuWidget
        (\case
          AppSelectorReplInput -> "^R REPL"
          AppSelectorReplOutput -> "^O Output"
          AppSelectorWalletTree -> "^T Tree"
          AppSelectorWalletPane -> "^P Pane"
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
  :: Components components
  => KnitFace components
  -> B.BrickEvent Void (UiEvent components)
  -> StateT (AppState components) IO AppCompleted
handleAppEvent knitFace ev = do
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
      | Just replEv <- toReplEv vtyEv,
        AppSelectorReplInput <- sel -> do
          completed <- zoom appStateReplL $
            handleReplWidgetEvent knitFace replEv
          return $ case completed of
            ReplCompleted -> AppCompleted
            ReplInProgress -> AppInProgress
    B.AppEvent (UiKnitEvent (KnitResultEvent commandId commandResult)) -> do
        completed <- zoom appStateReplL $
          handleReplWidgetEvent knitFace $
            ReplCommandResultEvent commandId commandResult
        return $ case completed of
          ReplCompleted -> AppCompleted
          ReplInProgress -> AppInProgress
    _ ->
      return AppInProgress

charAppSel :: Char -> Maybe AppSelector
charAppSel = \case
  'r' -> Just AppSelectorReplInput
  'o' -> Just AppSelectorReplOutput
  't' -> Just AppSelectorWalletTree
  'p' -> Just AppSelectorWalletPane
  'h' -> Just AppSelectorHelp
  'l' -> Just AppSelectorLogs
  _ -> Nothing

toReplEv :: V.Event -> Maybe (ReplWidgetEvent components)
toReplEv = \case
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
  V.EvKey (V.KChar c) _ ->
    Just $ ReplInputModifyEvent (InsertChar c)
  _ -> Nothing
