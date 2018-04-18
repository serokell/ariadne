module Ariadne.UI.Vty.App
  ( AppState
  , initialAppState
  , app
  ) where

import Control.Lens
import Control.Monad.Trans.State
import IiExtras
import Prelude

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Data.List.NonEmpty as NE
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.CommandHistory
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Theme
import Ariadne.UI.Vty.Widget.Help
import Ariadne.UI.Vty.Widget.Logs
import Ariadne.UI.Vty.Widget.Menu
import Ariadne.UI.Vty.Widget.Repl
import Ariadne.UI.Vty.Widget.Status
import Ariadne.UI.Vty.Widget.WalletPane
import Ariadne.UI.Vty.Widget.WalletTree

data AppSelector
  = AppSelectorReplInput
  | AppSelectorReplOutput
  | AppSelectorWalletTree
  | AppSelectorWalletPane
  | AppSelectorHelp
  | AppSelectorLogs
  deriving (Eq)

data AppBrickName
  = AppBrickReplOutput
  | AppBrickHelp
  | AppBrickLogs
  deriving (Eq, Ord, Show)

data AppState =
  AppState
    { appStateRepl :: ReplWidgetState AppBrickName
    , appStateMenu :: MenuWidgetState AppSelector
    , appStateStatus :: StatusWidgetState
    , appStateNavigationMode :: Bool
    , appStateHelp :: HelpWidgetState AppBrickName
    , appStateLogs :: LogsWidgetState AppBrickName
    , appStateWalletTree :: WalletTreeWidgetState
    , appStateWalletPane :: WalletPaneWidgetState
    }

makeLensesWith postfixLFields ''AppState

initialAppState :: UiLangFace -> CommandHistory -> AppState
initialAppState langFace history =
  AppState
    { appStateRepl = initReplWidget langFace history AppBrickReplOutput
    , appStateMenu = initMenuWidget menuItems 0
    , appStateStatus = initStatusWidget
    , appStateNavigationMode = False
    , appStateHelp = initHelpWidget AppBrickHelp
    , appStateLogs = initLogsWidget AppBrickLogs
    , appStateWalletTree = initWalletTreeWidget
    , appStateWalletPane = initWalletPaneWidget
    }
  where
    menuItems :: NE.NonEmpty (MenuWidgetElem AppSelector)
    menuItems = NE.fromList
      [ MenuWidgetElem AppSelectorReplInput "REPL" 'r'
      , MenuWidgetElem AppSelectorReplOutput "Output" 'o'
      , MenuWidgetElem AppSelectorWalletTree "Tree" 't'
      , MenuWidgetElem AppSelectorWalletPane "Pane" 'p'
      , MenuWidgetElem AppSelectorHelp "Help" 'h'
      , MenuWidgetElem AppSelectorLogs "Logs" 'l'
      ]

data AppCompleted = AppCompleted | AppInProgress

-- The Ariadne UI view and controller a single record.
app :: UiLangFace -> B.App AppState UiEvent AppBrickName
app langFace = B.App{..} where

  appDraw :: AppState -> [B.Widget AppBrickName]
  appDraw = drawAppWidget

  -- We do not use this feature of Brick.
  appChooseCursor
    :: AppState
    -> [B.CursorLocation AppBrickName]
    -> Maybe (B.CursorLocation AppBrickName)
  appChooseCursor = B.showFirstCursor

  appHandleEvent
    :: AppState
    -> B.BrickEvent AppBrickName UiEvent
    -> B.EventM AppBrickName (B.Next AppState)
  appHandleEvent appState ev = do
    (completed, appState') <-
      runStateT (handleAppEvent langFace ev) appState
    case completed of
      AppCompleted -> B.halt appState'
      AppInProgress -> B.continue appState'

  -- We do not use this feature of Brick.
  appStartEvent :: AppState -> B.EventM AppBrickName AppState
  appStartEvent = return

  appAttrMap :: AppState -> B.AttrMap
  appAttrMap = const defaultAttrMap

drawAppWidget :: AppState -> [B.Widget AppBrickName]
drawAppWidget AppState{..} =
  let
    defAttr :: B.AttrName
    defAttr = "default"

    -- Widgets don't always fill the screen, so we need a background widget
    -- in case default terminal background differs from our theme background
    drawBG = B.withAttr defAttr $ B.fill ' '
    drawMenu = drawMenuWidget appStateNavigationMode appStateMenu
    drawStatus = drawStatusWidget appStateStatus
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
    drawDefaultView =
      B.withAttr defAttr $ B.vBox
        [ drawMenu
        , B.hBox
            [ drawWalletTree
            , B.joinBorders B.vBorder
            , drawWalletPane
            ]
        , B.joinBorders B.hBorder
        , drawRepl
        , drawStatus
        ]
    drawHelp =
      drawHelpWidget appStateHelp
    drawHelpView =
      B.withAttr defAttr $ B.vBox
        [ drawMenu
        , drawHelp
        , drawStatus
        ]
    drawLogs =
      drawLogsWidget appStateLogs
    drawLogsView =
      B.withAttr defAttr $ B.vBox
        [ drawMenu
        , drawLogs
        , drawStatus
        ]
  in
    case menuWidgetSel appStateMenu of
      AppSelectorHelp -> [drawHelpView, drawBG]
      AppSelectorLogs -> [drawLogsView, drawBG]
      _ -> [drawDefaultView, drawBG]

handleAppEvent
  :: UiLangFace
  -> B.BrickEvent AppBrickName UiEvent
  -> StateT AppState (B.EventM AppBrickName) AppCompleted
handleAppEvent langFace ev = do
  sel <- uses appStateMenuL menuWidgetSel
  navModeEnabled <- use appStateNavigationModeL
  case ev of
    B.VtyEvent vtyEv
      | V.EvKey (V.KChar 'c') [V.MCtrl] <- vtyEv ->
          return AppCompleted
      | V.EvKey (V.KChar '\t') [] <- vtyEv,
        navModeEnabled -> do
          zoom appStateMenuL $ handleMenuWidgetEvent MenuNextEvent
          return AppInProgress
      | V.EvKey V.KBackTab [] <- vtyEv,
        navModeEnabled -> do
          zoom appStateMenuL $ handleMenuWidgetEvent MenuPrevEvent
          return AppInProgress
      | V.EvKey (V.KChar c) [] <- vtyEv,
        navModeEnabled,
        Just appSel <- charAppSel c -> do
          appStateNavigationModeL .= False
          zoom appStateMenuL $ handleMenuWidgetEvent (MenuSelectEvent (==appSel))
          return AppInProgress
      | V.EvKey (V.KChar 'g') [V.MCtrl] <- vtyEv ->
        do
            appStateNavigationModeL .= True
            return AppInProgress
      | navModeEnabled -> return AppInProgress
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
      | Just walletTreeEv <- toWalletTreeEv vtyEv,
        AppSelectorWalletTree <- sel -> do
            zoom appStateWalletTreeL $ handleWalletTreeWidgetEvent langFace walletTreeEv
            return AppInProgress
    B.AppEvent (UiWalletEvent walletEvent) -> do
      case walletEvent of
        UiWalletUpdate{..} -> do
          zoom appStateWalletTreeL $
            handleWalletTreeWidgetEvent langFace $
              WalletTreeUpdateEvent wuTrees wuSelection
          zoom appStateWalletPaneL $
            handleWalletPaneWidgetEvent $
              WalletPaneUpdateEvent wuPaneInfo
      return AppInProgress
    B.AppEvent (UiCommandEvent commandId commandEvent) -> do
        completed <- zoom appStateReplL $
          handleReplInputEvent langFace $
            ReplCommandEvent commandId commandEvent
        return $ case completed of
          ReplCompleted -> AppCompleted
          ReplInProgress -> AppInProgress
    B.AppEvent (UiCardanoEvent cardanoEvent) -> do
      case cardanoEvent of
        UiCardanoLogEvent message ->
          zoom appStateLogsL $
            handleLogsWidgetEvent $
              LogsMessage message
        UiCardanoStatusUpdateEvent statusUpdate ->
          zoom appStateStatusL $
            handleStatusWidgetEvent $
              StatusUpdateEvent statusUpdate
      return AppInProgress
    B.AppEvent (UiHelpUpdateData doc) -> do
        zoom appStateHelpL $ handleHelpWidgetEvent $ HelpData doc
        return AppInProgress
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
  V.EvKey (V.KChar 'w') [V.MCtrl] ->
    Just $ ReplInputModifyEvent DeleteWordBackwards
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

toWalletTreeEv :: V.Event -> Maybe WalletTreeWidgetEvent
toWalletTreeEv = \case
  V.EvKey V.KUp [] -> Just WalletNavigationUp
  V.EvKey V.KDown [] -> Just WalletNavigationDown
  V.EvKey V.KLeft [] -> Just WalletNavigationLeft
  V.EvKey V.KRight [] -> Just WalletNavigationRight
  _ -> Nothing
