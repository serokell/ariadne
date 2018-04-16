module Ariadne.UI.Vty.App
  ( AppState
  , initialAppState
  , app
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void
import IiExtras
import Prelude

import qualified Brick as B
import qualified Brick.Widgets.Border as B

import Ariadne.UI.Vty.CommandHistory
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
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
  = AppSelectorWallet
  | AppSelectorHelp
  | AppSelectorLogs
  deriving (Eq)

data AppFocus
  = AppFocusMenu
  | AppFocusWalletTree
  | AppFocusWalletPane
  | AppFocusRepl
  | AppFocusHelp
  | AppFocusLogs
  deriving (Eq)

data AppState =
  AppState
    { appStateFocus :: AppFocus
    , appStateEditorMode :: Bool

    , appStateRepl :: ReplWidgetState
    , appStateMenu :: MenuWidgetState AppSelector
    , appStateStatus :: StatusWidgetState
    , appStateHelp :: HelpWidgetState
    , appStateLogs :: LogsWidgetState
    , appStateWalletTree :: WalletTreeWidgetState
    , appStateWalletPane :: WalletPaneWidgetState
    }

makeLensesWith postfixLFields ''AppState

initialAppState :: UiLangFace -> CommandHistory -> AppState
initialAppState langFace history =
  AppState
    { appStateFocus = AppFocusRepl
    , appStateEditorMode = False

    , appStateRepl = initReplWidget langFace history
    , appStateMenu = initMenuWidget appSelectors 0
    , appStateStatus = initStatusWidget
    , appStateHelp = initHelpWidget
    , appStateLogs = initLogsWidget
    , appStateWalletTree = initWalletTreeWidget
    , appStateWalletPane = initWalletPaneWidget
    }
  where
    appSelectors :: NonEmpty (MenuWidgetElem AppSelector)
    appSelectors
      = MenuWidgetElem AppSelectorWallet "Wallet" 'w' :|
      [ MenuWidgetElem AppSelectorHelp "Help" 'h'
      , MenuWidgetElem AppSelectorLogs "Logs" 'l'
      ]

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

  appAttrMap :: AppState -> B.AttrMap
  appAttrMap = const defaultAttrMap

drawAppWidget :: AppState -> [B.Widget Void]
drawAppWidget AppState{..} =
  let
    defAttr :: B.AttrName
    defAttr = "default"
    focusAttr :: AppFocus -> B.AttrName
    focusAttr focus =
      if not (appStateMenu ^. menuWidgetNavModeL) && appStateFocus == focus
        then "focused"
        else defAttr

    -- Widgets don't always fill the screen, so we need a background widget
    -- in case default terminal background differs from our theme background
    drawBG = B.withAttr defAttr $ B.fill ' '
    drawMenu = drawMenuWidget appStateMenu
    drawStatus = drawStatusWidget appStateStatus
    drawReplInput =
      B.withAttr (focusAttr AppFocusRepl) $
        drawReplInputWidget appStateEditorMode appStateRepl
    drawReplOutput = drawReplOutputWidget appStateRepl
    drawRepl =
      B.vBox
        [ drawReplOutput
        , B.hBorder
        , drawReplInput
        ]
    drawWalletTree =
      B.withAttr (focusAttr AppFocusWalletTree) $
        drawWalletTreeWidget appStateWalletTree
    drawWalletPane =
      B.withAttr (focusAttr AppFocusWalletPane) $
        drawWalletPaneWidget appStateWalletPane
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
        , B.hBorder
        , drawReplInput
        , drawStatus
        ]
    drawLogs =
      drawLogsWidget appStateLogs
    drawLogsView =
      B.withAttr defAttr $ B.vBox
        [ drawMenu
        , drawLogs
        , B.hBorder
        , drawReplInput
        , drawStatus
        ]
  in
    case menuWidgetSel appStateMenu of
      AppSelectorHelp -> [drawHelpView, drawBG]
      AppSelectorLogs -> [drawLogsView, drawBG]
      _ -> [drawDefaultView, drawBG]

handleAppEvent
  :: UiLangFace
  -> B.BrickEvent Void UiEvent
  -> StateT AppState IO AppCompleted
handleAppEvent langFace ev =
  case ev of
    B.VtyEvent vtyEv -> do
      menuState <- use appStateMenuL
      focus <- use appStateFocusL
      editorModeEnabled <- use appStateEditorModeL
      replEmpty <- uses appStateReplL replWidgetEmpty
      let
        sel = menuWidgetSel menuState
        navMode = menuState ^. menuWidgetNavModeL
        key = vtyToKey vtyEv
        editKey = vtyToEditKey vtyEv
      if
        | KeyExit <- key ->
            return AppCompleted

        -- Navigation mode related events
        | navMode ->
            case keyToMenuWidgetEvent menuState key of
              Just event -> do
                zoom appStateMenuL $ handleMenuWidgetEvent event
                newSel <- uses appStateMenuL menuWidgetSel
                appStateFocusL .= restoreFocus newSel focus
                return AppInProgress
              Nothing -> do
                zoom appStateMenuL $ handleMenuWidgetEvent MenuExitEvent
                -- Handle event once again in non-nav mode
                handleAppEvent langFace ev
        | KeyNavigation <- key -> do
            zoom appStateMenuL $ handleMenuWidgetEvent MenuEnterEvent
            appStateEditorModeL .= False
            return AppInProgress

        -- Switch focus between widgets, unless we do autocomplete
        | key `elem` [KeyFocusNext, KeyFocusPrev],
          not editorModeEnabled || replEmpty || editKey == KeyUnknown -> do
            appStateEditorModeL .= False
            appStateFocusL .= rotateFocus sel focus (key == KeyFocusPrev)
            return AppInProgress

        -- REPL in editor mode
        | Just replEv <- keyToReplInputEvent editKey,
          AppFocusRepl <- focus,
          editorModeEnabled -> do
            completed <- zoom appStateReplL $
              handleReplInputEvent langFace replEv
            return $ case completed of
              ReplCompleted -> AppCompleted
              ReplInProgress -> AppInProgress

        -- Scroll widgets while REPL is in focus
        | Just scrollAction <- eventToScrollingAction key,
          AppFocusRepl <- focus -> do
            case sel of
              AppSelectorWallet ->
                zoom appStateReplL $ handleReplOutputEvent $
                  ReplOutputScrollingEvent scrollAction
              AppSelectorHelp ->
                zoom appStateHelpL $ handleHelpWidgetEvent $
                  HelpScrollingEvent scrollAction
              AppSelectorLogs ->
                zoom appStateLogsL $ handleLogsWidgetEvent $
                  LogsScrollingEvent scrollAction
            return AppInProgress

        -- Enter REPL editor mode
        | KeyEnter <- key,
          AppFocusRepl <- focus -> do
            appStateEditorModeL .= True
            return AppInProgress
        | Just replEv <- keyToReplInputEvent editKey,
          AppFocusRepl <- focus -> do
            appStateEditorModeL .= True
            completed <- zoom appStateReplL $
              handleReplInputEvent langFace replEv
            return $ case completed of
              ReplCompleted -> AppCompleted
              ReplInProgress -> AppInProgress

        | otherwise ->
            return AppInProgress
    B.AppEvent (UiWalletEvent walletEvent) -> do
      case walletEvent of
        UiWalletUpdate{..} -> do
          zoom appStateWalletTreeL $
            handleWalletTreeWidgetEvent $
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

focusesBySel :: AppSelector -> [AppFocus]
focusesBySel = \case
  AppSelectorWallet -> [AppFocusWalletTree, AppFocusWalletPane, AppFocusRepl]
  AppSelectorHelp -> [AppFocusRepl]
  AppSelectorLogs -> [AppFocusRepl]

rotateFocus :: AppSelector -> AppFocus -> Bool -> AppFocus
rotateFocus selector focus back = dropWhile (/= focus) focuses !! 1
  where
    focuses = cycle $ (if back then reverse . focusesBySel else focusesBySel) selector

restoreFocus :: AppSelector -> AppFocus -> AppFocus
restoreFocus selector focus =
  if focus `elem` focuses then focus else head focuses
  where focuses = focusesBySel selector
