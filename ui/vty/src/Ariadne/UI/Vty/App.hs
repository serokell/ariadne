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
  = AppFocusWalletTree
  | AppFocusWalletPane
  | AppFocusReplOutput
  | AppFocusReplInput
  | AppFocusHelp
  | AppFocusLogs
  deriving (Eq)

data AppBrickName
  = AppBrickReplOutput
  | AppBrickHelp
  | AppBrickLogs
  deriving (Eq, Ord, Show)

data AppState =
  AppState
    { appStateFocus :: AppFocus
    , appStateRepl :: ReplWidgetState AppBrickName
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
    { appStateFocus = AppFocusReplInput
    , appStateRepl = initReplWidget langFace history AppBrickReplOutput
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
      [ MenuWidgetElem AppSelectorWallet "Wallet" 'w'
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
    focusAttr :: AppFocus -> B.AttrName
    focusAttr focus =
      if not appStateNavigationMode && appStateFocus == focus
        then "focused"
        else defAttr

    -- Widgets don't always fill the screen, so we need a background widget
    -- in case default terminal background differs from our theme background
    drawBG = B.withAttr defAttr $ B.fill ' '
    drawMenu = drawMenuWidget appStateNavigationMode appStateMenu
    drawStatus = drawStatusWidget appStateStatus
    drawReplInput =
      B.withAttr (focusAttr AppFocusReplInput) $
        drawReplInputWidget
          (appStateFocus == AppFocusReplInput)
          appStateRepl
    drawReplOutput =
      drawReplOutputWidget
        (appStateFocus == AppFocusReplOutput)
        appStateRepl
    drawRepl =
      B.vBox
        [ drawReplOutput
        , B.hBorder
        , drawReplInput
        ]
    drawWalletTree =
      B.withAttr (focusAttr AppFocusWalletTree) $
      drawWalletTreeWidget
        (appStateFocus == AppFocusWalletTree)
        appStateWalletTree
    drawWalletPane =
      B.withAttr (focusAttr AppFocusWalletPane) $
      drawWalletPaneWidget
        (appStateFocus == AppFocusWalletPane)
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
handleAppEvent langFace ev =
  case ev of
    B.VtyEvent vtyEv -> do
      sel <- uses appStateMenuL menuWidgetSel
      focus <- use appStateFocusL
      navModeEnabled <- use appStateNavigationModeL
      let
        key = vtyToKey vtyEv
        editKey = vtyToEditKey vtyEv
      if
        | KeyQuit <- key ->
            return AppCompleted

        -- Switch focus between widgets
        | key `elem` [KeyFocusNext, KeyFocusPrev] -> do
            appStateNavigationModeL .= False
            appStateFocusL .= rotateFocus sel focus (key == KeyFocusPrev)
            return AppInProgress

        -- Navigation mode related events
        | KeyRight <- key,
          navModeEnabled -> do
            zoom appStateMenuL $ handleMenuWidgetEvent MenuNextEvent
            return AppInProgress
        | KeyLeft <- key,
          navModeEnabled -> do
            zoom appStateMenuL $ handleMenuWidgetEvent MenuPrevEvent
            return AppInProgress
        | KeyChar c <- key,
          navModeEnabled,
          Just appSel <- charAppSel c -> do
            appStateNavigationModeL .= False
            zoom appStateMenuL $ handleMenuWidgetEvent (MenuSelectEvent (==appSel))
            appStateFocusL .= restoreFocus appSel focus
            return AppInProgress
        | KeyNavigation <- key -> do
            appStateNavigationModeL .= not navModeEnabled
            appStateFocusL .= restoreFocus sel focus
            return AppInProgress
        | navModeEnabled -> return AppInProgress

        -- REPL editor events
        | Just replEv <- keyToReplInputEvent editKey,
          AppFocusReplInput <- focus -> do
            completed <- zoom appStateReplL $
              handleReplInputEvent langFace replEv
            return $ case completed of
              ReplCompleted -> AppCompleted
              ReplInProgress -> AppInProgress

        -- Scrolling events
        | Just scrollAction <- keyToScrollingAction key,
          focus `elem` [AppFocusReplInput, AppFocusReplOutput]-> do
            zoom appStateReplL $ handleReplOutputEvent $ ReplOutputScrollingEvent scrollAction
            return AppInProgress
        | Just scrollAction <- keyToScrollingAction key,
          AppFocusHelp <- focus -> do
            zoom appStateHelpL $ handleHelpWidgetEvent $ HelpScrollingEvent scrollAction
            return AppInProgress
        | Just scrollAction <- keyToScrollingAction key,
          AppFocusLogs <- focus -> do
            zoom appStateLogsL $ handleLogsWidgetEvent $ LogsScrollingEvent scrollAction
            return AppInProgress

        -- Widget-specific events
        | Just walletTreeEv <- keyToWalletTreeEvent key,
          AppFocusWalletTree <- focus -> do
            zoom appStateWalletTreeL $ handleWalletTreeWidgetEvent langFace walletTreeEv
            return AppInProgress

        | otherwise ->
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
  'w' -> Just AppSelectorWallet
  'h' -> Just AppSelectorHelp
  'l' -> Just AppSelectorLogs
  _ -> Nothing

focusesBySel :: AppSelector -> NE.NonEmpty AppFocus
focusesBySel sel = NE.fromList $ case sel of
  AppSelectorWallet -> [AppFocusReplInput, AppFocusWalletTree, AppFocusWalletPane, AppFocusReplOutput]
  AppSelectorHelp -> [AppFocusHelp]
  AppSelectorLogs -> [AppFocusLogs]

rotateFocus :: AppSelector -> AppFocus -> Bool -> AppFocus
rotateFocus selector focus back = NE.dropWhile (/= focus) focuses !! 1
  where
    focuses = NE.cycle $ (if back then NE.reverse . focusesBySel else focusesBySel) selector

restoreFocus :: AppSelector -> AppFocus -> AppFocus
restoreFocus selector focus =
  if focus `elem` focuses then focus else NE.head focuses
  where focuses = focusesBySel selector