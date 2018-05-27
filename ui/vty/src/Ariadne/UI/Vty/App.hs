module Ariadne.UI.Vty.App
  ( AppState
  , initialAppState
  , app
  ) where

import Universum

import Control.Lens (makeLensesWith, uses, zoom, (.=))
import Data.List ((!!))
import IiExtras

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Data.List.NonEmpty as NE
import qualified Graphics.Vty as V

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

-- | Selected menu item and, consequently, visible screen
data AppSelector
  = AppSelectorWallet
  | AppSelectorHelp
  | AppSelectorLogs
  deriving (Eq)

-- | Currently focused widget
data AppFocus
  = AppFocusWalletTree
  | AppFocusWalletPane
  | AppFocusReplOutput
  | AppFocusReplInput
  | AppFocusHelp
  | AppFocusLogs
  deriving (Eq)

-- | Brick-specific ID for scrolling viewports, widget cache items and clickable widgets
data AppBrickName
  = AppBrickMenu
  | AppBrickWalletTree
  | AppBrickWalletPane
  | AppBrickReplOutput
  | AppBrickReplInput
  | AppBrickHelp
  | AppBrickLogs
  deriving (Eq, Ord, Show)

data AppState =
  AppState
    { appStateFocus :: AppFocus
    , appStateRepl :: ReplWidgetState AppBrickName
    , appStateMenu :: MenuWidgetState AppSelector AppBrickName
    , appStateStatus :: StatusWidgetState
    , appStateHelp :: HelpWidgetState AppBrickName
    , appStateLogs :: LogsWidgetState AppBrickName
    , appStateWalletTree :: WalletTreeWidgetState AppBrickName
    , appStateWalletPane :: WalletPaneWidgetState AppBrickName
    }

makeLensesWith postfixLFields ''AppState

initialAppState :: UiLangFace -> CommandHistory -> AppState
initialAppState langFace history =
  AppState
    { appStateFocus = AppFocusReplInput
    , appStateRepl = initReplWidget langFace history AppBrickReplOutput AppBrickReplInput
    , appStateMenu = initMenuWidget menuItems 0 AppBrickMenu
    , appStateStatus = initStatusWidget
    , appStateHelp = initHelpWidget langFace AppBrickHelp
    , appStateLogs = initLogsWidget AppBrickLogs
    , appStateWalletTree = initWalletTreeWidget AppBrickWalletTree
    , appStateWalletPane = initWalletPaneWidget AppBrickWalletPane
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
    navMode = menuWidgetNavMode appStateMenu
    defAttr :: B.AttrName
    defAttr = "default"
    focusAttr :: AppFocus -> B.AttrName
    focusAttr focus
      | not navMode, appStateFocus == focus
          = "focused"
      | otherwise
          = defAttr
    focusIndicator :: AppFocus -> B.Widget name
    focusIndicator focus
      | not navMode, appStateFocus == focus
          = B.withAttr "focus" $ B.txt "•"
      | navMode
          = B.withAttr "focus.key" $ B.txt $
            case focus of
              AppFocusWalletTree -> "T"
              AppFocusWalletPane -> "P"
              AppFocusReplOutput -> "O"
              AppFocusReplInput  -> "R"
              _                  -> " "
      | otherwise
          = B.txt " "
    withFocus :: AppFocus -> B.Widget name -> B.Widget name
    withFocus focus widget =
      B.hBox
        [ focusIndicator focus
        , B.withAttr (focusAttr focus) $ widget
        ]

    -- Widgets don't always fill the screen, so we need a background widget
    -- in case default terminal background differs from our theme background
    drawBG = B.withAttr defAttr $ B.fill ' '
    drawMenu = drawMenuWidget appStateMenu
    drawStatus = drawStatusWidget appStateStatus
    drawReplInput =
      withFocus AppFocusReplInput $
      drawReplInputWidget
        (appStateFocus == AppFocusReplInput)
        appStateRepl
    drawReplOutput =
      withFocus AppFocusReplOutput $
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
      B.padTop (B.Pad 1) $ B.padRight (B.Pad 1) $
      withFocus AppFocusWalletTree $
      drawWalletTreeWidget
        (appStateFocus == AppFocusWalletTree)
        appStateWalletTree
    drawWalletPane =
      B.padTop (B.Pad 1) $ B.padRight (B.Pad 1) $
      withFocus AppFocusWalletPane $
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
    B.VtyEvent (V.EvPaste bs) -> do
      whenRight (decodeUtf8' bs) $ \pasted ->
        void $ zoom appStateReplL $
          handleReplInputEvent langFace $
            ReplInputModifyEvent (InsertMany pasted)
      return AppInProgress
    B.VtyEvent vtyEv -> do
      focus <- use appStateFocusL
      menuState <- use appStateMenuL
      replState <- use appStateReplL
      let
        sel = menuWidgetSel menuState
        navMode = menuWidgetNavMode menuState
        key = vtyToKey vtyEv
        editKey = vtyToEditKey vtyEv
      if
        -- Navigation mode related events
        | navMode,
          KeyChar c <- key,
          Just (newSel, newFocus) <- charToFocus c -> do
            zoom appStateMenuL $ handleMenuWidgetEvent $ MenuSelectEvent (== newSel)
            appStateFocusL .= newFocus
            return AppInProgress
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
            return AppInProgress

        -- Switch focus between widgets
        | key `elem` [KeyFocusNext, KeyFocusPrev] -> do
            appStateFocusL .= rotateFocus sel focus (key == KeyFocusPrev)
            return AppInProgress

        -- REPL editor events
        | Just replEv <- keyToReplInputEvent replState editKey,
          AppFocusReplInput <- focus -> do
            completed <- zoom appStateReplL $
              handleReplInputEvent langFace replEv
            return $ case completed of
              ReplCompleted -> AppCompleted
              ReplInProgress -> AppInProgress

        -- This one is here, so that REPL can intercept it and cancel current command
        | KeyQuit <- key ->
            return AppCompleted

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

        | Just walletPaneEv <- keyToWalletPaneEvent key,
          AppFocusWalletPane <- focus -> do
            zoom appStateWalletPaneL $ handleWalletPaneWidgetEvent langFace walletPaneEv
            return AppInProgress

        | otherwise ->
            return AppInProgress
    B.MouseDown name V.BLeft [] coords ->
      case name of
        AppBrickMenu -> do
          zoom appStateMenuL $ handleMenuWidgetEvent $
            MenuMouseDownEvent coords
          newSel <- uses appStateMenuL menuWidgetSel
          focus <- use appStateFocusL
          appStateFocusL .= restoreFocus newSel focus
          return AppInProgress
        AppBrickWalletTree -> do
          appStateFocusL .= AppFocusWalletTree
          zoom appStateWalletTreeL $ handleWalletTreeWidgetEvent langFace $
            WalletTreeMouseDownEvent coords
          return AppInProgress
        AppBrickWalletPane -> do
          appStateFocusL .= AppFocusWalletPane
          zoom appStateWalletPaneL $ handleWalletPaneWidgetEvent langFace $
            WalletPaneMouseDownEvent coords
          return AppInProgress
        AppBrickReplOutput -> do
          appStateFocusL .= AppFocusReplOutput
          return AppInProgress
        AppBrickReplInput -> do
          appStateFocusL .= AppFocusReplInput
          void $ zoom appStateReplL $ handleReplInputEvent langFace $
            ReplMouseDownEvent coords
          return AppInProgress
        _ ->
          return AppInProgress
    B.MouseDown name button [] _
      | Just scrollAction <- buttonToScrollAction button -> do
          case name of
            AppBrickWalletTree ->
              zoom appStateWalletTreeL $ handleWalletTreeWidgetEvent langFace $
                WalletTreeScrollingEvent scrollAction
            AppBrickReplOutput ->
              zoom appStateReplL $ handleReplOutputEvent $
                ReplOutputScrollingEvent scrollAction
            AppBrickHelp ->
              zoom appStateHelpL $ handleHelpWidgetEvent $
                HelpScrollingEvent scrollAction
            AppBrickLogs ->
              zoom appStateLogsL $ handleLogsWidgetEvent $
                LogsScrollingEvent scrollAction
            _ ->
              return ()
          return AppInProgress
    B.AppEvent (UiWalletEvent walletEvent) -> do
      case walletEvent of
        UiWalletUpdate{..} -> do
          zoom appStateWalletTreeL $
            handleWalletTreeWidgetEvent langFace $
              WalletTreeUpdateEvent wuTrees wuSelection
          zoom appStateWalletPaneL $
            handleWalletPaneWidgetEvent langFace  $
              WalletPaneUpdateEvent wuPaneInfoUpdate
      return AppInProgress
    B.AppEvent (UiCommandResultEvent commandId commandEvent) -> do
        zoom appStateWalletPaneL $
          handleWalletPaneCommandEvent $
            WalletPaneCommandEvent commandId commandEvent
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
    B.AppEvent (UiNewVersionEvent ver) -> do
      zoom appStateStatusL $
        handleStatusWidgetEvent $
          StatusNewVersionEvent ver
      return AppInProgress
    B.AppEvent (UiCommandEvent commandEvent) -> do
      case commandEvent of
        UiCommandHelp -> do
          focus <- use appStateFocusL
          zoom appStateMenuL $ handleMenuWidgetEvent $ MenuSelectEvent (== AppSelectorHelp)
          appStateFocusL .= restoreFocus AppSelectorHelp focus
          return AppInProgress
        UiCommandLogs -> do
          focus <- use appStateFocusL
          zoom appStateMenuL $ handleMenuWidgetEvent $ MenuSelectEvent (== AppSelectorLogs)
          appStateFocusL .= restoreFocus AppSelectorLogs focus
          return AppInProgress
    _ ->
      return AppInProgress

buttonToScrollAction :: V.Button -> Maybe ScrollingAction
buttonToScrollAction = \case
  V.BScrollUp -> Just ScrollingLineUp
  V.BScrollDown -> Just ScrollingLineDown
  _ -> Nothing

charToFocus :: Char -> Maybe (AppSelector, AppFocus)
charToFocus = \case
  't' -> Just (AppSelectorWallet, AppFocusWalletTree)
  'p' -> Just (AppSelectorWallet, AppFocusWalletPane)
  'o' -> Just (AppSelectorWallet, AppFocusReplOutput)
  'r' -> Just (AppSelectorWallet, AppFocusReplInput)
  _   -> Nothing

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
