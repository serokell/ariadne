module Ariadne.UI.Vty.App
  ( AppState
  , initialAppState
  , app
  ) where

import Control.Lens
import Control.Monad.Trans.State
import Data.List.NonEmpty
import IiExtras
import Prelude

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.CommandHistory
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling
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
    , appStateMenu = initMenuWidget appSelectors 0
    , appStateStatus = initStatusWidget
    , appStateNavigationMode = False
    , appStateHelp = initHelpWidget AppBrickHelp
    , appStateLogs = initLogsWidget AppBrickLogs
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

  -- We do not use this feature of Brick.
  appAttrMap :: AppState -> B.AttrMap
  appAttrMap _ = B.attrMap V.defAttr []

drawAppWidget :: AppState -> [B.Widget AppBrickName]
drawAppWidget AppState{..} =
  let
    drawMenu = drawMenuWidget appStateNavigationMode
          (\case
            AppSelectorReplInput -> "REPL"
            AppSelectorReplOutput -> "Output"
            AppSelectorWalletTree -> "Tree"
            AppSelectorWalletPane -> "Pane"
            AppSelectorHelp -> "Help"
            AppSelectorLogs -> "Logs")
          appStateMenu
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
    padLR =
      B.padLeft (B.Pad 1) . B.padRight (B.Pad 1)
    drawDefaultView =
      B.vBox
        [ drawMenu
        , B.hBox
            [ padLR drawWalletTree
            , B.joinBorders B.vBorder
            , padLR drawWalletPane
            ]
        , B.joinBorders B.hBorder
        , drawRepl
        , drawStatus
        ]
    drawHelp =
      drawHelpWidget appStateHelp
    drawHelpView =
      B.vBox [drawMenu, drawHelp, drawStatus]
    drawLogs =
      drawLogsWidget appStateLogs
    drawLogsView =
      B.vBox [drawMenu, drawLogs, drawStatus]
  in
    case (menuWidgetSel appStateMenu) of
      AppSelectorHelp -> [drawHelpView]
      AppSelectorLogs -> [drawLogsView]
      _ -> [drawDefaultView]

handleAppEvent
  :: UiLangFace
  -> B.BrickEvent AppBrickName UiEvent
  -> StateT AppState (B.EventM AppBrickName) AppCompleted
handleAppEvent langFace ev =
  case ev of
    B.VtyEvent vtyEv -> do
      sel <- uses appStateMenuL menuWidgetSel
      navModeEnabled <- use appStateNavigationModeL
      let
        key = vtyToKey vtyEv
        editKey = vtyToEditKey vtyEv
      if
        | KeyQuit <- key ->
            return AppCompleted
        | KeyNavNext <- key,
          navModeEnabled -> do
            zoom appStateMenuL $ handleMenuWidgetEvent MenuNextEvent
            return AppInProgress
        | KeyNavPrev <- key,
          navModeEnabled -> do
            zoom appStateMenuL $ handleMenuWidgetEvent MenuPrevEvent
            return AppInProgress
        | KeyChar c <- key,
          navModeEnabled,
          Just appSel <- charAppSel c -> do
            appStateNavigationModeL .= False
            zoom appStateMenuL $ handleMenuWidgetEvent (MenuSelectEvent (==appSel))
            return AppInProgress
        | KeyNavigation <- key ->
          do
              appStateNavigationModeL .= True
              return AppInProgress
        | navModeEnabled -> return AppInProgress
        | Just replEv <- keyToReplInputEvent editKey,
          AppSelectorReplInput <- sel -> do
            completed <- zoom appStateReplL $
              handleReplInputEvent langFace replEv
            return $ case completed of
              ReplCompleted -> AppCompleted
              ReplInProgress -> AppInProgress
        | Just scrollAction <- keyToScrollingAction key,
          AppSelectorReplOutput <- sel -> do
            zoom appStateReplL $ handleReplOutputEvent $ ReplOutputScrollingEvent scrollAction
            return AppInProgress
        | Just scrollAction <- keyToScrollingAction key,
          AppSelectorHelp <- sel -> do
            zoom appStateHelpL $ handleHelpWidgetEvent $ HelpScrollingEvent scrollAction
            return AppInProgress
        | Just scrollAction <- keyToScrollingAction key,
          AppSelectorLogs <- sel -> do
            zoom appStateLogsL $ handleLogsWidgetEvent $ LogsScrollingEvent scrollAction
            return AppInProgress
        | Just walletTreeEv <- keyToWalletTreeEvent key,
          AppSelectorWalletTree <- sel -> do
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
  'r' -> Just AppSelectorReplInput
  'o' -> Just AppSelectorReplOutput
  't' -> Just AppSelectorWalletTree
  'p' -> Just AppSelectorWalletPane
  'h' -> Just AppSelectorHelp
  'l' -> Just AppSelectorLogs
  _ -> Nothing
