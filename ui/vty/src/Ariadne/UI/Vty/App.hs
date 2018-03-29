module Ariadne.UI.Vty.App
  ( AppState
  , initialAppState
  , app
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List.NonEmpty
import Data.Text as T
import Data.Void
import Formatting (sformat, stext, (%))
import IiExtras
import Prelude


import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.CommandHistory
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Scrolling

import Ariadne.UI.Vty.Widget.Repl
  (CommandAction(..), InputModification(..), NavAction(..), ReplCompleted(..),
  ReplInputEvent(..), ReplOutputEvent(..), ReplWidgetState(..),
  drawReplInputWidget, drawReplOutputWidget, handleReplInputEvent,
  handleReplOutputEvent, initReplWidget)

import Ariadne.UI.Vty.Widget.Menu
  (MenuWidgetEvent(..), MenuWidgetState, drawMenuWidget, handleMenuWidgetEvent,
  initMenuWidget, menuWidgetSel)

import Ariadne.UI.Vty.Widget.Help
  (HelpWidgetState, drawHelpWidget, initHelpWidget)

import Ariadne.UI.Vty.Widget.Logs
  (LogsWidgetEvent(..), LogsWidgetState, drawLogsWidget, handleLogsWidgetEvent,
  initLogsWidget)

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
    , appStateNavigationMode :: Bool
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
    , appStateNavigationMode = False
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
    tabTitle t navMode
      | T.null t = error "Bug: empty title"
      | navMode = sformat (stext%" - "%stext) ((T.singleton . T.head) t) t
      | otherwise = sformat stext t

    drawMenu = drawMenuWidget appStateNavigationMode
          (\case
            AppSelectorReplInput -> tabTitle "REPL" appStateNavigationMode
            AppSelectorReplOutput -> tabTitle "Output" appStateNavigationMode
            AppSelectorWalletTree -> tabTitle "Tree" appStateNavigationMode
            AppSelectorWalletPane -> tabTitle "Pane" appStateNavigationMode
            AppSelectorHelp -> tabTitle "Help" appStateNavigationMode
            AppSelectorLogs -> tabTitle "Logs"  appStateNavigationMode)
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
  AppState{..} <- get
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
      -- | True <- appStateNavigationMode,
      | (V.EvKey (V.KChar c) [], True) <- (vtyEv, appStateNavigationMode),
        Just appSel <- charAppSel c -> do
          put $ AppState { appStateNavigationMode = False, ..}
          zoom appStateMenuL $ handleMenuWidgetEvent (MenuSelectEvent (==appSel))
          return AppInProgress
      | V.EvKey (V.KChar 'g') [V.MCtrl] <- vtyEv ->
        do
            put $ AppState { appStateNavigationMode = True, ..}
            return AppInProgress
      | Just replEv <- toReplEv vtyEv,
        AppSelectorReplInput <- sel -> do
          completed <- zoom appStateReplL $
            handleReplInputEvent langFace replEv
          return $ case completed of
            ReplCompleted -> AppCompleted
            ReplInProgress -> AppInProgress
      | Just replEv <- toReplOutputEv vtyEv,
        AppSelectorReplOutput <- sel -> do
            zoom appStateReplL $ handleReplOutputEvent replEv
            return AppInProgress
      | Just logsEv <- toLogsEv vtyEv,
        AppSelectorLogs <- sel -> do
            zoom appStateLogsL $ handleLogsWidgetEvent logsEv
            return AppInProgress
    B.AppEvent (UiCommandEvent commandId commandEvent) -> do
        completed <- zoom appStateReplL $
          handleReplInputEvent langFace $
            ReplCommandEvent commandId commandEvent
        return $ case completed of
          ReplCompleted -> AppCompleted
          ReplInProgress -> AppInProgress
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

toReplOutputEv :: V.Event -> Maybe ReplOutputEvent
toReplOutputEv = \case
  V.EvKey V.KUp [] ->
    Just $ ReplOutputScrollingEvent ScrollingLineUp
  V.EvKey V.KDown [] ->
    Just $ ReplOutputScrollingEvent ScrollingLineDown
  V.EvKey V.KPageUp [] ->
    Just $ ReplOutputScrollingEvent ScrollingPgUp
  V.EvKey V.KPageDown [] ->
    Just $ ReplOutputScrollingEvent ScrollingPgDown
  _ -> Nothing

toLogsEv :: V.Event -> Maybe LogsWidgetEvent
toLogsEv = \case
  V.EvKey V.KUp [] ->
    Just $ LogsScrollingEvent ScrollingLineUp
  V.EvKey V.KDown [] ->
    Just $ LogsScrollingEvent ScrollingLineDown
  V.EvKey V.KPageUp [] ->
    Just $ LogsScrollingEvent ScrollingPgUp
  V.EvKey V.KPageDown [] ->
    Just $ LogsScrollingEvent ScrollingPgDown
  _ -> Nothing
