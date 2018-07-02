module Ariadne.UI.Vty.App
  ( initApp
  , app
  ) where

import Universum

import Control.Lens (makeLensesWith, uses, (.=), (%=))
import IiExtras
import Named (Named(..))

import qualified Brick as B
import qualified Brick.Focus as B
import qualified Brick.Widgets.Border as B
import qualified Data.List.NonEmpty as NE
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Focus
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Theme
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.About
import Ariadne.UI.Vty.Widget.Account
import Ariadne.UI.Vty.Widget.AddWallet
import Ariadne.UI.Vty.Widget.Help
import Ariadne.UI.Vty.Widget.Logs
import Ariadne.UI.Vty.Widget.Menu
import Ariadne.UI.Vty.Widget.Repl
import Ariadne.UI.Vty.Widget.Status
import Ariadne.UI.Vty.Widget.Tree
import Ariadne.UI.Vty.Widget.Wallet

data AppScreen
  = AppScreenWallet
  | AppScreenHelp
  | AppScreenAbout
  | AppScreenLogs
  deriving (Eq)

data AppSelection
  = AppSelectionNone
  | AppSelectionWallet
  | AppSelectionAccount
  | AppSelectionAddWallet

data AppCompleted = AppCompleted | AppInProgress

data AppWidgetState =
  AppWidgetState
    { appScreen :: !AppScreen
    , appSelection :: !AppSelection
    }

data AppState =
  AppState
    { appWidget :: Widget AppState
    , appFocusRing :: B.FocusRing WidgetName
    , appNavMode :: Bool
    }

makeLensesWith postfixLFields ''AppWidgetState
makeLensesWith postfixLFields ''AppState

initApp :: Text `Named` "ariadne_url" -> UiFace -> UiLangFace -> UiHistoryFace -> AppState
initApp ariadneURL uiFace langFace historyFace =
  AppState
    { appWidget = appWidget
    , appFocusRing = getFocusRing appWidget
    , appNavMode = False
    }
  where
    appWidget = initWidget $ do
      setWidgetState appWidgetState
      setWidgetFocusList $ appFocusList appWidgetState
      setWidgetDrawWithFocus drawAppWidget
      setWidgetHandleKey handleAppWidgetKey
      setWidgetHandleEvent handleAppWidgetEvent

      addWidgetChild WidgetNameMenu $ initMenuWidget menuItems (widgetParentLens appScreenL)
      addWidgetChild WidgetNameStatus $ initStatusWidget ariadneURL
      addWidgetChild WidgetNameTree $ initTreeWidget langFace
      addWidgetChild WidgetNameAddWallet $ initAddWalletWidget langFace
      addWidgetChild WidgetNameWallet $ initWalletWidget langFace
      addWidgetChild WidgetNameAccount $ initAccountWidget langFace
      addWidgetChild WidgetNameRepl $ initReplWidget uiFace langFace historyFace
      addWidgetChild WidgetNameHelp $ initHelpWidget langFace
      addWidgetChild WidgetNameAbout initAboutWidget
      addWidgetChild WidgetNameLogs initLogsWidget

      addWidgetEventHandler WidgetNameMenu $ \case
        WidgetEventMenuSelected -> do
          resetAppFocus
          assignWidgetLens (Lens appNavModeL) False
        _ -> return ()

    appWidgetState = AppWidgetState
      { appScreen = AppScreenWallet
      , appSelection = AppSelectionNone
      }

    menuItems = NE.fromList
      [ MenuWidgetElem AppScreenWallet "Wallet" 'w'
      , MenuWidgetElem AppScreenHelp "Help" 'h'
      , MenuWidgetElem AppScreenAbout "About" 'a'
      , MenuWidgetElem AppScreenLogs "Logs" 'l'
      ]

appFocusList :: AppWidgetState -> [WidgetNamePart]
appFocusList AppWidgetState{..} = case appScreen of
    AppScreenWallet -> [WidgetNameTree] ++ mainWidgetName ++ [WidgetNameRepl]
    AppScreenHelp -> [WidgetNameHelp]
    AppScreenAbout -> [WidgetNameAbout]
    AppScreenLogs -> [WidgetNameLogs]
  where
    mainWidgetName = case appSelection of
      AppSelectionNone -> []
      AppSelectionAddWallet -> [WidgetNameAddWallet]
      AppSelectionWallet -> [WidgetNameWallet]
      AppSelectionAccount -> [WidgetNameAccount]

getAppFocus :: AppState -> WidgetName
getAppFocus AppState{..} =
  if appNavMode
    then [WidgetNameMenu]
    else fromMaybe [] $ B.focusGetCurrent appFocusRing

resetAppFocus :: WidgetEventM AppWidgetState AppState ()
resetAppFocus = do
  get >>= lift . setWidgetFocusList . appFocusList
  lift $ do
    widget <- get
    lift $ do
      mcurrent <- uses appFocusRingL B.focusGetCurrent
      appFocusRingL .= getFocusRing (Widget widget)
      whenJust mcurrent setAppFocus

setAppFocus :: Monad m => WidgetName -> StateT AppState m ()
setAppFocus focus = do
  focus' <- uses appWidgetL $ findClosestFocus focus
  appFocusRingL %= B.focusSetCurrent focus'

-- The Ariadne UI view and controller a single record.
app :: B.App AppState UiEvent WidgetName
app = B.App{..} where

  appDraw :: AppState -> [B.Widget WidgetName]
  appDraw = drawApp

  appChooseCursor
    :: AppState
    -> [B.CursorLocation WidgetName]
    -> Maybe (B.CursorLocation WidgetName)
  appChooseCursor = B.focusRingCursor appFocusRing

  appHandleEvent
    :: AppState
    -> B.BrickEvent WidgetName UiEvent
    -> B.EventM WidgetName (B.Next AppState)
  appHandleEvent appState ev = do
    (completed, appState') <-
      runStateT (handleAppEvent ev) appState
    case completed of
      AppCompleted -> B.halt appState'
      AppInProgress -> B.continue appState'

  -- We do not use this feature of Brick.
  appStartEvent :: AppState -> B.EventM WidgetName AppState
  appStartEvent = return

  appAttrMap :: AppState -> B.AttrMap
  appAttrMap = const defaultAttrMap

drawApp :: AppState -> [B.Widget WidgetName]
drawApp appState@AppState{..} =
    [ drawWidget (getAppFocus appState) appState appWidget
    -- Widgets don't always fill the screen, so we need a background widget
    -- in case default terminal background differs from our theme background
    , B.withAttr "default" $ B.fill ' '
    ]

drawAppWidget :: WidgetName -> AppWidgetState -> WidgetDrawM AppWidgetState p (B.Widget WidgetName)
drawAppWidget focus AppWidgetState{..} = do
  widget <- ask
  let
    drawChild = drawWidgetChild focus widget
    drawScreen widgets =
      B.withAttr "default" $ B.vBox $
        [drawChild WidgetNameMenu]
        ++ widgets
        ++ [drawChild WidgetNameStatus]
    drawWalletScreen = drawScreen
        [ B.hBox
            [ withFocusIndicator focus [WidgetNameTree] 'T' 1 $ drawChild WidgetNameTree
            , B.joinBorders B.vBorder
            , mainWidget
            ]
        , B.joinBorders B.hBorder
        , drawChild WidgetNameRepl
        ]
      where
        mainWidget = case appSelection of
          AppSelectionAddWallet -> withFocusIndicator focus [WidgetNameAddWallet] 'P' 1 $ drawChild WidgetNameAddWallet
          AppSelectionWallet -> withFocusIndicator focus [WidgetNameWallet] 'P' 1 $ drawChild WidgetNameWallet
          AppSelectionAccount -> withFocusIndicator focus [WidgetNameAccount] 'P' 1 $ drawChild WidgetNameAccount
          _ -> B.emptyWidget
    drawHelpScreen = drawScreen [drawChild WidgetNameHelp]
    drawAboutScreen = drawScreen [drawChild WidgetNameAbout]
    drawLogsScreen = drawScreen [drawChild WidgetNameLogs]

  return $ case appScreen of
    AppScreenWallet -> drawWalletScreen
    AppScreenHelp -> drawHelpScreen
    AppScreenAbout -> drawAboutScreen
    AppScreenLogs -> drawLogsScreen

handleAppEvent
  :: B.BrickEvent WidgetName UiEvent
  -> StateT AppState (B.EventM WidgetName) AppCompleted
handleAppEvent brickEvent = do
  case brickEvent of
    B.VtyEvent vtyEv@V.EvKey{} -> do
      let
        key = vtyToKey vtyEv
        editKey = vtyToEditKey vtyEv
      focus <- gets getAppFocus
      runHandler (handleWidgetEditKey editKey focus) >>= \case
        WidgetEventHandled -> return AppInProgress
        WidgetEventNotHandled ->
          runHandler (handleWidgetKey key focus) >>= \case
            WidgetEventHandled -> return AppInProgress
            WidgetEventNotHandled
              | KeyQuit <- key ->
                  return AppCompleted
              | KeyNavigation <- key -> do
                  appNavModeL %= not
                  return AppInProgress
              | KeyFocusPrev <- key -> do
                  appFocusRingL %= B.focusPrev
                  appNavModeL .= False
                  return AppInProgress
              | KeyFocusNext <- key -> do
                  appFocusRingL %= B.focusNext
                  appNavModeL .= False
                  return AppInProgress
              | Just scrollAction <- keyToScrollingAction key -> do
                  void $ runHandler $ handleWidgetScroll scrollAction focus
                  return AppInProgress
              | otherwise ->
                  return AppInProgress
    B.VtyEvent (V.EvPaste raw) -> do
      whenRight (decodeUtf8' raw) $ \pasted -> do
        focus <- gets getAppFocus
        void $ runHandler $ handleWidgetPaste pasted focus
      return AppInProgress
    B.MouseDown name button [] coords -> do
      case button of
        V.BScrollUp -> void $ runHandler $ handleWidgetScroll ScrollingLineUp name
        V.BScrollDown -> void $ runHandler $ handleWidgetScroll ScrollingLineDown name
        _ -> do
          setAppFocus name
          void $ runHandler $ handleWidgetMouseDown coords name
      return AppInProgress
    B.AppEvent (UiCommandAction UiCommandQuit) -> do
      return AppCompleted
    B.AppEvent event -> do
      runHandler $ handleWidgetEvent event
      return AppInProgress
    _ ->
      return AppInProgress
  where
    runHandler handler = do
      widget <- use appWidgetL
      (res, widget') <- runStateT handler widget
      appWidgetL .= widget'
      return res

handleAppWidgetKey
  :: KeyboardEvent
  -> WidgetEventM AppWidgetState AppState WidgetEventResult
handleAppWidgetKey key = do
    navMode <- useWidgetLens $ Lens appNavModeL
    selection <- use appSelectionL
    case key of
      KeyChar c
        | navMode -> do
            whenJust (charToFocus selection c) $ \(screen, focus) -> do
              appScreenL .= screen
              resetAppFocus
              lift . lift $ setAppFocus focus
            assignWidgetLens (Lens appNavModeL) False
            return WidgetEventHandled
      _ ->
        return WidgetEventNotHandled
  where
    charToFocus selection = \case
      't' -> Just (AppScreenWallet, [WidgetNameTree])
      'p' -> Just (AppScreenWallet, mainFocus selection)
      'r' -> Just (AppScreenWallet, [WidgetNameRepl])
      _   -> Nothing
    mainFocus = \case
      AppSelectionNone -> [WidgetNameTree]
      AppSelectionAddWallet -> [WidgetNameAddWallet]
      AppSelectionWallet -> [WidgetNameWallet]
      AppSelectionAccount -> [WidgetNameAccount]

handleAppWidgetEvent
  :: UiEvent
  -> WidgetEventM AppWidgetState AppState ()
handleAppWidgetEvent = \case
  UiWalletEvent UiWalletUpdate{..} -> do
    appSelectionL .= AppSelectionAddWallet
    whenJust wuPaneInfoUpdate $ \UiWalletInfo{..} -> case wpiType of
      Just UiWalletInfoWallet -> appSelectionL .= AppSelectionWallet
      Just UiWalletInfoAccount{} -> appSelectionL .= AppSelectionAccount
      _ -> return ()
    resetAppFocus
  UiCommandAction UiCommandHelp -> do
    appScreenL .= AppScreenHelp
    resetAppFocus
  UiCommandAction UiCommandLogs -> do
    appScreenL .= AppScreenLogs
    resetAppFocus
  _ ->
    return ()
