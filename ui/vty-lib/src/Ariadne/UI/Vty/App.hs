module Ariadne.UI.Vty.App
       ( initApp
       , app
       ) where

import Control.Lens (makeLensesWith, uses, (%=), (.=))
import Data.Char (toLower)

import qualified Brick as B
import qualified Brick.Focus as B
import qualified Brick.Widgets.Border as B
import qualified Data.List.NonEmpty as NE
import qualified Graphics.Vty as V

import Ariadne.Logging (Logging, logDebug)
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Focus
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Theme
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.About
import Ariadne.UI.Vty.Widget.Account
import Ariadne.UI.Vty.Widget.AddWallet
import Ariadne.UI.Vty.Widget.Dialog.ConfirmMnemonic
import Ariadne.UI.Vty.Widget.Dialog.ConfirmRemove
import Ariadne.UI.Vty.Widget.Dialog.ConfirmSend
import Ariadne.UI.Vty.Widget.Dialog.Password
import Ariadne.UI.Vty.Widget.Help
import Ariadne.UI.Vty.Widget.Logs
import Ariadne.UI.Vty.Widget.Menu
import Ariadne.UI.Vty.Widget.Repl
import Ariadne.UI.Vty.Widget.Status
import Ariadne.UI.Vty.Widget.Tree
import Ariadne.UI.Vty.Widget.Wallet
import Ariadne.Util
import Ariadne.UX.PasswordManager

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

data AppModal
  = NoModal
  | PasswordMode
  | ConfirmationMode UiConfirmationType

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
    , appModal :: AppModal
    }

makeLensesWith postfixLFields ''AppWidgetState
makeLensesWith postfixLFields ''AppState

initApp
    :: UiFeatures
    -> Logging
    -> PutPassword
    -> UiFace
    -> UiLangFace
    -> UiHistoryFace
    -> AppState
initApp features logging putPass uiFace langFace historyFace =
  AppState
    { appWidget = appWidget
    , appFocusRing = getFocusRing appWidget
    , appNavMode = False
    , appModal = NoModal
    }
  where
    appWidget = initWidget $ do
      setWidgetState appWidgetState
      setWidgetFocusList $ appFocusList appWidgetState
      setWidgetDrawWithFocus drawAppWidget
      setWidgetHandleKey handleAppWidgetKey
      setWidgetHandleEvent (handleAppWidgetEvent logging)

      addWidgetChild WidgetNameMenu $ initMenuWidget menuItems (widgetParentLens appScreenL)
      when (featureStatus features) $
        addWidgetChild WidgetNameStatus $ initStatusWidget
      addWidgetChild WidgetNameTree $ initTreeWidget langFace
      addWidgetChild WidgetNameAddWallet $ initAddWalletWidget langFace features
      addWidgetChild WidgetNameWallet $ initWalletWidget langFace features
      addWidgetChild WidgetNameAccount $ initAccountWidget langFace
      addWidgetChild WidgetNameRepl $ initReplWidget langFace historyFace
        (widgetParentGetter $ (== AppScreenWallet) . appScreen)
      addWidgetChild WidgetNameHelp $ initHelpWidget langFace
      addWidgetChild WidgetNameAbout initAboutWidget
      addWidgetChild WidgetNameLogs initLogsWidget
      addWidgetChild WidgetNamePassword $ initPasswordWidget putPass uiFace
      addWidgetChild WidgetNameConfirmMnemonic $ initConfirmMnemonicWidget uiFace
      addWidgetChild WidgetNameConfirmRemove $ initConfirmRemoveWidget uiFace
      addWidgetChild WidgetNameConfirmSend $ initConfirmSendWidget uiFace

      addWidgetEventHandler WidgetNameMenu $ \case
        WidgetEventMenuSelected -> do
          resetAppFocus
          assignWidgetLens (Lens appNavModeL) False
        _ -> pass

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
        AppScreenHelp -> [WidgetNameHelp, WidgetNameRepl]
        AppScreenAbout -> [WidgetNameAbout, WidgetNameRepl]
        AppScreenLogs -> [WidgetNameLogs, WidgetNameRepl]
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
  modal <- useWidgetLens (Lens appModalL)
  setWidgetFocusList =<< case modal of
    NoModal -> appFocusList <$> getWidgetState
    PasswordMode -> return [WidgetNamePassword]
    ConfirmationMode confirmationType -> case confirmationType of
      UiConfirmMnemonic _   -> return [WidgetNameConfirmMnemonic]
      UiConfirmRemove _     -> return [WidgetNameConfirmRemove]
      UiConfirmSend _       -> return [WidgetNameConfirmSend]

setAppFocus :: Monad m => WidgetName -> StateT AppState m ()
setAppFocus focus = do
  current <- fromMaybe [] <$> uses appFocusRingL B.focusGetCurrent
  focus' <- uses appWidgetL $ findClosestFocus current focus
  appFocusRingL %= B.focusSetCurrent focus'

updateAppFocusRing :: StateT AppState (B.EventM WidgetName) ()
updateAppFocusRing = do
  widget <- use appWidgetL
  mcurrent <- uses appFocusRingL B.focusGetCurrent
  appFocusRingL .= getFocusRing widget
  whenJust mcurrent setAppFocus

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
      runStateT (handleAppEvent ev <* updateAppFocusRing) appState
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
    toList (drawWidget (getAppFocus appState) appState appWidget)
    -- Widgets don't always fill the screen, so we need a background widget
    -- in case default terminal background differs from our theme background
    ++ [ B.withAttr "default" $ B.fill ' ' ]

drawAppWidget :: WidgetName -> AppWidgetState -> WidgetDrawM AppWidgetState AppState WidgetDrawing
drawAppWidget focus AppWidgetState{..} = do
  widget <- ask
  let
    drawChild = last . drawWidgetChild focus widget
    drawChildWithFocus name char pad =
      withFocusIndicator focus [name] char pad $ drawChild name
    drawContentChild name char =
      withFocusIndicator focus [name] char 0 $ B.padLeft (B.Pad 1) $ B.withAttr "default" $ drawChild name

    drawScreen widgets =
      B.withAttr "default" $ B.vBox $
        [ drawChild WidgetNameMenu
        ] ++
        widgets ++
        [ B.joinBorders B.hBorder
        , drawChild WidgetNameRepl
        , drawChild WidgetNameStatus
        ]
    drawWalletScreen = drawScreen
        [ B.hBox
            [ drawChildWithFocus WidgetNameTree 'T' 1
            , B.joinBorders B.vBorder
            , mainWidget
            ]
        ]
      where
        mainWidget = case appSelection of
          AppSelectionAddWallet -> drawChildWithFocus WidgetNameAddWallet 'P' 1
          AppSelectionWallet -> drawChildWithFocus WidgetNameWallet 'P' 1
          AppSelectionAccount -> drawChildWithFocus WidgetNameAccount 'P' 1
          _ -> B.emptyWidget
    drawHelpScreen = drawScreen [drawContentChild WidgetNameHelp 'H']
    drawAboutScreen = drawScreen [drawContentChild WidgetNameAbout 'A']
    drawLogsScreen = drawScreen [drawContentChild WidgetNameLogs 'L']

  let screenDraw = case appScreen of
        AppScreenWallet -> drawWalletScreen
        AppScreenHelp -> drawHelpScreen
        AppScreenAbout -> drawAboutScreen
        AppScreenLogs -> drawLogsScreen

  modal <- viewWidgetLens (Lens appModalL)
  return $ case modal of
    NoModal -> singleDrawing screenDraw
    PasswordMode -> layeredDrawing (drawChild WidgetNamePassword) [screenDraw]
    ConfirmationMode confirmationType -> (`layeredDrawing` [screenDraw]) $
      case confirmationType of
        UiConfirmMnemonic _ -> (drawChild WidgetNameConfirmMnemonic)
        UiConfirmRemove _   -> (drawChild WidgetNameConfirmRemove)
        UiConfirmSend _     -> (drawChild WidgetNameConfirmSend)

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
            WidgetEventHalt -> return AppCompleted
        WidgetEventHalt -> return AppCompleted
    B.VtyEvent (V.EvPaste raw) -> do
      whenRight (decodeUtf8' raw) $ \pasted -> do
        focus <- gets getAppFocus
        void $ runHandler $ handleWidgetPaste pasted focus
      return AppInProgress
    B.MouseDown name button [] coords -> do
      modal <- use appModalL
      let modalName = case modal of
            NoModal -> []
            PasswordMode -> [WidgetNamePassword]
            ConfirmationMode confirmationType -> case confirmationType of
              UiConfirmMnemonic _ -> [WidgetNameConfirmMnemonic]
              UiConfirmRemove _   -> [WidgetNameConfirmRemove]
              UiConfirmSend _     -> [WidgetNameConfirmSend]

      when (modalName `isPrefixOf` name) $ case button of
        V.BScrollUp -> void $ runHandler $ handleWidgetScroll ScrollingLineUp name
        V.BScrollDown -> void $ runHandler $ handleWidgetScroll ScrollingLineDown name
        _ -> do
          setAppFocus name
          void $ runHandler $ handleWidgetMouseDown coords name
      return AppInProgress
    B.AppEvent event -> do
      runHandler $ handleWidgetEvent event
      return AppInProgress
    _ ->
      return AppInProgress
  where
    runHandler
      :: StateT (Widget AppState) (StateT AppState (B.EventM WidgetName)) a
      -> StateT AppState (B.EventM WidgetName) a
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
    selection <- use (widgetStateL . appSelectionL)
    case key of
      KeyChar (toLower -> c)
        | navMode -> do
            whenJust (charToScreen c) $ \screen -> do
              widgetStateL . appScreenL .= screen
              resetAppFocus
            whenJust (charToFocus selection c) $ \focus -> do
              lift . setAppFocus $ [focus]
            assignWidgetLens (Lens appNavModeL) False
            return WidgetEventHandled
      _ ->
        return WidgetEventNotHandled
  where
    charToScreen = \case
      'w' -> Just AppScreenWallet
      't' -> Just AppScreenWallet
      'p' -> Just AppScreenWallet
      'h' -> Just AppScreenHelp
      'a' -> Just AppScreenAbout
      'l' -> Just AppScreenLogs
      _ -> Nothing
    charToFocus selection = \case
      't' -> Just WidgetNameTree
      'p' -> Just $ mainFocus selection
      'h' -> Just WidgetNameHelp
      'a' -> Just WidgetNameAbout
      'l' -> Just WidgetNameLogs
      'r' -> Just WidgetNameRepl
      _ -> Nothing
    mainFocus = \case
      AppSelectionNone -> WidgetNameTree
      AppSelectionAddWallet -> WidgetNameAddWallet
      AppSelectionWallet -> WidgetNameWallet
      AppSelectionAccount -> WidgetNameAccount

handleAppWidgetEvent
  :: HasCallStack
  => Logging
  -> UiEvent
  -> WidgetEventM AppWidgetState AppState ()
handleAppWidgetEvent logging = \case
  UiWalletEvent UiWalletUpdate{..} -> do
    -- Do not log, because there are too many of them during block sync
    zoomWidgetState $ do
      appSelectionL .= AppSelectionAddWallet
      whenJust wuSelectionInfo $ \case
        UiSelectionWallet{} -> appSelectionL .= AppSelectionWallet
        UiSelectionAccount{} -> appSelectionL .= AppSelectionAccount
    resetAppFocus
  UiCommandAction UiCommandHelp -> do
    logDebug logging "App widget received 'Help' event"
    widgetStateL . appScreenL .= AppScreenHelp
    resetAppFocus
  UiCommandAction UiCommandLogs -> do
    logDebug logging "Received 'Logs' event"
    widgetStateL . appScreenL .= AppScreenLogs
    resetAppFocus
  UiPasswordEvent passEvent -> do
    logDebug logging "App widget received a password event"
    assignWidgetLens (Lens appModalL) $ case passEvent of
      UiPasswordRequest _ _ -> PasswordMode
      UiPasswordSent ->  NoModal
    resetAppFocus
  UiConfirmEvent confirmEvent -> do
    logDebug logging "App widget received a confirm event"
    assignWidgetLens (Lens appModalL) $ case confirmEvent of
      UiConfirmRequest _ confirmationType -> ConfirmationMode confirmationType
      UiConfirmDone -> NoModal
    resetAppFocus
  _ ->
    pass
