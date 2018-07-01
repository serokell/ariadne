module Ariadne.UI.Vty.Widget
       ( WidgetNamePart(..)
       , WidgetName
       , WidgetEvent(..)
       , WidgetEventResult(..)
       , Widget(..)
       , WidgetInitM
       , WidgetDrawM
       , WidgetEventM

       , initWidget
       , setWidgetState
       , addWidgetChild
       , setWidgetFocusList
       , addWidgetEventHandler
       , setWidgetDraw
       , setWidgetDrawWithFocused
       , setWidgetDrawWithFocus
       , setWidgetScrollable
       , setWidgetHandleEditKey
       , setWidgetHandleKey
       , setWidgetHandlePaste
       , setWidgetHandleMouseDown
       , setWidgetHandleScroll
       , setWidgetHandleEvent

       , getWidgetName
       , getFocusRing
       , findClosestFocus
       , liftBrick
       , widgetParentGetter
       , widgetParentLens
       , viewWidgetLens
       , useWidgetLens
       , assignWidgetLens
       , widgetEvent

       , drawWidget
       , drawWidgetChild

       , handleWidgetEditKey
       , handleWidgetKey
       , handleWidgetPaste
       , handleWidgetMouseDown
       , handleWidgetScroll
       , handleWidgetEvent

       -- Re-exports
       , ReifiedLens'
       , ReifiedLens(..)
       ) where

import Universum

import Control.Lens (ReifiedLens', ReifiedLens(..), assign, lens, makeLensesWith, (%=), (.=))
import IiExtras (postfixLFields)

import qualified Brick as B
import qualified Brick.Focus as B
import qualified Data.Map.Strict as Map

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling

----------------------------------------------------------------------------
-- Types and instances
----------------------------------------------------------------------------

data WidgetNamePart
  = WidgetNameSelf  -- Special name for adding widget itself to focus list

  | WidgetNameMenu
  | WidgetNameTree
  | WidgetNameStatus
  | WidgetNameHelp
  | WidgetNameAbout
  | WidgetNameLogs

  | WidgetNameRepl
  | WidgetNameReplInput

  | WidgetNameAddWallet
  | WidgetNameAddWalletNewName
  | WidgetNameAddWalletNewPass
  | WidgetNameAddWalletNewButton
  | WidgetNameAddWalletRestoreName
  | WidgetNameAddWalletRestoreMnemonic
  | WidgetNameAddWalletRestorePass
  | WidgetNameAddWalletRestoreFull
  | WidgetNameAddWalletRestoreButton

  | WidgetNameWallet
  | WidgetNameWalletSendAddress
  | WidgetNameWalletSendAmount
  | WidgetNameWalletSendPass
  | WidgetNameWalletSendButton

  | WidgetNameAccount
  deriving (Eq, Ord, Show)

data WidgetEvent
  = WidgetEventMenuSelected
  | WidgetEventButtonPressed
  | WidgetEventEditChanged

type WidgetName = [WidgetNamePart]

data WidgetEventResult
  = WidgetEventHandled
  | WidgetEventNotHandled

data WidgetInfo s p = WidgetInfo
  { widgetName :: !WidgetName
  , widgetState :: s
  , widgetChildren :: !(Map WidgetNamePart (Widget (WidgetInfo s p)))
  , widgetFocusList :: ![WidgetNamePart]
  , widgetEventHandlers :: !(Map WidgetNamePart (WidgetEvent -> WidgetEventM s p ()))
  , widgetEventQueue :: ![(WidgetNamePart, WidgetEvent)]
  , widgetEventSend :: WidgetEvent -> WidgetEventM s p ()
  , widgetDraw :: !(s -> WidgetDrawM s p (B.Widget WidgetName))
  , widgetDrawWithFocused :: !(Bool -> s -> WidgetDrawM s p (B.Widget WidgetName))
  , widgetDrawWithFocus :: !(WidgetName -> s -> WidgetDrawM s p (B.Widget WidgetName))
  , widgetHandleEditKey :: !(KeyboardEditEvent -> WidgetEventM s p WidgetEventResult)
  , widgetHandleKey :: !(KeyboardEvent -> WidgetEventM s p WidgetEventResult)
  , widgetHandlePaste :: !(Text -> WidgetEventM s p WidgetEventResult)
  , widgetHandleMouseDown :: !(B.Location -> WidgetEventM s p WidgetEventResult)
  , widgetHandleScroll :: !(ScrollingAction -> WidgetEventM s p WidgetEventResult)
  , widgetHandleEvent :: !(UiEvent -> WidgetEventM s p ())
  }

instance B.Named (WidgetInfo s p) WidgetName where
  getName = widgetName

data Widget p = forall s. Widget (WidgetInfo s p)

instance B.Named (Widget p) WidgetName where
  getName (Widget WidgetInfo{..}) = widgetName

type WidgetInitM s p = State (WidgetInfo s p) ()

type WidgetDrawM s p a = ReaderT (WidgetInfo s p) (Reader p) a

type WidgetEventM s p a = StateT s (StateT (WidgetInfo s p) (StateT p (B.EventM WidgetName))) a

makeLensesWith postfixLFields ''WidgetInfo

----------------------------------------------------------------------------
-- Widget initialization
----------------------------------------------------------------------------

initWidget :: WidgetInitM s p -> Widget p
initWidget action =
  Widget $ execState action WidgetInfo
    { widgetName = []
    , widgetState = error "State not defined for this widget"
    , widgetChildren = Map.empty
    , widgetFocusList = []
    , widgetEventHandlers = Map.empty
    , widgetEventQueue = []
    , widgetEventSend = return . const ()
    , widgetDraw = return . const B.emptyWidget
    , widgetDrawWithFocused = \_ s -> do
        draw <- view widgetDrawL
        draw s
    , widgetDrawWithFocus = \focus s -> do
        draw <- view widgetDrawWithFocusedL
        name <- view widgetNameL
        draw (name == focus) s
    , widgetHandleEditKey = return . const WidgetEventNotHandled
    , widgetHandleKey = return . const WidgetEventNotHandled
    , widgetHandlePaste = return . const WidgetEventNotHandled
    , widgetHandleMouseDown = return . const WidgetEventNotHandled
    , widgetHandleScroll = return . const WidgetEventNotHandled
    , widgetHandleEvent = return . const ()
    }

setWidgetState :: s -> WidgetInitM s p
setWidgetState = assign widgetStateL

addWidgetChild :: WidgetNamePart -> Widget (WidgetInfo s p) -> WidgetInitM s p
addWidgetChild namePart (Widget child) = do
    let child' = child{ widgetEventSend = \event -> lift . lift $ widgetEventQueueL %= ((namePart, event):) }
    widgetChildrenL %= Map.insert namePart (rename $ Widget child')
  where
    rename :: Widget p -> Widget p
    rename (Widget widget@WidgetInfo{..}) =
      Widget widget
        { widgetName = namePart : widgetName
        , widgetChildren = Map.map rename widgetChildren
        }

setWidgetFocusList :: MonadState (WidgetInfo s p) m => [WidgetNamePart] -> m ()
setWidgetFocusList = assign widgetFocusListL

addWidgetEventHandler :: MonadState (WidgetInfo s p) m => WidgetNamePart -> (WidgetEvent -> WidgetEventM s p ()) -> m ()
addWidgetEventHandler namePart handler = do
  widgetEventHandlersL %= Map.insert namePart handler

setWidgetDraw :: MonadState (WidgetInfo s p) m => (s -> WidgetDrawM s p (B.Widget WidgetName)) -> m ()
setWidgetDraw = assign widgetDrawL

setWidgetDrawWithFocused :: MonadState (WidgetInfo s p) m => (Bool -> s -> WidgetDrawM s p (B.Widget WidgetName)) -> m ()
setWidgetDrawWithFocused = assign widgetDrawWithFocusedL

setWidgetDrawWithFocus :: MonadState (WidgetInfo s p) m => (WidgetName -> s -> WidgetDrawM s p (B.Widget WidgetName)) -> m ()
setWidgetDrawWithFocus = assign widgetDrawWithFocusL

setWidgetScrollable :: MonadState (WidgetInfo s p) m => m ()
setWidgetScrollable = assign widgetHandleScrollL $ \action -> do
  name <- lift $ use widgetNameL
  liftBrick $ handleScrollingEvent name action
  return WidgetEventHandled

setWidgetHandleEditKey :: MonadState (WidgetInfo s p) m => (KeyboardEditEvent -> WidgetEventM s p WidgetEventResult) -> m ()
setWidgetHandleEditKey = assign widgetHandleEditKeyL

setWidgetHandleKey :: MonadState (WidgetInfo s p) m => (KeyboardEvent -> WidgetEventM s p WidgetEventResult) -> m ()
setWidgetHandleKey = assign widgetHandleKeyL

setWidgetHandlePaste :: MonadState (WidgetInfo s p) m => (Text -> WidgetEventM s p WidgetEventResult) -> m ()
setWidgetHandlePaste = assign widgetHandlePasteL

setWidgetHandleMouseDown :: MonadState (WidgetInfo s p) m => (B.Location -> WidgetEventM s p WidgetEventResult) -> m ()
setWidgetHandleMouseDown = assign widgetHandleMouseDownL

setWidgetHandleScroll :: MonadState (WidgetInfo s p) m => (ScrollingAction -> WidgetEventM s p WidgetEventResult) -> m ()
setWidgetHandleScroll = assign widgetHandleScrollL

setWidgetHandleEvent :: MonadState (WidgetInfo s p) m => (UiEvent -> WidgetEventM s p ()) -> m ()
setWidgetHandleEvent = assign widgetHandleEventL

----------------------------------------------------------------------------
-- Widget utilities
----------------------------------------------------------------------------

getWidgetName :: WidgetDrawM s p WidgetName
getWidgetName = view widgetNameL

getFocusRing :: Widget p -> B.FocusRing WidgetName
getFocusRing = B.focusRing . collectFocuses
  where
    collectFocuses :: Widget p -> [WidgetName]
    collectFocuses (Widget WidgetInfo{..}) =
        if null widgetFocusList
        then [widgetName]
        else concat $ catMaybes $ childFocuses <$> widgetFocusList
      where
        childFocuses WidgetNameSelf = Just [widgetName]
        childFocuses namePart = collectFocuses <$> Map.lookup namePart widgetChildren

findClosestFocus :: WidgetName -> Widget p -> WidgetName
findClosestFocus [] (Widget WidgetInfo{..})
    | n:_ <- catMaybes $ findInChild <$> widgetFocusList = n
    | otherwise = widgetName
  where
    findInChild WidgetNameSelf = Just widgetName
    findInChild namePart = findClosestFocus [] <$> Map.lookup namePart widgetChildren
findClosestFocus (np:nps) widget@(Widget WidgetInfo{..})
  | Just child <- Map.lookup np widgetChildren = findClosestFocus nps child
  | otherwise = findClosestFocus [] widget

liftBrick :: B.EventM WidgetName a -> WidgetEventM s p a
liftBrick = lift . lift . lift

widgetParentGetter :: (p -> a) -> (WidgetInfo p q -> a)
widgetParentGetter f = \WidgetInfo{ widgetState } -> f widgetState

widgetParentLens :: Lens' p a -> Lens' (WidgetInfo p q) a
widgetParentLens l = lens getter setter
  where
    getter WidgetInfo{ widgetState } = widgetState ^. l
    setter widget@WidgetInfo{ widgetState } v = widget{ widgetState = widgetState & l .~ v }

viewWidgetLens :: ReifiedLens' p a -> WidgetDrawM s p a
viewWidgetLens = lift . view . runLens

useWidgetLens :: ReifiedLens' p a -> WidgetEventM s p a
useWidgetLens = lift . lift . use . runLens

assignWidgetLens :: ReifiedLens' p a -> a -> WidgetEventM s p ()
assignWidgetLens (Lens l) = lift . lift . assign l

widgetEvent :: WidgetEvent -> WidgetEventM s p ()
widgetEvent event = do
  WidgetInfo{..} <- lift get
  widgetEventSend event

----------------------------------------------------------------------------
-- Widget rendering
----------------------------------------------------------------------------

drawWidget :: WidgetName -> p -> Widget p -> B.Widget WidgetName
drawWidget focus parent (Widget widget@WidgetInfo{ widgetState, widgetDrawWithFocus }) =
  runReader (runReaderT (widgetDrawWithFocus focus widgetState) widget) parent

drawWidgetChild :: WidgetName -> WidgetInfo s p -> WidgetNamePart -> B.Widget WidgetName
drawWidgetChild focus widget@WidgetInfo{ widgetChildren } name =
  maybe B.emptyWidget (drawWidget focus widget) $ Map.lookup name widgetChildren

----------------------------------------------------------------------------
-- Widget event handling
----------------------------------------------------------------------------

handleWidgetEditKey :: KeyboardEditEvent -> WidgetName -> StateT (Widget p) (StateT p (B.EventM WidgetName)) WidgetEventResult
handleWidgetEditKey editKey name = do
  Widget widget@WidgetInfo{..} <- get
  handleByName widget name
    (widgetHandleEditKey editKey)
    (handleWidgetEditKey editKey)

handleWidgetKey :: KeyboardEvent -> WidgetName -> StateT (Widget p) (StateT p (B.EventM WidgetName)) WidgetEventResult
handleWidgetKey key name = do
  Widget widget@WidgetInfo{..} <- get
  handleByName widget name
    (widgetHandleKey key)
    (handleWidgetKey key)

handleWidgetPaste :: Text -> WidgetName -> StateT (Widget p) (StateT p (B.EventM WidgetName)) WidgetEventResult
handleWidgetPaste pasted name = do
  Widget widget@WidgetInfo{..} <- get
  handleByName widget name
    (widgetHandlePaste pasted)
    (handleWidgetPaste pasted)

handleWidgetMouseDown :: B.Location -> WidgetName -> StateT (Widget p) (StateT p (B.EventM WidgetName)) WidgetEventResult
handleWidgetMouseDown coords name = do
  Widget widget@WidgetInfo{..} <- get
  handleByName widget name
    (widgetHandleMouseDown coords)
    (handleWidgetMouseDown coords)

handleWidgetScroll :: ScrollingAction -> WidgetName -> StateT (Widget p) (StateT p (B.EventM WidgetName)) WidgetEventResult
handleWidgetScroll action name = do
  Widget widget@WidgetInfo{..} <- get
  handleByName widget name
    (widgetHandleScroll action)
    (handleWidgetScroll action)

handleWidgetEvent :: UiEvent -> StateT (Widget p) (StateT p (B.EventM WidgetName)) ()
handleWidgetEvent event = do
  Widget widget@WidgetInfo{..} <- get
  handleAll widget
    (widgetHandleEvent event)
    (handleWidgetEvent event)

----------------------------------------------------------------------------
-- Event handling helpers
----------------------------------------------------------------------------

handleAll
  :: WidgetInfo s p
  -> WidgetEventM s p ()
  -> StateT (Widget (WidgetInfo s p)) (StateT (WidgetInfo s p) (B.EventM WidgetName)) ()
  -> StateT (Widget p) (StateT p (B.EventM WidgetName)) ()
handleAll widget@WidgetInfo{..} widgetHandler childHandler = do
  (widgetChildren', widget') <- lift . lift $ runStateT (mapM (execStateT childHandler) widgetChildren) widget
  withWidget widget'{ widgetChildren = widgetChildren' } widgetHandler

handleByName
  :: WidgetInfo s p
  -> WidgetName
  -> WidgetEventM s p WidgetEventResult
  -> (WidgetName -> StateT (Widget (WidgetInfo s p)) (StateT (WidgetInfo s p) (B.EventM WidgetName)) WidgetEventResult)
  -> StateT (Widget p) (StateT p (B.EventM WidgetName)) WidgetEventResult
handleByName widget@WidgetInfo{..} name widgetHandler childHandler =
  case name of
    [] -> withWidget widget widgetHandler
    x:xs -> case Map.lookup x widgetChildren of
      Nothing -> withWidget widget widgetHandler
      Just child -> do
        ((res, child'), widget') <- lift . lift $ runStateT (runStateT (childHandler xs) child) widget
        withWidget widget'{ widgetChildren = Map.insert x child' widgetChildren } $
          case res of
            WidgetEventNotHandled -> widgetHandler
            WidgetEventHandled -> return res

withWidget
  :: WidgetInfo s p
  -> WidgetEventM s p a
  -> StateT (Widget p) (StateT p (B.EventM WidgetName)) a
withWidget widget@WidgetInfo{..} action = do
  let
    subaction = do
      (res, widgetState') <- runStateT action widgetState
      widgetStateL .= widgetState'
      forM_ widgetEventQueue $ \(namePart, event) -> do
        whenJust (Map.lookup namePart widgetEventHandlers) $ \handler -> do
          use widgetStateL >>= execStateT (handler event) >>= assign widgetStateL
      widgetEventQueueL .= []
      return res
  (res, widget') <- lift $ runStateT subaction widget
  put $ Widget widget'
  return res
