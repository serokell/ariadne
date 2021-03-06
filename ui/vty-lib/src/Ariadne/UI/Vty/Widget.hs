module Ariadne.UI.Vty.Widget
       ( WidgetNamePart(..)
       , WidgetName
       , WidgetDrawing
       , WidgetEvent(..)
       , WidgetEventResult(..)
       , WidgetInfo
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
       , getIgnoreVisibility
       , getFocusRing
       , findClosestFocus
       , liftBrick
       , widgetParentGetter
       , widgetParentLens
       , widgetStateL
       , zoomWidgetState
       , getWidgetState
       , viewWidgetLens
       , useWidgetLens
       , assignWidgetLens
       , widgetEvent
       , updateEditable

       , drawWidget
       , drawWidgetChild
       , singleDrawing
       , layeredDrawing

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

import Control.Lens
  (ReifiedLens(..), ReifiedLens', Zoom, Zoomed, assign, makeLensesWith, to,
  zoom, (%=), (.=))

import qualified Brick as B
import qualified Brick.Focus as B
import qualified Data.Map.Strict as Map

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling
import Ariadne.Util

----------------------------------------------------------------------------
-- Types and instances
----------------------------------------------------------------------------

-- | Component of a unique widget name
--
-- Each widget has its child widgets indexed by this name component.
-- Widget's unique name is constructed as a list of these components,
-- corresponding to its location in widget tree.
--
-- Should be made into a union of widget-local name types.
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
  | WidgetNameAddWalletRestoreButton

  | WidgetNameSendAdd
  | WidgetNameSendAddress Int
  | WidgetNameSendAmount Int
  | WidgetNameSendRemove Int
  | WidgetNameSendPass
  | WidgetNameSendButton

  | WidgetNameWallet
  | WidgetNameWalletName
  | WidgetNameWalletRenameButton
  | WidgetNameWalletRemoveButton
  | WidgetNameWalletExportButton
  | WidgetNameWalletAccountList
  | WidgetNameWalletNewAccountName
  | WidgetNameWalletNewAccountButton
  | WidgetNameWalletSend
  | WidgetNameWalletChangePasswordButton

  | WidgetNameAccount
  | WidgetNameAccountName
  | WidgetNameAccountRenameButton
  | WidgetNameAccountRemoveButton
  | WidgetNameAccountSend
  | WidgetNameAccountAddressGenerateButton
  | WidgetNameAccountAddressList

  | WidgetNamePassword
  | WidgetNamePasswordInput
  | WidgetNamePasswordContinue

  | WidgetNameChangePassword
  | WidgetNameChangePasswordNewPassword
  | WidgetNameChangePasswordConfirmPassword
  | WidgetNameChangePasswordContinue

  | WidgetNameConfirmMnemonic
  | WidgetNameConfirmMnemonicInput
  | WidgetNameConfirmMnemonicCheckScreen
  | WidgetNameConfirmMnemonicCheckOnDevice
  | WidgetNameConfirmMnemonicCheckMoved
  | WidgetNameConfirmMnemonicContinue
  | WidgetNameConfirmMnemonicCancel

  | WidgetNameConfirmRemove
  | WidgetNameConfirmRemoveCheck
  | WidgetNameConfirmRemoveKeysExpand
  | WidgetNameConfirmRemoveName
  | WidgetNameConfirmRemoveContinue
  | WidgetNameConfirmRemoveCancel

  | WidgetNameConfirmSend
  | WidgetNameConfirmSendCheck
  | WidgetNameConfirmSendContinue
  | WidgetNameConfirmSendCancel
  | WidgetNameConfirmSendList
  deriving (Eq, Ord, Show)

-- | Unique widget name, describing its location in widget tree
type WidgetName = [WidgetNamePart]

-- | Result of drawing a widget
-- it can be a single Brick Widget or a stack of layers
type WidgetDrawing = NonEmpty (B.Widget WidgetName)

-- | Events child widget can send back to parent widget
--
-- Should also be a union type of widget-local event types
data WidgetEvent
  = WidgetEventMenuSelected
  | WidgetEventButtonPressed
  | WidgetEventEditChanged
  | WidgetEventEditLocationChanged
  | WidgetEventListSelected Int
  | WidgetEventCheckboxToggled
  | WidgetEventModalExited

-- | Result of handling a UI event
--
-- Each UI event is sent to the deepest focused widget possible.
-- If its handler returns @WidgetEventNotHandled@, the event "bubbles" up
-- through the widget hierarchy and is fed to parent widget's event handler.
-- Event handler can return @WidgetEventHalt@, which means that the whole
-- application should be halted.
data WidgetEventResult
  = WidgetEventHandled
  | WidgetEventNotHandled
  | WidgetEventHalt

-- | Base widget type
--
-- Contains local widget state @s@, child widgets, drawing routines and event handlers.
-- It is parametrized by parent's @WidgetInfo@ type to allow access to its state
-- through lenses, and also its event queue for child→parent communication via @WidgetEvent@s.
data WidgetInfo s p = WidgetInfo
  { widgetName :: !WidgetName
  , widgetState :: s
  -- ^ Lazy, because can be undefined for stateless widgets
  , widgetChildren :: !(Map WidgetNamePart (Widget (WidgetInfo s p)))
  , widgetFocusList :: ![WidgetNamePart]
  , widgetEventHandlers :: !(Map WidgetNamePart (WidgetEvent -> WidgetEventM s p ()))
  -- ^ Should be refactored into a dependent-map, for something like this:
  -- @Map (e, WidgetNamePart) -> (e -> m ())@
  , widgetEventQueue :: ![(WidgetNamePart, WidgetEvent)]
  -- ^ List of events received from child widgets, is traversed after child event handlers complete
  , widgetEventSend :: !(WidgetEvent -> WidgetEventM s p ())
  -- ^ Send event to parent widget's queue
  , widgetIgnoreVisibility :: Bool
  -- ^ Ignore visibility requests to allow scrolling focused child away from viewport.
  -- This is set when scrolling is initiated and reset after any other user interaction.
  , widgetDraw :: !(s -> WidgetDrawM s p WidgetDrawing)
  , widgetDrawWithFocused :: !(Bool -> s -> WidgetDrawM s p WidgetDrawing)
  , widgetDrawWithFocus :: !(WidgetName -> s -> WidgetDrawM s p WidgetDrawing)
  -- ^ These three routines are just three variants of one routine.
  -- They differ only in a way current focus is handled.
  -- A widget should normally implement only one of them,
  -- as default implementations of others dispatch call automatically.
  , widgetHandleEditKey :: !(KeyboardEditEvent -> WidgetEventM s p WidgetEventResult)
  , widgetHandleKey :: !(KeyboardEvent -> WidgetEventM s p WidgetEventResult)
  , widgetHandlePaste :: !(Text -> WidgetEventM s p WidgetEventResult)
  , widgetHandleMouseDown :: !(B.Location -> WidgetEventM s p WidgetEventResult)
  , widgetHandleScroll :: !(ScrollingAction -> WidgetEventM s p WidgetEventResult)
  , widgetHandleEvent :: !(UiEvent -> WidgetEventM s p ())
  }

instance B.Named (WidgetInfo s p) WidgetName where
  getName = widgetName

-- | Opaque widget
--
-- Parametrized by parent @WidgetInfo@ with existential widget state.
-- This allows parent to store all children in one map for easy UI event propagation.
data Widget p = forall s. Widget (WidgetInfo s p)

instance B.Named (Widget p) WidgetName where
  getName (Widget WidgetInfo{..}) = widgetName

-- | Widget initalization monad
--
-- Allows defining only needed parts of @WidgetInfo@.
-- Should probably give access to parent widget too, but it will require changing @initWidget@ flow.
type WidgetInitM s p = State (WidgetInfo s p) ()

-- | Widget rendering monad
--
-- Reader monad is used, because Brick doesn't allow changing app state during rendering.
-- Local widget state @s@ is passed as a parameter to drawing routine
-- to allow more convenient utilizing of @RecordWildcards@.
type WidgetDrawM s p a = ReaderT (WidgetInfo s p) (Reader p) a

-- | Widget event handler monad
--
-- Allows access to:
-- * @WidgetInfo s p@: widget itself (use 'widgetStateL' to access local state)
-- * @p@: parent widget via lenses and @widgetEvent@
-- * @B.EventM@: Brick event handling environment
type WidgetEventM s p a = StateT (WidgetInfo s p) (StateT p (B.EventM WidgetName)) a

makeLensesWith postfixLFields ''WidgetInfo

----------------------------------------------------------------------------
-- Widget initialization
----------------------------------------------------------------------------

-- | Creates an empty widget with default implementations of all routines
-- and runs supplied constructor on it, returning opaque initialized widget
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
    , widgetIgnoreVisibility = False
    , widgetDraw = return . singleDrawing . const B.emptyWidget
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

-- | Adds an opaque child to current widget
--
-- As child is created before parent (which will probably be changed at some point),
-- the child has to be renamed to include path to parent.
addWidgetChild :: MonadState (WidgetInfo s p) m => WidgetNamePart -> Widget (WidgetInfo s p) -> m ()
addWidgetChild namePart (Widget child) = do
    parentName <- use widgetNameL
    let child' = child{ widgetEventSend = \event -> lift $ widgetEventQueueL %= ((namePart, event):) }
    widgetChildrenL %= Map.insert namePart (rename parentName $ Widget child')
  where
    rename :: WidgetName -> Widget p -> Widget p
    rename parentName (Widget widget@WidgetInfo{..}) =
      Widget widget
        { widgetName = parentName ++ (namePart : widgetName)
        , widgetChildren = Map.map (rename parentName) widgetChildren
        }

setWidgetFocusList :: MonadState (WidgetInfo s p) m => [WidgetNamePart] -> m ()
setWidgetFocusList = assign widgetFocusListL

addWidgetEventHandler :: MonadState (WidgetInfo s p) m => WidgetNamePart -> (WidgetEvent -> WidgetEventM s p ()) -> m ()
addWidgetEventHandler namePart handler = do
  widgetEventHandlersL %= Map.insert namePart handler

setWidgetDraw :: MonadState (WidgetInfo s p) m => (s -> WidgetDrawM s p WidgetDrawing) -> m ()
setWidgetDraw = assign widgetDrawL

setWidgetDrawWithFocused :: MonadState (WidgetInfo s p) m => (Bool -> s -> WidgetDrawM s p WidgetDrawing) -> m ()
setWidgetDrawWithFocused = assign widgetDrawWithFocusedL

setWidgetDrawWithFocus :: MonadState (WidgetInfo s p) m => (WidgetName -> s -> WidgetDrawM s p WidgetDrawing) -> m ()
setWidgetDrawWithFocus = assign widgetDrawWithFocusL

-- | Sets a default scroll handler, which fits for most widgets
setWidgetScrollable :: MonadState (WidgetInfo s p) m => m ()
setWidgetScrollable = assign widgetHandleScrollL $ \action -> do
  name <- use widgetNameL
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

getIgnoreVisibility :: WidgetDrawM s p Bool
getIgnoreVisibility = view widgetIgnoreVisibilityL

-- | Collects focus list from all widget hierarchy to create a global focus ring
--
-- Normally widgets either don't have focus list and are only focused themselves,
-- or include only children in their focus list, but a special value @WidgetNameSelf@
-- can be used to allow focusing the widget itself along with its children
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

-- | Used to properly set focus, when it is being reset by mouse click or screen change
--
-- Goes up along the given path to find closest parent with focus list,
-- then goes down from there to find the first focusable child.
findClosestFocus :: WidgetName -> WidgetName -> Widget p -> WidgetName
findClosestFocus current [] (Widget WidgetInfo{..})
    | n:_ <- catMaybes $ findInChild <$> widgetFocusList = n
    | otherwise = widgetName
  where
    findInChild WidgetNameSelf = Just widgetName
    findInChild namePart = findClosestFocus current [] <$> Map.lookup namePart widgetChildren
findClosestFocus current focus@(np:nps) widget@(Widget WidgetInfo{..})
  | focus `isPrefixOf` current = current
  | np `elem` widgetFocusList
  , Just child <- Map.lookup np widgetChildren = findClosestFocus current nps child
  | otherwise = findClosestFocus current [] widget

liftBrick :: B.EventM WidgetName a -> WidgetEventM s p a
liftBrick = lift . lift

-- | As a widget is parametrized by parent widget, not parent state,
-- we have to convert getters and lenses
widgetParentGetter :: (p -> a) -> (WidgetInfo p q -> a)
widgetParentGetter f = view $ widgetStateL . to f

widgetParentLens :: Lens' p a -> Lens' (WidgetInfo p q) a
widgetParentLens l = widgetStateL . l

-- | Basically this function allows you to run an action which needs
-- @MonadState s@ inside @WidgetEventM s p@.
zoomWidgetState ::
       (Functor (Zoomed m c), Zoom m n s (WidgetInfo s p))
    => m c
    -> n c
zoomWidgetState = zoom widgetStateL

getWidgetState :: WidgetEventM s p s
getWidgetState = zoomWidgetState get

viewWidgetLens :: ReifiedLens' p a -> WidgetDrawM s p a
viewWidgetLens = lift . view . runLens

useWidgetLens :: ReifiedLens' p a -> WidgetEventM s p a
useWidgetLens = lift . use . runLens

assignWidgetLens :: ReifiedLens' p a -> a -> WidgetEventM s p ()
assignWidgetLens (Lens l) = lift . assign l

-- | Send event from child widget to parent
widgetEvent :: WidgetEvent -> WidgetEventM s p ()
widgetEvent event = do
  WidgetInfo{..} <- get
  widgetEventSend event

updateEditable :: (MonadState s m, Eq a) => Lens' s a -> Lens' s a -> a -> m ()
updateEditable origL editL newValue = do
  current <- use origL
  when (current /= newValue) $ do
    origL .= newValue
    editL .= newValue

----------------------------------------------------------------------------
-- Widget rendering
----------------------------------------------------------------------------

drawWidget :: WidgetName -> p -> Widget p -> WidgetDrawing
drawWidget focus parent (Widget widget@WidgetInfo{..}) =
    map visible $ runReader (runReaderT (widgetDrawWithFocus focus widgetState) widget) parent
  where
    visible w
      | focus == widgetName
      , not widgetIgnoreVisibility = B.visible w
      | otherwise = w

drawWidgetChild :: WidgetName -> WidgetInfo s p -> WidgetNamePart -> WidgetDrawing
drawWidgetChild focus widget@WidgetInfo{..} name =
    singleDrawing . maybe B.emptyWidget (last . drawWidget focus widget . withIgnoreVisibility) $ Map.lookup name widgetChildren
  where
    withIgnoreVisibility (Widget child) = Widget child{ widgetIgnoreVisibility = widgetIgnoreVisibility }

singleDrawing :: B.Widget WidgetName -> WidgetDrawing
singleDrawing = (:| [])

layeredDrawing :: B.Widget WidgetName -> [B.Widget WidgetName] -> WidgetDrawing
layeredDrawing = (:|)

----------------------------------------------------------------------------
-- Widget event handling
----------------------------------------------------------------------------

handleWidgetEditKey :: KeyboardEditEvent -> WidgetName -> StateT (Widget p) (StateT p (B.EventM WidgetName)) WidgetEventResult
handleWidgetEditKey editKey name = do
  Widget widget@WidgetInfo{..} <- get
  handleByName widget{ widgetIgnoreVisibility = False } name
    (widgetHandleEditKey editKey)
    (handleWidgetEditKey editKey)

handleWidgetKey :: KeyboardEvent -> WidgetName -> StateT (Widget p) (StateT p (B.EventM WidgetName)) WidgetEventResult
handleWidgetKey key name = do
  Widget widget@WidgetInfo{..} <- get
  handleByName widget{ widgetIgnoreVisibility = False } name
    (widgetHandleKey key)
    (handleWidgetKey key)

handleWidgetPaste :: Text -> WidgetName -> StateT (Widget p) (StateT p (B.EventM WidgetName)) WidgetEventResult
handleWidgetPaste pasted name = do
  Widget widget@WidgetInfo{..} <- get
  handleByName widget{ widgetIgnoreVisibility = False } name
    (widgetHandlePaste pasted)
    (handleWidgetPaste pasted)

handleWidgetMouseDown :: B.Location -> WidgetName -> StateT (Widget p) (StateT p (B.EventM WidgetName)) WidgetEventResult
handleWidgetMouseDown coords name = do
  Widget widget@WidgetInfo{..} <- get
  handleByName widget{ widgetIgnoreVisibility = False } name
    (widgetHandleMouseDown coords)
    (handleWidgetMouseDown coords)

handleWidgetScroll :: ScrollingAction -> WidgetName -> StateT (Widget p) (StateT p (B.EventM WidgetName)) WidgetEventResult
handleWidgetScroll action name = do
  Widget widget@WidgetInfo{..} <- get
  handleByName widget{ widgetIgnoreVisibility = True } name
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

-- | Broadcasts an event to all widgets in the widget tree
handleAll
  :: WidgetInfo s p
  -> WidgetEventM s p ()
  -> StateT (Widget (WidgetInfo s p)) (StateT (WidgetInfo s p) (B.EventM WidgetName)) ()
  -> StateT (Widget p) (StateT p (B.EventM WidgetName)) ()
handleAll widget@WidgetInfo{..} widgetHandler childHandler = do
  (widgetChildren', widget') <- lift . lift $ runStateT (mapM (execStateT childHandler) widgetChildren) widget
  withWidget widget'{ widgetChildren = widgetChildren' } widgetHandler

-- | Finds a widget by name in the widget tree, calls its event handler
-- If event is not successfully handled, calls its parent's event handler, and so on.
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
            WidgetEventHalt -> return res

-- | Executes a particular widget's event handler, then executes parent's handlers for child→parent events, if any
withWidget
  :: forall s p a. WidgetInfo s p
  -> WidgetEventM s p a
  -> StateT (Widget p) (StateT p (B.EventM WidgetName)) a
withWidget widget@WidgetInfo{..} action = do
  let
    subaction :: StateT (WidgetInfo s p) (StateT p (B.EventM WidgetName)) a
    subaction = do
      res <- action
      forM_ widgetEventQueue $ \(namePart, event) -> do
        whenJust (Map.lookup namePart widgetEventHandlers) $ \handler -> do
          handler event
      widgetEventQueueL .= []
      return res
  (res, widget') <- lift $ runStateT subaction widget
  put $ Widget widget'
  return res
