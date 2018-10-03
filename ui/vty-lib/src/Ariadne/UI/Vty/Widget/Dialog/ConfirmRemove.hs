module Ariadne.UI.Vty.Widget.Dialog.ConfirmRemove
    ( initConfirmRemoveWidget
    ) where

import Control.Lens (makeLensesWith, (.=))

import qualified Brick as B
import qualified Brick.Widgets.Center as B

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Dialog.Utils
import Ariadne.Util

data ConfirmRemoveWidgetState = ConfirmRemoveWidgetState
    { confirmRemoveWidgetUiFace    :: !UiFace
    , confirmRemoveWidgetResultVar :: !(Maybe (MVar Bool))
    , confirmRemoveWidgetDelItem   :: !(Maybe (UiDeletingItem))
    }

makeLensesWith postfixLFields ''ConfirmRemoveWidgetState

initConfirmRemoveWidget :: UiFace -> Widget p
initConfirmRemoveWidget uiFace = initWidget $ do
    setWidgetDrawWithFocus drawConfirmRemoveWidget
    setWidgetHandleKey handleConfirmRemoveWidgetKey
    setWidgetHandleEvent handleConfirmRemoveWidgetEvent
    setWidgetState ConfirmRemoveWidgetState
        { confirmRemoveWidgetUiFace    = uiFace
        , confirmRemoveWidgetResultVar = Nothing
        , confirmRemoveWidgetDelItem   = Nothing
        }

    addDialogButton WidgetNameConfirmRemoveCancel "Cancel" performCancel
    addDialogButton WidgetNameConfirmRemoveContinue "Continue" performContinue

    setWidgetFocusList
        [ WidgetNameSelf
        , WidgetNameConfirmRemoveContinue
        , WidgetNameConfirmRemoveCancel
        ]

drawConfirmRemoveWidget
    :: WidgetName
    -> ConfirmRemoveWidgetState
    -> WidgetDrawM ConfirmRemoveWidgetState p WidgetDrawing
drawConfirmRemoveWidget focus ConfirmRemoveWidgetState{..} =
    case confirmRemoveWidgetResultVar of
        Nothing -> return $ singleDrawing B.emptyWidget
        Just _  -> drawInsideDialog "Confirm Remove" focus
            [WidgetNameConfirmRemoveCancel, WidgetNameConfirmRemoveContinue] $
            B.vBox
                [ B.hCenter $ B.txt "WARNING"
                , B.padTopBottom 1 $ B.txtWrap "Removing cannot be reverted. Check twice before doing this."
                ]

handleConfirmRemoveWidgetKey
    :: KeyboardEvent
    -> WidgetEventM ConfirmRemoveWidgetState p WidgetEventResult
handleConfirmRemoveWidgetKey = \case
    KeyEnter -> performContinue $> WidgetEventHandled
    KeyNavigation -> performCancel $> WidgetEventHandled
    _ -> return WidgetEventNotHandled

performContinue :: WidgetEventM ConfirmRemoveWidgetState p ()
performContinue = do
    ConfirmRemoveWidgetState{..} <- get
    whenJust confirmRemoveWidgetResultVar $ \resultVar -> do
        putMVar resultVar True
        UiFace{..} <- use confirmRemoveWidgetUiFaceL
        liftIO $ putUiEvent $ UiConfirmEvent UiConfirmDone
        confirmRemoveWidgetResultVarL .= Nothing
        confirmRemoveWidgetDelItemL .= Nothing

performCancel :: WidgetEventM ConfirmRemoveWidgetState p ()
performCancel = do
    ConfirmRemoveWidgetState{..} <- get
    whenJust confirmRemoveWidgetResultVar $ \resultVar -> do
        putMVar resultVar False
        UiFace{..} <- use confirmRemoveWidgetUiFaceL
        liftIO $ putUiEvent $ UiConfirmEvent UiConfirmDone
        confirmRemoveWidgetResultVarL .= Nothing

handleConfirmRemoveWidgetEvent
    :: UiEvent
    -> WidgetEventM ConfirmRemoveWidgetState p ()
handleConfirmRemoveWidgetEvent = \case
    UiConfirmEvent (UiConfirmRequest resVar (UiConfirmRemove delItem)) -> do
        confirmRemoveWidgetResultVarL .= Just resVar
        confirmRemoveWidgetDelItemL .= Just delItem
    _ -> pass
