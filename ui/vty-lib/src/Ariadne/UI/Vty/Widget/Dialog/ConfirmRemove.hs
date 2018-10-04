module Ariadne.UI.Vty.Widget.Dialog.ConfirmRemove
    ( initConfirmRemoveWidget
    ) where

import Control.Lens (makeLensesWith, (.=))

import qualified Brick as B

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Dialog.Utils
import Ariadne.UI.Vty.Widget.Form.Checkbox
import Ariadne.Util

data ConfirmRemoveWidgetState = ConfirmRemoveWidgetState
    { confirmRemoveWidgetUiFace    :: !UiFace
    , confirmRemoveWidgetResultVar :: !(Maybe (MVar Bool))
    , confirmRemoveWidgetDelItem   :: !(Maybe (UiDeletingItem))
    , confirmRemoveWidgetCheck     :: !Bool
    , confirmRemoveWidgetDialog    :: !DialogState
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
        , confirmRemoveWidgetCheck     = False
        , confirmRemoveWidgetDialog    = newDialogState "Confirm Deletion"
        }
    
    addWidgetChild WidgetNameConfirmRemoveCheck
        $ initCheckboxWidget "Make sure you have access to backup before \
                              \continuing. Otherwise you will lose all your \
                              \funds connected to this."
        $ widgetParentLens confirmRemoveWidgetCheckL

    addDialogButton confirmRemoveWidgetDialogL
        WidgetNameConfirmRemoveContinue "Delete" performContinue
    addDialogButton confirmRemoveWidgetDialogL
        WidgetNameConfirmRemoveCancel "Cancel" performCancel

    setWidgetFocusList
        [ WidgetNameSelf
        , WidgetNameConfirmRemoveCheck
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
        Just _  -> do
            widget <- ask
            let drawChild = last . drawWidgetChild focus widget
            drawInsideDialog confirmRemoveWidgetDialog focus $ B.vBox
                [ B.padTopBottom 1 . B.txtWrap $
                  "Do you really want to delete this" <> itemTypeText <> "?"
                , drawChild WidgetNameConfirmRemoveCheck
                ]
  where
    itemTypeText = case confirmRemoveWidgetDelItem of
        Just UiDelWallet -> " wallet"
        Just UiDelAccount -> " account"
        _ -> ""

handleConfirmRemoveWidgetKey
    :: KeyboardEvent
    -> WidgetEventM ConfirmRemoveWidgetState p WidgetEventResult
handleConfirmRemoveWidgetKey = \case
    KeyEnter -> performContinue $> WidgetEventHandled
    KeyNavigation -> performCancel $> WidgetEventHandled
    _ -> return WidgetEventNotHandled

performContinue :: WidgetEventM ConfirmRemoveWidgetState p ()
performContinue = whenJustM (use confirmRemoveWidgetResultVarL) $ \resultVar ->
    whenM (use confirmRemoveWidgetCheckL) $ putMVar resultVar True *> closeDialog

performCancel :: WidgetEventM ConfirmRemoveWidgetState p ()
performCancel = whenJustM (use confirmRemoveWidgetResultVarL) $ \resultVar ->
    putMVar resultVar False *> closeDialog

closeDialog :: WidgetEventM ConfirmRemoveWidgetState p ()
closeDialog = do
    UiFace{..} <- use confirmRemoveWidgetUiFaceL
    liftIO $ putUiEvent $ UiConfirmEvent UiConfirmDone
    confirmRemoveWidgetResultVarL .= Nothing
    confirmRemoveWidgetDelItemL .= Nothing
    confirmRemoveWidgetCheckL .= False

handleConfirmRemoveWidgetEvent
    :: UiEvent
    -> WidgetEventM ConfirmRemoveWidgetState p ()
handleConfirmRemoveWidgetEvent = \case
    UiConfirmEvent (UiConfirmRequest resVar (UiConfirmRemove delItem)) -> do
        confirmRemoveWidgetResultVarL .= Just resVar
        confirmRemoveWidgetDelItemL   .= Just delItem
    _ -> pass
