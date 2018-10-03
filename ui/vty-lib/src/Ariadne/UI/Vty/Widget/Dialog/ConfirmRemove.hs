module Ariadne.UI.Vty.Widget.Dialog.ConfirmRemove
    ( initConfirmRemoveWidget
    ) where

import Control.Lens (makeLensesWith, (.=))
import Formatting

import qualified Brick as B

import Ariadne.UIConfig
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Dialog.Utils
import Ariadne.UI.Vty.Widget.Form.Checkbox
<<<<<<< 223c107eeadc91a5fb6a68511f53618532393f93
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.Util

data ConfirmRemoveWidgetState = ConfirmRemoveWidgetState
    { confirmRemoveWidgetUiFace     :: !UiFace
    , confirmRemoveWidgetDelRequest :: !(Maybe DeleteRequest)
    , confirmRemoveWidgetCheck      :: !Bool
    , confirmRemoveWidgetName       :: !Text
    , confirmRemoveWidgetDialog     :: !DialogState
    }

data DeleteRequest = DeleteRequest
    { requestResultVar :: !(MVar Bool)
    , requestDelItem   :: !UiDeletingItem
=======
import Ariadne.Util

data ConfirmRemoveWidgetState = ConfirmRemoveWidgetState
    { confirmRemoveWidgetUiFace    :: !UiFace
    , confirmRemoveWidgetResultVar :: !(Maybe (MVar Bool))
    , confirmRemoveWidgetDelItem   :: !(Maybe (UiDeletingItem))
    , confirmRemoveWidgetCheck     :: !Bool
    , confirmRemoveWidgetDialog    :: !DialogState
>>>>>>> [AD-207] improve TUI dialogs
    }

makeLensesWith postfixLFields ''ConfirmRemoveWidgetState

initConfirmRemoveWidget :: UiFace -> Widget p
initConfirmRemoveWidget uiFace = initWidget $ do
    setWidgetDrawWithFocus drawConfirmRemoveWidget
    setWidgetHandleKey handleConfirmRemoveWidgetKey
    setWidgetHandleEvent handleConfirmRemoveWidgetEvent
    setWidgetState ConfirmRemoveWidgetState
<<<<<<< 223c107eeadc91a5fb6a68511f53618532393f93
        { confirmRemoveWidgetUiFace     = uiFace
        , confirmRemoveWidgetDelRequest = Nothing
        , confirmRemoveWidgetCheck      = False
        , confirmRemoveWidgetName       = ""
        , confirmRemoveWidgetDialog     = newDialogState deleteHeaderMessage
        }
    
    addWidgetChild WidgetNameConfirmRemoveCheck
        $ initCheckboxWidget deleteSureMessage
        $ widgetParentLens confirmRemoveWidgetCheckL
    addWidgetChild WidgetNameConfirmRemoveName $ initEditWidget $
        widgetParentLens confirmRemoveWidgetNameL

    addDialogButton confirmRemoveWidgetDialogL
        WidgetNameConfirmRemoveContinue "Delete" performContinue
    addDialogButton confirmRemoveWidgetDialogL
        WidgetNameConfirmRemoveCancel "Cancel" performCancel

    addWidgetEventHandler WidgetNameConfirmRemoveCheck $ \case
        WidgetEventCheckboxToggled -> updateFocusList
        _ -> pass
=======
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
>>>>>>> [AD-207] improve TUI dialogs

    setWidgetFocusList
        [ WidgetNameConfirmRemoveCheck
        , WidgetNameConfirmRemoveCancel
        , WidgetNameConfirmRemoveContinue
        ]

drawConfirmRemoveWidget
    :: WidgetName
    -> ConfirmRemoveWidgetState
    -> WidgetDrawM ConfirmRemoveWidgetState p WidgetDrawing
drawConfirmRemoveWidget focus ConfirmRemoveWidgetState{..} =
    case confirmRemoveWidgetDelRequest of
        Nothing -> return $ singleDrawing B.emptyWidget
<<<<<<< 223c107eeadc91a5fb6a68511f53618532393f93
        Just DeleteRequest {..} -> do
            widget <- ask
            let drawChild = last . drawWidgetChild focus widget
                itemName = delItemName requestDelItem
                hasNameToConfirm = isJust $ itemNameToConfirm requestDelItem
            drawInsideDialog confirmRemoveWidgetDialog focus $ B.vBox
                [ B.padTopBottom 1 . B.txtWrap $
                  deleteIntroMkMessage itemTypeFormat itemName requestDelItem
                , drawChild WidgetNameConfirmRemoveCheck
                , if confirmRemoveWidgetCheck && hasNameToConfirm
                  then B.padTopBottom 1 $ B.hBox
                    [ B.padLeftRight 2 . B.txtWrap $
                      deleteRetypeMkMessage itemTypeFormat requestDelItem
                    , drawChild WidgetNameConfirmRemoveName
                    ]
                  else B.emptyWidget
=======
        Just _  -> do
            widget <- ask
            let drawChild = last . drawWidgetChild focus widget
            drawInsideDialog confirmRemoveWidgetDialog focus $ B.vBox
                [ B.padTopBottom 1 . B.txtWrap $
                  "Do you really want to delete this" <> itemTypeText <> "?"
                , drawChild WidgetNameConfirmRemoveCheck
>>>>>>> [AD-207] improve TUI dialogs
                ]
  where
    itemTypeText = case confirmRemoveWidgetDelItem of
        Just UiDelWallet -> " wallet"
        Just UiDelAccount -> " account"
        _ -> ""

itemTypeFormat :: Format r (UiDeletingItem -> r)
itemTypeFormat = later $ \case
    UiDelWallet _  -> "wallet"
    UiDelAccount _ -> "account"

-- no confirm is requested for an account (or something with no name)
itemNameToConfirm :: UiDeletingItem -> Maybe Text
itemNameToConfirm = \case
    UiDelWallet maybeName -> maybeName
    _ -> Nothing

-- only for rendering, gives back "this" if it doesn't know any better
delItemName :: UiDeletingItem -> Text
delItemName delItem = fromMaybe "this" $ case delItem of
    UiDelWallet maybeName -> maybeName
    UiDelAccount maybeName -> maybeName

handleConfirmRemoveWidgetKey
    :: KeyboardEvent
    -> WidgetEventM ConfirmRemoveWidgetState p WidgetEventResult
handleConfirmRemoveWidgetKey = \case
    KeyEnter -> performContinue $> WidgetEventHandled
    KeyNavigation -> performCancel $> WidgetEventHandled
    _ -> return WidgetEventNotHandled

performContinue :: WidgetEventM ConfirmRemoveWidgetState p ()
<<<<<<< 223c107eeadc91a5fb6a68511f53618532393f93
performContinue = whenJustM (use confirmRemoveWidgetDelRequestL) $
    \DeleteRequest {..} -> unlessM (nameNotConfirmed requestDelItem) $
        putMVar requestResultVar True *> closeDialog
  where
    nameNotConfirmed delItem = case itemNameToConfirm delItem of
        Just nameValue -> do
            confirmNameValue <- use confirmRemoveWidgetNameL
            return $ nameValue /= confirmNameValue
        Nothing -> return False

performCancel :: WidgetEventM ConfirmRemoveWidgetState p ()
performCancel = whenJustM (use confirmRemoveWidgetDelRequestL) $
    \DeleteRequest {..} -> putMVar requestResultVar False *> closeDialog
=======
performContinue = whenJustM (use confirmRemoveWidgetResultVarL) $ \resultVar ->
    whenM (use confirmRemoveWidgetCheckL) $ putMVar resultVar True *> closeDialog

performCancel :: WidgetEventM ConfirmRemoveWidgetState p ()
performCancel = whenJustM (use confirmRemoveWidgetResultVarL) $ \resultVar ->
    putMVar resultVar False *> closeDialog
>>>>>>> [AD-207] improve TUI dialogs

closeDialog :: WidgetEventM ConfirmRemoveWidgetState p ()
closeDialog = do
    UiFace{..} <- use confirmRemoveWidgetUiFaceL
    liftIO $ putUiEvent $ UiConfirmEvent UiConfirmDone
<<<<<<< 223c107eeadc91a5fb6a68511f53618532393f93
    confirmRemoveWidgetDelRequestL .= Nothing
    confirmRemoveWidgetCheckL .= False
    confirmRemoveWidgetNameL .= ""

updateFocusList :: WidgetEventM ConfirmRemoveWidgetState p ()
updateFocusList = do
    removeCheck <- use confirmRemoveWidgetCheckL
    delRequest <- use confirmRemoveWidgetDelRequestL
    let nameToConfirm = itemNameToConfirm . requestDelItem <$> delRequest
    lift . setWidgetFocusList $
        if removeCheck && isJust nameToConfirm
        then 
            [ WidgetNameConfirmRemoveCheck
            , WidgetNameConfirmRemoveName
            ] <> dialogButtons
        else WidgetNameConfirmRemoveCheck : dialogButtons
  where
    dialogButtons =
        [ WidgetNameConfirmRemoveCancel
        , WidgetNameConfirmRemoveContinue
        ]
=======
    confirmRemoveWidgetResultVarL .= Nothing
    confirmRemoveWidgetDelItemL .= Nothing
    confirmRemoveWidgetCheckL .= False
>>>>>>> [AD-207] improve TUI dialogs

handleConfirmRemoveWidgetEvent
    :: UiEvent
    -> WidgetEventM ConfirmRemoveWidgetState p ()
handleConfirmRemoveWidgetEvent = \case
<<<<<<< 223c107eeadc91a5fb6a68511f53618532393f93
    UiConfirmEvent (UiConfirmRequest requestResultVar (UiConfirmRemove requestDelItem)) -> do
        confirmRemoveWidgetDelRequestL .= Just DeleteRequest {..}
        updateFocusList
=======
    UiConfirmEvent (UiConfirmRequest resVar (UiConfirmRemove delItem)) -> do
        confirmRemoveWidgetResultVarL .= Just resVar
        confirmRemoveWidgetDelItemL   .= Just delItem
>>>>>>> [AD-207] improve TUI dialogs
    _ -> pass
