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
    }

makeLensesWith postfixLFields ''ConfirmRemoveWidgetState

initConfirmRemoveWidget :: UiFace -> Widget p
initConfirmRemoveWidget uiFace = initWidget $ do
    setWidgetDrawWithFocus drawConfirmRemoveWidget
    setWidgetHandleKey handleConfirmRemoveWidgetKey
    setWidgetHandleEvent handleConfirmRemoveWidgetEvent
    setWidgetState ConfirmRemoveWidgetState
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
                ]

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

closeDialog :: WidgetEventM ConfirmRemoveWidgetState p ()
closeDialog = do
    UiFace{..} <- use confirmRemoveWidgetUiFaceL
    liftIO $ putUiEvent $ UiConfirmEvent UiConfirmDone
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

handleConfirmRemoveWidgetEvent
    :: UiEvent
    -> WidgetEventM ConfirmRemoveWidgetState p ()
handleConfirmRemoveWidgetEvent = \case
    UiConfirmEvent (UiConfirmRequest requestResultVar (UiConfirmRemove requestDelItem)) -> do
        confirmRemoveWidgetDelRequestL .= Just DeleteRequest {..}
        updateFocusList
    _ -> pass
