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
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.Util

data ConfirmRemoveWidgetState = ConfirmRemoveWidgetState
    { confirmRemoveWidgetUiFace    :: !UiFace
    , confirmRemoveWidgetResultVar :: !(Maybe (MVar Bool))
    , confirmRemoveWidgetDelItem   :: !(Maybe UiDeletingItem)
    , confirmRemoveWidgetCheck     :: !Bool
    , confirmRemoveWidgetName      :: !Text
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
        , confirmRemoveWidgetName      = ""
        , confirmRemoveWidgetDialog    = newDialogState "Confirm Deletion"
        }
    
    addWidgetChild WidgetNameConfirmRemoveCheck
        $ initCheckboxWidget "Make sure you have access to backup before \
                              \continuing. Otherwise you will lose all your \
                              \funds connected to this."
        $ widgetParentLens confirmRemoveWidgetCheckL
    addWidgetChild WidgetNameConfirmRemoveName $ initEditWidget $
        widgetParentLens confirmRemoveWidgetNameL

    addDialogButton confirmRemoveWidgetDialogL
        WidgetNameConfirmRemoveContinue "Delete" performContinue
    addDialogButton confirmRemoveWidgetDialogL
        WidgetNameConfirmRemoveCancel "Cancel" performCancel

    setWidgetFocusList
        [ WidgetNameSelf
        , WidgetNameConfirmRemoveCheck
        , WidgetNameConfirmRemoveName
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
                  "Do you really want to delete " <> itemName <> itemTypeText <> "?"
                , drawChild WidgetNameConfirmRemoveCheck
                , if confirmRemoveWidgetCheck && hasNameToConfirm
                  then B.padTopBottom 1 $ B.hBox
                    [ B.padLeftRight 1 . B.txtWrap $
                      "Type" <> itemTypeText <> " name to confirm deletion"
                    , drawChild WidgetNameConfirmRemoveName
                    ]
                  else B.emptyWidget
                ]
  where
    itemTypeText = case confirmRemoveWidgetDelItem of
        Just (UiDelWallet _) -> " wallet"
        Just (UiDelAccount _) -> " account"
        _ -> ""
    itemName = delItemName confirmRemoveWidgetDelItem
    hasNameToConfirm = isJust (itemNameToConfirm confirmRemoveWidgetDelItem)

-- no confirm is requested for an account (or something with no name)
itemNameToConfirm :: Maybe UiDeletingItem -> Maybe Text
itemNameToConfirm = \case
    Just (UiDelWallet maybeName) -> maybeName
    _ -> Nothing

-- only for rendering, gives back "this" if it doesn't know any better
delItemName :: Maybe UiDeletingItem -> Text
delItemName maybeDelItem = fromMaybe "this" $ case maybeDelItem of
  Just (UiDelWallet maybeName) -> maybeName
  Just (UiDelAccount maybeName) -> maybeName
  _ -> Nothing

handleConfirmRemoveWidgetKey
    :: KeyboardEvent
    -> WidgetEventM ConfirmRemoveWidgetState p WidgetEventResult
handleConfirmRemoveWidgetKey = \case
    KeyEnter -> performContinue $> WidgetEventHandled
    KeyNavigation -> performCancel $> WidgetEventHandled
    _ -> return WidgetEventNotHandled

performContinue :: WidgetEventM ConfirmRemoveWidgetState p ()
performContinue = whenJustM (use confirmRemoveWidgetResultVarL) $ \resultVar ->
    unlessM (nameNotConfirmed) $ putMVar resultVar True *> closeDialog
  where
    nameNotConfirmed = use confirmRemoveWidgetDelItemL >>= \delItem ->
        case itemNameToConfirm delItem of
            Just nameValue -> do
                confirmNameValue <- use confirmRemoveWidgetNameL
                return $ nameValue /= confirmNameValue
            Nothing -> return False

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
    confirmRemoveWidgetNameL .= ""

handleConfirmRemoveWidgetEvent
    :: UiEvent
    -> WidgetEventM ConfirmRemoveWidgetState p ()
handleConfirmRemoveWidgetEvent = \case
    UiConfirmEvent (UiConfirmRequest resVar (UiConfirmRemove delItem)) -> do
        confirmRemoveWidgetResultVarL .= Just resVar
        confirmRemoveWidgetDelItemL   .= Just delItem
    _ -> pass
