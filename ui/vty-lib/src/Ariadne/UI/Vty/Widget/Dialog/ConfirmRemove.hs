module Ariadne.UI.Vty.Widget.Dialog.ConfirmRemove
    ( initConfirmRemoveWidget
    ) where

import Control.Lens (makeLensesWith, (.=))
import qualified Data.Text as T
import Formatting

import qualified Brick as B

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Dialog.Utils
import Ariadne.UI.Vty.Widget.Form.Checkbox
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.UIConfig
import Ariadne.Util

data ConfirmRemoveWidgetState = ConfirmRemoveWidgetState
    { confirmRemoveWidgetUiFace           :: !UiFace
    , confirmRemoveWidgetDelRequest       :: !(Maybe DeleteRequest)
    , confirmRemoveWidgetCheck            :: !Bool
    , confirmRemoveWidgetShowDeletingObjs :: !Bool
    , confirmRemoveWidgetName             :: !Text
    , confirmRemoveWidgetExpandTitle      :: !Text
    , confirmRemoveWidgetCheckTitle       :: !Text
    , confirmRemoveWidgetDialog           :: !DialogState
    }

data DeleteRequest = DeleteRequest
    { requestResultVar :: !(MVar Bool)
    , requestDelItem   :: !UiDeletingItem
    }

data MessagesOnDelWidget =
  MessagesOnDelWidget
    { header          :: !Text
    , intro           :: !Text
    , isSureMessage   :: !Text
    , reTypeMessage   :: !Text
    , expandingMessge :: !Text
    , confirmMsg      :: !Text
    }

makeLensesWith postfixLFields ''ConfirmRemoveWidgetState


makeMessages :: UiDeletingItem -> MessagesOnDelWidget
makeMessages (UiDelUnknownKeys _) =
  let header = rmUnkownKeysHeaderMessage
      intro = rmUnknownKeysIntroMessage
      expandingMessge = expandingMessageRemovingKeys
      isSureMessage = rmUnknownKeysSureMessage
      reTypeMessage = rmUnknownKeysRetypeMkMessage rmUnknownKeysRetypeConfirm
      confirmMsg = rmUnknownKeysRetypeConfirm
  in MessagesOnDelWidget{..}
makeMessages (UiDelBrokenWallets _) =
  let header = rmBrokenWalletsHeaderMessage
      intro = rmBrokenWalletsIntroMkMessage
      expandingMessge = expandingMessageRemovingWallets
      isSureMessage = rmBrokenWalletDelSureMessage
      reTypeMessage = rmBrokenWltRetypeMkMessage rmBrokenRetypeConfirm
      confirmMsg = rmBrokenRetypeConfirm
  in MessagesOnDelWidget{..}
makeMessages itemType =
  let header = T.toUpper $ deleteHeaderMkMessage itemTypeFormat itemType
      itemName = delItemName itemType
      intro = deleteIntroMkMessage itemTypeFormat itemName itemType
      expandingMessge = ""
      isSureMessage = deleteSureMkMessage itemTypeFormat itemType
      reTypeMessage = deleteRetypeMkMessage itemTypeFormat itemType
      confirmMsg = itemName
  in MessagesOnDelWidget{..}

initConfirmRemoveWidget :: UiFace -> Widget p
initConfirmRemoveWidget uiFace = initWidget $ do
    setWidgetDrawWithFocus drawConfirmRemoveWidget
    setWidgetHandleKey handleConfirmRemoveWidgetKey
    setWidgetHandleEvent handleConfirmRemoveWidgetEvent
    setWidgetScrollable
    setWidgetState ConfirmRemoveWidgetState
        { confirmRemoveWidgetUiFace           = uiFace
        , confirmRemoveWidgetDelRequest       = Nothing
        , confirmRemoveWidgetCheck            = False
        , confirmRemoveWidgetShowDeletingObjs = False
        , confirmRemoveWidgetName             = ""
        , confirmRemoveWidgetExpandTitle      = ""
        , confirmRemoveWidgetCheckTitle       = ""
        , confirmRemoveWidgetDialog           = newDialogState deleteHeaderMessage
        }

    addWidgetChild WidgetNameConfirmRemoveName $ initEditWidget $
        widgetParentLens confirmRemoveWidgetNameL

    addWidgetChild WidgetNameConfirmRemoveCheck
        $ initCheckboxWidget Check (widgetParentLens confirmRemoveWidgetCheckTitleL)
        $ widgetParentLens confirmRemoveWidgetCheckL

    addDialogButton confirmRemoveWidgetDialogL
        WidgetNameConfirmRemoveContinue "Delete" performContinue
    addDialogButton confirmRemoveWidgetDialogL
        WidgetNameConfirmRemoveCancel "Cancel" performCancel

    addWidgetEventHandler WidgetNameConfirmRemoveCheck $ \case
        WidgetEventCheckboxToggled -> updateFocusList
        _ -> pass

    addWidgetEventHandler WidgetNameConfirmRemoveKeysExpand $ \case
        WidgetEventCheckboxToggled -> updateFocusList
        _ -> pass

drawConfirmRemoveWidget
    :: WidgetName
    -> ConfirmRemoveWidgetState
    -> WidgetDrawM ConfirmRemoveWidgetState p WidgetDrawing
drawConfirmRemoveWidget focus ConfirmRemoveWidgetState{..} =
    case confirmRemoveWidgetDelRequest of
        Nothing -> return $ singleDrawing B.emptyWidget
        Just DeleteRequest {..} -> do
            let MessagesOnDelWidget{..} = makeMessages requestDelItem
            widget <- ask
            widgetName <- getWidgetName
            let drawChild = last . drawWidgetChild focus widget
                hasNameToConfirm = isJust $ itemNameToConfirm requestDelItem
                delOnStartupObjectsWidget = case requestDelItem of
                    UiDelUnknownKeys unknownRootIDs -> B.vBox $
                      [ drawChild WidgetNameConfirmRemoveKeysExpand
                      , showDelOnStartupObjects unknownRootIDs
                      ]
                    UiDelBrokenWallets walletsWOSecretKeys -> B.vBox $
                      [ drawChild WidgetNameConfirmRemoveKeysExpand
                      , showDelOnStartupObjects walletsWOSecretKeys
                      ]
                    _ -> B.emptyWidget
                showDelOnStartupObjects toDeleteObjectsTxt = B.padLeftRight 4 $
                      if confirmRemoveWidgetShowDeletingObjs
                      then scrollingViewport widgetName B.Vertical $ B.txtWrap toDeleteObjectsTxt
                      else B.emptyWidget
            drawInsideDialog confirmRemoveWidgetDialog focus $ B.vBox
                [ B.padTopBottom 1 . B.txtWrap $ intro
                , delOnStartupObjectsWidget
                , drawChild WidgetNameConfirmRemoveCheck
                , if confirmRemoveWidgetCheck && hasNameToConfirm
                  then B.padTopBottom 1 $ B.hBox
                    [ B.padLeftRight 2 . B.txtWrap $ reTypeMessage
                    , drawChild WidgetNameConfirmRemoveName
                    ]
                  else B.emptyWidget
                ]

itemTypeFormat :: Format r (UiDeletingItem -> r)
itemTypeFormat = later $ \case
    UiDelWallet _  -> "wallet"
    UiDelAccount _ -> "account"
    UiDelUnknownKeys _ -> "keys"
    UiDelBrokenWallets _ -> "wallets"

-- no confirm is requested for an account (or something with no name)
itemNameToConfirm :: UiDeletingItem -> Maybe Text
itemNameToConfirm = \case
    UiDelWallet maybeName -> maybeName
    UiDelUnknownKeys _ -> Just "Yes"
    UiDelBrokenWallets _ -> Just "Yes"
    _ -> Nothing

-- only for rendering, gives back "this" if it doesn't know any better
delItemName :: UiDeletingItem -> Text
delItemName delItem = fromMaybe "this" $ case delItem of
    UiDelWallet maybeName -> maybeName
    UiDelAccount maybeName -> maybeName
    UiDelUnknownKeys _ -> Just "unknown keys"
    UiDelBrokenWallets _ -> Just "broken wallets"

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
    confirmRemoveWidgetShowDeletingObjsL .= False

updateFocusList :: WidgetEventM ConfirmRemoveWidgetState p ()
updateFocusList = do
    removeCheck <- use confirmRemoveWidgetCheckL
    deleteRequest <- use confirmRemoveWidgetDelRequestL
    let deleteItem = requestDelItem <$> deleteRequest
    let nameToConfirm = itemNameToConfirm =<< deleteItem
    lift . setWidgetFocusList $
        focusList deleteItem (removeCheck && isJust nameToConfirm)

focusList :: Maybe UiDeletingItem -> Bool -> [WidgetNamePart]
focusList deleteItemM removeCheck =
    let startWidgetsBase = [ WidgetNameConfirmRemoveKeysExpand, WidgetNameConfirmRemoveCheck]
        mainWidgetPart deleteItem = case deleteItem of
            (UiDelUnknownKeys _) -> startWidgetsBase
            (UiDelBrokenWallets _) -> startWidgetsBase
            _  -> [WidgetNameConfirmRemoveCheck]
        endWidgetPart =
            bool dialogButtons (WidgetNameConfirmRemoveName : dialogButtons) removeCheck
        dialogButtons =
            [ WidgetNameConfirmRemoveCancel
            , WidgetNameConfirmRemoveContinue
            ]
    in case deleteItemM of
        Just deleteItem -> mainWidgetPart deleteItem <> endWidgetPart
        Nothing -> []

handleConfirmRemoveWidgetEvent
    :: UiEvent
    -> WidgetEventM ConfirmRemoveWidgetState p ()
handleConfirmRemoveWidgetEvent = \case
    UiConfirmEvent (UiConfirmRequest requestResultVar (UiConfirmRemove requestDelItem)) -> do
        confirmRemoveWidgetDelRequestL .= Just DeleteRequest {..}
        let msgs = makeMessages requestDelItem
        confirmRemoveWidgetDialogL . dialogLabelL .= header msgs
        confirmRemoveWidgetCheckTitleL .= isSureMessage msgs
        case requestDelItem of
            UiDelUnknownKeys _ -> confirmRemoveWidgetExpandTitleL .= expandingMessge msgs
            UiDelBrokenWallets _ -> confirmRemoveWidgetExpandTitleL .= expandingMessge msgs
            _ -> pass
        -- Adding Checkbox widget here to make the label dependent on deleted item.
        lift $ case requestDelItem of
                  (UiDelUnknownKeys _) -> do
                        addWidgetChild WidgetNameConfirmRemoveKeysExpand
                            $ initCheckboxWidget Expand (widgetParentLens confirmRemoveWidgetExpandTitleL)
                            $ widgetParentLens confirmRemoveWidgetShowDeletingObjsL
                  (UiDelBrokenWallets _) -> do
                        addWidgetChild WidgetNameConfirmRemoveKeysExpand
                            $ initCheckboxWidget Expand (widgetParentLens confirmRemoveWidgetExpandTitleL)
                            $ widgetParentLens confirmRemoveWidgetShowDeletingObjsL
                  _ -> pass
        updateFocusList
    _ -> pass
