module Ariadne.UI.Vty.Widget.Dialog.ConfirmSend
    ( initConfirmSendWidget
    ) where

import Control.Lens (makeLensesWith, (.=))

import qualified Brick as B

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Dialog.Utils
import Ariadne.UI.Vty.Widget.Form.Checkbox
import Ariadne.Util

data ConfirmSendWidgetState = ConfirmSendWidgetState
    { confirmSendWidgetUiFace     :: !UiFace
    , confirmSendWidgetOutputList :: ![(Text, Text, Text)]
    , confirmSendWidgetResultVar  :: !(Maybe (MVar Bool))
    , confirmSendWidgetCheck      :: !Bool
    , confirmSendWidgetDialog     :: !DialogState
    }

makeLensesWith postfixLFields ''ConfirmSendWidgetState

initConfirmSendWidget :: UiFace -> Widget p
initConfirmSendWidget uiFace = initWidget $ do
    setWidgetDrawWithFocus drawConfirmSendWidget
    setWidgetHandleKey handleConfirmSendWidgetKey
    setWidgetHandleEvent handleConfirmSendWidgetEvent
    setWidgetState ConfirmSendWidgetState
        { confirmSendWidgetUiFace     = uiFace
        , confirmSendWidgetOutputList = []
        , confirmSendWidgetResultVar  = Nothing
        , confirmSendWidgetCheck      = False
        , confirmSendWidgetDialog     = newDialogState "Confirm Transaction"
        }

    addWidgetChild WidgetNameConfirmSendCheck
        $ initCheckboxWidget "I understand that continuing with this operation\
                              \ will make it definitive and irreversible."
        $ widgetParentLens confirmSendWidgetCheckL

    addDialogButton confirmSendWidgetDialogL
        WidgetNameConfirmSendContinue "Send" performContinue
    addDialogButton confirmSendWidgetDialogL
        WidgetNameConfirmSendCancel "Cancel" performCancel

    setWidgetFocusList
        [ WidgetNameSelf
        , WidgetNameConfirmSendContinue
        , WidgetNameConfirmSendCancel
        ]


drawConfirmSendWidget
    :: WidgetName
    -> ConfirmSendWidgetState
    -> WidgetDrawM ConfirmSendWidgetState p WidgetDrawing
drawConfirmSendWidget focus ConfirmSendWidgetState{..} = do
    case confirmSendWidgetResultVar of
        Nothing -> return $ singleDrawing B.emptyWidget
        Just _  -> do
            widget <- ask
            widgetName <- getWidgetName
            let drawChild = last . drawWidgetChild focus widget
            drawInsideDialog confirmSendWidgetDialog focus $ B.vBox
                [ B.padTopBottom 1 $ B.txtWrap $ "This is the list of this \
                  \operation's output transitions. Please review them carefully."
                , scrollingViewport widgetName B.Both . B.vBox $
                  map (B.txt . transactionLine) confirmSendWidgetOutputList
                , drawChild WidgetNameConfirmSendCheck
                ]

transactionLine :: (Text, Text, Text) -> Text
transactionLine (address, amount, coin) = amount <> " " <> coin <> " to " <> address

handleConfirmSendWidgetKey
    :: KeyboardEvent
    -> WidgetEventM ConfirmSendWidgetState p WidgetEventResult
handleConfirmSendWidgetKey = \case
    KeyEnter -> performContinue $> WidgetEventHandled
    KeyNavigation -> performCancel $> WidgetEventHandled
    _ -> return WidgetEventNotHandled

performContinue :: WidgetEventM ConfirmSendWidgetState p ()
performContinue = whenJustM (use confirmSendWidgetResultVarL) $ \resultVar ->
    whenM (use confirmSendWidgetCheckL) $ putMVar resultVar True *> closeDialog

performCancel :: WidgetEventM ConfirmSendWidgetState p ()
performCancel = whenJustM (use confirmSendWidgetResultVarL) $ \resultVar ->
    putMVar resultVar False *> closeDialog

closeDialog :: WidgetEventM ConfirmSendWidgetState p ()
closeDialog = do
    UiFace{..} <- use confirmSendWidgetUiFaceL
    liftIO $ putUiEvent $ UiConfirmEvent UiConfirmDone
    confirmSendWidgetResultVarL .= Nothing
    confirmSendWidgetOutputListL .= []
    confirmSendWidgetCheckL .= False

handleConfirmSendWidgetEvent
    :: UiEvent
    -> WidgetEventM ConfirmSendWidgetState p ()
handleConfirmSendWidgetEvent = \case
    UiConfirmEvent (UiConfirmRequest resVar (UiConfirmSend outLst)) -> do
        confirmSendWidgetResultVarL .= Just resVar
        confirmSendWidgetOutputListL .= outLst
    _ -> pass

