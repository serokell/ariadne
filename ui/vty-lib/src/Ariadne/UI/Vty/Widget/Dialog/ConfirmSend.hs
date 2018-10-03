module Ariadne.UI.Vty.Widget.Dialog.ConfirmSend
    ( initConfirmSendWidget
    ) where

import Control.Lens (makeLensesWith, (.=))

import qualified Brick as B
import qualified Brick.Widgets.Center as B

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Dialog.Utils
import Ariadne.Util

data ConfirmSendWidgetState = ConfirmSendWidgetState
    { confirmSendWidgetUiFace            :: !UiFace
    , confirmSendWidgetOutputList        :: ![(Text, Text, Text)]
    , confirmSendWidgetResultVar         :: !(Maybe (MVar Bool))
    }

makeLensesWith postfixLFields ''ConfirmSendWidgetState

initConfirmSendWidget :: UiFace -> Widget p
initConfirmSendWidget uiFace = initWidget $ do
    setWidgetDrawWithFocus drawConfirmSendWidget
    setWidgetHandleKey handleConfirmSendWidgetKey
    setWidgetHandleEvent handleConfirmSendWidgetEvent
    setWidgetState ConfirmSendWidgetState
        { confirmSendWidgetUiFace            = uiFace
        , confirmSendWidgetOutputList        = []
        , confirmSendWidgetResultVar         = Nothing
        }

    addDialogButton WidgetNameConfirmSendCancel "Cancel" performCancel
    addDialogButton WidgetNameConfirmSendContinue "Continue" performContinue

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
        Just _  -> drawInsideDialog "Confirm Send" focus [WidgetNameConfirmSendCancel, WidgetNameConfirmSendContinue] $ B.vBox $
            [ B.hCenter $ B.txt "WARNING"] ++
            map (\(address, amount, coin) ->
                    B.padTopBottom 1 $ B.txtWrap $ amount <> " " <> coin <> " to " <> address)
                confirmSendWidgetOutputList

handleConfirmSendWidgetKey
    :: KeyboardEvent
    -> WidgetEventM ConfirmSendWidgetState p WidgetEventResult
handleConfirmSendWidgetKey = \case
    KeyEnter -> performContinue $> WidgetEventHandled
    KeyNavigation -> performCancel $> WidgetEventHandled
    _ -> return WidgetEventNotHandled

performContinue :: WidgetEventM ConfirmSendWidgetState p ()
performContinue = do
    ConfirmSendWidgetState{..} <- get
    whenJust confirmSendWidgetResultVar $ \resultVar -> do
        putMVar resultVar True
        UiFace{..} <- use confirmSendWidgetUiFaceL
        liftIO $ putUiEvent $ UiConfirmEvent UiConfirmDone
        confirmSendWidgetResultVarL .= Nothing
        confirmSendWidgetOutputListL .= []

performCancel :: WidgetEventM ConfirmSendWidgetState p ()
performCancel = do
    ConfirmSendWidgetState{..} <- get
    whenJust confirmSendWidgetResultVar $ \resultVar -> do
        putMVar resultVar False
        UiFace{..} <- use confirmSendWidgetUiFaceL
        liftIO $ putUiEvent $ UiConfirmEvent UiConfirmDone
        confirmSendWidgetResultVarL .= Nothing
        confirmSendWidgetOutputListL .= []

handleConfirmSendWidgetEvent
    :: UiEvent
    -> WidgetEventM ConfirmSendWidgetState p ()
handleConfirmSendWidgetEvent = \case
    UiConfirmEvent (UiConfirmRequest resVar (UiConfirmSend outLst)) -> do
        confirmSendWidgetResultVarL .= Just resVar
        confirmSendWidgetOutputListL .= outLst
    _ -> pass

