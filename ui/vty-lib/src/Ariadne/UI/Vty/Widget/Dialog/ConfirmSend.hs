module Ariadne.UI.Vty.Widget.Dialog.ConfirmSend
    ( initConfirmSendWidget
    ) where

import Control.Lens (makeLensesWith, (.=))

import qualified Brick as B
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Text.Wrap (WrapSettings (..))

import Ariadne.UIConfig
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Account (cutAddressHash)
import Ariadne.UI.Vty.Widget.Dialog.Utils
import Ariadne.UI.Vty.Widget.Form.Checkbox
import Ariadne.UI.Vty.Widget.Form.List
import Ariadne.Util

data ConfirmSendWidgetState = ConfirmSendWidgetState
    { confirmSendWidgetUiFace     :: !UiFace
    , confirmSendWidgetOutputList :: ![UiConfirmSendInfo]
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
    setWidgetScrollable
    setWidgetState ConfirmSendWidgetState
        { confirmSendWidgetUiFace     = uiFace
        , confirmSendWidgetOutputList = []
        , confirmSendWidgetResultVar  = Nothing
        , confirmSendWidgetCheck      = False
        , confirmSendWidgetDialog     = newDialogState sendHeaderMessage
        }

    addWidgetChild WidgetNameConfirmSendCheck
        $ initCheckboxWidget sendDefinitiveMessage
        $ widgetParentLens confirmSendWidgetCheckL
    addWidgetChild WidgetNameConfirmSendList $
        initListWidget (widgetParentGetter confirmSendWidgetOutputList) transactionLine

    addDialogButton confirmSendWidgetDialogL
        WidgetNameConfirmSendContinue "Send" performContinue
    addDialogButton confirmSendWidgetDialogL
        WidgetNameConfirmSendCancel "Cancel" performCancel

    setWidgetFocusList
        [ WidgetNameSelf
        , WidgetNameConfirmSendList
        , WidgetNameConfirmSendCheck
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
            drawInsideDialog confirmSendWidgetDialog focus $
                scrollingViewport widgetName B.Vertical $ B.vBox
                    [ B.padTopBottom 1 $ B.txtWrap $ sendListMessage
                    , B.padTopBottom 1 $ drawChild WidgetNameConfirmSendList
                    , drawChild WidgetNameConfirmSendCheck
                    ]

transactionLine :: Bool -> UiConfirmSendInfo -> B.Widget WidgetName
transactionLine focused UiConfirmSendInfo{..}
    | focused = B.withAttr "selected" $ wrapped
    | otherwise = B.Widget B.Greedy B.Fixed renderCut
  where
    fundsTo = csiAmount <> " " <> csiCoin <> " to: " 

    wrapSetting = WrapSettings {preserveIndentation = True, breakLongWords = True}
    wrapped = B.txtWrapWith wrapSetting $ fundsTo <> "\n  " <> csiAddress

    renderCut = do
        c <- B.getContext
        let addressWidth = (c ^. B.availWidthL) - T.length fundsTo
            addressLength = T.length csiAddress
            addressCut = cutAddressHash csiAddress addressWidth addressLength
            img = V.horizCat $ V.text' (c ^. B.attrL) <$> [fundsTo, addressCut]

        return $ B.emptyResult & B.imageL .~ img

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
    UiConfirmEvent (UiConfirmRequest resVar (UiConfirmSend sendInfo)) -> do
        confirmSendWidgetResultVarL .= Just resVar
        confirmSendWidgetOutputListL .= sendInfo
    _ -> pass

