module Ariadne.UI.Vty.Widget.Dialog.ConfirmMnemonic
    ( initConfirmMnemonicWidget
    ) where

import Control.Lens (makeLensesWith, (.=))
import qualified Data.Text as T

import qualified Brick as B

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Dialog.Utils
import Ariadne.UI.Vty.Widget.Form.Checkbox
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.UIConfig
import Ariadne.Util

data ConfirmMnemonicWidgetState = ConfirmMnemonicWidgetState
    { confirmMnemonicWidgetUiFace            :: !UiFace
    , confirmMnemonicWidgetContent           :: !Text
    , confirmMnemonicWidgetAppMovedMessage   :: !Text
    , confirmMnemonicWidgetOnDeviceMessage   :: !Text
    , confirmMnemonicWidgetNoLooksMessage    :: !Text
    , confirmMnemonicWidgetValue             :: ![Text]
    , confirmMnemonicWidgetConfirmationState :: !ConfirmationState
    , confirmMnemonicWidgetResultVar         :: !(Maybe (MVar Bool))
    , confirmMnemonicWidgetDialog            :: !DialogState
    , confirmRemoveWidgetCheckScreen         :: !Bool
    , confirmRemoveWidgetCheckOnDevice       :: !Bool
    , confirmRemoveWidgetCheckMoved          :: !Bool
    }

data ConfirmationState
  = Before
  | DisplayConfirmMnemonic
  | RetypeConfirmMnemonic

makeLensesWith postfixLFields ''ConfirmMnemonicWidgetState

initConfirmMnemonicWidget :: UiFace -> Widget p
initConfirmMnemonicWidget uiFace = initWidget $ do
    setWidgetDrawWithFocus drawConfirmMnemonicWidget
    setWidgetHandleKey handleConfirmMnemonicWidgetKey
    setWidgetHandleEvent handleConfirmMnemonicWidgetEvent
    setWidgetState ConfirmMnemonicWidgetState
        { confirmMnemonicWidgetUiFace            = uiFace
        , confirmMnemonicWidgetContent           = ""
        , confirmMnemonicWidgetAppMovedMessage   = mnemonicAppMovedMessage
        , confirmMnemonicWidgetOnDeviceMessage   = mnemonicOnDeviceMessage
        , confirmMnemonicWidgetNoLooksMessage    = mnemonicNoLooksMessage
        , confirmMnemonicWidgetValue             = []
        , confirmMnemonicWidgetConfirmationState = Before
        , confirmMnemonicWidgetResultVar         = Nothing
        , confirmRemoveWidgetCheckScreen         = False
        , confirmRemoveWidgetCheckOnDevice       = False
        , confirmRemoveWidgetCheckMoved          = False
        , confirmMnemonicWidgetDialog            = newDialogState mnemonicHeaderMessage
        }

    addWidgetChild WidgetNameConfirmMnemonicInput $
        initEditWidget (widgetParentLens confirmMnemonicWidgetContentL)
    addWidgetChild WidgetNameConfirmMnemonicCheckScreen
        $ initCheckboxWidget Check (widgetParentLens confirmMnemonicWidgetAppMovedMessageL)
        $ widgetParentLens confirmRemoveWidgetCheckScreenL
    addWidgetChild WidgetNameConfirmMnemonicCheckOnDevice
        $ initCheckboxWidget Check (widgetParentLens confirmMnemonicWidgetOnDeviceMessageL)
        $ widgetParentLens confirmRemoveWidgetCheckOnDeviceL
    addWidgetChild WidgetNameConfirmMnemonicCheckMoved
        $ initCheckboxWidget Check (widgetParentLens confirmMnemonicWidgetNoLooksMessageL)
        $ widgetParentLens confirmRemoveWidgetCheckMovedL

    addDialogButton confirmMnemonicWidgetDialogL
        WidgetNameConfirmMnemonicContinue "Continue" performContinue
    addDialogButton confirmMnemonicWidgetDialogL
        WidgetNameConfirmMnemonicCancel "Cancel" performCancel

    addWidgetEventHandler WidgetNameConfirmMnemonicInput $ \case
        WidgetEventEditChanged -> updateFocusList
        _ -> pass

    setWidgetFocusList
        [ WidgetNameConfirmMnemonicCheckScreen
        , WidgetNameConfirmMnemonicCancel
        , WidgetNameConfirmMnemonicContinue
        ]

drawConfirmMnemonicWidget
    :: WidgetName
    -> ConfirmMnemonicWidgetState
    -> WidgetDrawM ConfirmMnemonicWidgetState p WidgetDrawing
drawConfirmMnemonicWidget focus ConfirmMnemonicWidgetState{..} =
    case confirmMnemonicWidgetResultVar of
        Nothing -> return $ singleDrawing B.emptyWidget
        Just _  -> do
            widget <- ask
            let drawChild = last . drawWidgetChild focus widget
            drawInsideDialog confirmMnemonicWidgetDialog focus $
                case confirmMnemonicWidgetConfirmationState of
                    Before -> B.vBox
                        [ B.padTopBottom 1 . B.txtWrap $
                          mnemonicBeforeMkMessage mnemonicSize
                        , drawChild WidgetNameConfirmMnemonicCheckScreen
                        ]
                    DisplayConfirmMnemonic -> B.vBox
                        [ B.padTopBottom 1 . B.txtWrap $ mnemonicDisplayMessage
                        , B.withAttr "selected" $ B.txtWrap mnemonicText
                        ]
                    RetypeConfirmMnemonic -> B.vBox $
                        [ B.txtWrap mnemonicRetypeMessage
                        , B.vBox
                            [ B.padTopBottom 1 $ B.hBox
                                [ B.padLeftRight 1 $ B.txt $ mnemonicHeaderMessage
                                , B.padRight (B.Pad 1) $
                                  drawChild WidgetNameConfirmMnemonicInput
                                ]
                            ]
                        ] ++ if words confirmMnemonicWidgetContent == confirmMnemonicWidgetValue
                        then map drawChild
                            [ WidgetNameConfirmMnemonicCheckOnDevice
                            , WidgetNameConfirmMnemonicCheckMoved
                            ]
                        else []
  where
    mnemonicSize = length confirmMnemonicWidgetValue
    mnemonicText = T.intercalate " " confirmMnemonicWidgetValue

handleConfirmMnemonicWidgetKey
    :: KeyboardEvent
    -> WidgetEventM ConfirmMnemonicWidgetState p WidgetEventResult
handleConfirmMnemonicWidgetKey = \case
    KeyEnter -> performContinue $> WidgetEventHandled
    KeyNavigation -> performCancel $> WidgetEventHandled
    _ -> return WidgetEventNotHandled

performContinue :: WidgetEventM ConfirmMnemonicWidgetState p ()
performContinue = do
    ConfirmMnemonicWidgetState{..} <- get
    whenJust confirmMnemonicWidgetResultVar $ \resultVar ->
        case confirmMnemonicWidgetConfirmationState of
            Before -> when confirmRemoveWidgetCheckScreen $ do
                confirmMnemonicWidgetConfirmationStateL .= DisplayConfirmMnemonic
                updateFocusList
            DisplayConfirmMnemonic -> do
                confirmMnemonicWidgetConfirmationStateL .= RetypeConfirmMnemonic
                updateFocusList
            RetypeConfirmMnemonic -> when
                ( words confirmMnemonicWidgetContent == confirmMnemonicWidgetValue
                && confirmRemoveWidgetCheckOnDevice
                && confirmRemoveWidgetCheckMoved
                ) $ putMVar resultVar True *> closeDialog

performCancel :: WidgetEventM ConfirmMnemonicWidgetState p ()
performCancel = whenJustM (use confirmMnemonicWidgetResultVarL) $ \resultVar ->
    putMVar resultVar False *> closeDialog

closeDialog :: WidgetEventM ConfirmMnemonicWidgetState p ()
closeDialog = do
    UiFace{..} <- use confirmMnemonicWidgetUiFaceL
    liftIO $ putUiEvent $ UiConfirmEvent UiConfirmDone
    confirmMnemonicWidgetContentL .= ""
    confirmMnemonicWidgetValueL .= []
    confirmMnemonicWidgetConfirmationStateL .= Before
    confirmMnemonicWidgetResultVarL .= Nothing
    confirmRemoveWidgetCheckScreenL .= False
    confirmRemoveWidgetCheckOnDeviceL .= False
    confirmRemoveWidgetCheckMovedL .= False
    updateFocusList

updateFocusList :: WidgetEventM ConfirmMnemonicWidgetState p ()
updateFocusList = do
    confirmationState <- use confirmMnemonicWidgetConfirmationStateL
    lift . setWidgetFocusList =<< case confirmationState of
        Before -> return $ WidgetNameConfirmMnemonicCheckScreen : dialogButtons
        DisplayConfirmMnemonic -> return dialogButtons
        RetypeConfirmMnemonic -> do
            confirmContent <- use confirmMnemonicWidgetContentL
            confirmValue <- use confirmMnemonicWidgetValueL
            return $ if words confirmContent == confirmValue
                then
                    [ WidgetNameConfirmMnemonicInput
                    , WidgetNameConfirmMnemonicCheckOnDevice
                    , WidgetNameConfirmMnemonicCheckMoved
                    ] <> dialogButtons
                else WidgetNameConfirmMnemonicInput : dialogButtons
  where
    dialogButtons =
        [ WidgetNameConfirmMnemonicCancel
        , WidgetNameConfirmMnemonicContinue
        ]

handleConfirmMnemonicWidgetEvent
    :: UiEvent
    -> WidgetEventM ConfirmMnemonicWidgetState p ()
handleConfirmMnemonicWidgetEvent = \case
    UiConfirmEvent (UiConfirmRequest resVar (UiConfirmMnemonic mnemonic)) -> do
        confirmMnemonicWidgetResultVarL .= Just resVar
        confirmMnemonicWidgetValueL .= mnemonic
        updateFocusList
    _ -> pass
