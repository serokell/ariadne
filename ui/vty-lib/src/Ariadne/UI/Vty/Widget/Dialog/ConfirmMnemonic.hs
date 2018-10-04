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
import Ariadne.Util

data ConfirmMnemonicWidgetState = ConfirmMnemonicWidgetState
    { confirmMnemonicWidgetUiFace            :: !UiFace
    , confirmMnemonicWidgetContent           :: !Text
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
        , confirmMnemonicWidgetValue             = []
        , confirmMnemonicWidgetConfirmationState = Before
        , confirmMnemonicWidgetResultVar         = Nothing
        , confirmRemoveWidgetCheckScreen         = False
        , confirmRemoveWidgetCheckOnDevice       = False
        , confirmRemoveWidgetCheckMoved          = False
        , confirmMnemonicWidgetDialog            = newDialogState "Confirm Mnemonic"
        }

    addWidgetChild WidgetNameConfirmMnemonicInput $
        initEditWidget (widgetParentLens confirmMnemonicWidgetContentL)
    addWidgetChild WidgetNameConfirmMnemonicCheckScreen
        $ initCheckboxWidget "Make sure nobody looks into your screen unless \
                              \you want them to have access to your funds."
        $ widgetParentLens confirmRemoveWidgetCheckScreenL
    addWidgetChild WidgetNameConfirmMnemonicCheckOnDevice
        $ initCheckboxWidget "I understand that my money are held securely on \
                              \this device only, not on the company servers"
        $ widgetParentLens confirmRemoveWidgetCheckOnDeviceL
    addWidgetChild WidgetNameConfirmMnemonicCheckMoved
        $ initCheckboxWidget "I understand that if this application is moved \
                              \to another device or deleted, my money can be \
                              \only recovered with the backup phrase which was \
                              \written down in a secure place"
        $ widgetParentLens confirmRemoveWidgetCheckMovedL

    addDialogButton confirmMnemonicWidgetDialogL
        WidgetNameConfirmMnemonicContinue "Continue" performContinue
    addDialogButton confirmMnemonicWidgetDialogL
        WidgetNameConfirmMnemonicCancel "Cancel" performCancel

    setWidgetFocusList
        [ WidgetNameSelf
        , WidgetNameConfirmMnemonicInput
        , WidgetNameConfirmMnemonicCheckScreen
        , WidgetNameConfirmMnemonicCheckOnDevice
        , WidgetNameConfirmMnemonicCheckMoved
        , WidgetNameConfirmMnemonicContinue
        , WidgetNameConfirmMnemonicCancel
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
                        [ B.padTopBottom 1 $ B.txtWrap $
                          "On the following screen, you will see a set of " <> 
                          mnemonicSize <> " random words. This is your wallet \
                          \backup phrase. It can be entered in any version of \
                          \Ariadne application in order to restore your wallet’s\
                          \ funds and private key."
                        , drawChild WidgetNameConfirmMnemonicCheckScreen
                        ]
                    DisplayConfirmMnemonic -> B.vBox
                        [ B.padTopBottom 1 $ B.txtWrap $
                          "Please make sure you have carefully writen down your\
                          \ recovery phrase somewhere safe. You will need this \
                          \phrase later for next use and recover. \
                          \Phrase is case sensitive."
                        , B.withAttr "selected" $ B.txtWrap mnemonicText
                        ]
                    RetypeConfirmMnemonic -> B.vBox $
                        [ B.txtWrap $
                          "Type each word in the correct order to verify your\
                          \ recovery phrase."
                        , B.padLeftRight 1 $ B.vBox
                            [ B.padTopBottom 1 $ B.hBox
                                [ B.txt "RECOVERY PHRASE: "
                                , drawChild WidgetNameConfirmMnemonicInput
                                ]
                            ]
                        ] ++ if words confirmMnemonicWidgetContent == confirmMnemonicWidgetValue
                        then map drawChild 
                            [ WidgetNameConfirmMnemonicCheckOnDevice
                            , WidgetNameConfirmMnemonicCheckMoved
                            ]
                        else []
  where
    mnemonicSize = show $ length confirmMnemonicWidgetValue
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
    whenJust confirmMnemonicWidgetResultVar $ \resultVar -> do 
        case confirmMnemonicWidgetConfirmationState of
            Before -> when confirmRemoveWidgetCheckScreen $
                confirmMnemonicWidgetConfirmationStateL .= DisplayConfirmMnemonic
            DisplayConfirmMnemonic ->
                confirmMnemonicWidgetConfirmationStateL .= RetypeConfirmMnemonic
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

handleConfirmMnemonicWidgetEvent
    :: UiEvent
    -> WidgetEventM ConfirmMnemonicWidgetState p ()
handleConfirmMnemonicWidgetEvent = \case
    UiConfirmEvent (UiConfirmRequest resVar (UiConfirmMnemonic mnemonic)) -> do
        confirmMnemonicWidgetResultVarL .= Just resVar
        confirmMnemonicWidgetValueL .= mnemonic
    _ -> pass
