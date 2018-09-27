module Ariadne.UI.Vty.Widget.Dialog.ConfirmMnemonic
    ( initConfirmMnemonicWidget
    ) where

import Control.Lens (makeLensesWith, (.=))
import qualified Data.Text as T

import qualified Brick as B
import qualified Brick.Widgets.Center as B

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Dialog.Utils
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.Util

data ConfirmMnemonicWidgetState = ConfirmMnemonicWidgetState
    { confirmMnemonicWidgetUiFace            :: !UiFace
    , confirmMnemonicWidgetContent           :: !Text
    , confirmMnemonicWidgetValue             :: ![Text]
    , confirmMnemonicWidgetConfirmationState :: !ConfirmationState
    , confirmMnemonicWidgetResultVar         :: !(Maybe (MVar Bool))
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
        }

    addWidgetChild WidgetNameConfirmMnemonicInput $
        initEditWidget (widgetParentLens confirmMnemonicWidgetContentL)

    addDialogButton WidgetNameConfirmMnemonicCancel "Cancel" performCancel
    addDialogButton WidgetNameConfirmMnemonicContinue "Continue" performContinue

    setWidgetFocusList
        [ WidgetNameSelf
        , WidgetNameConfirmMnemonicContinue
        , WidgetNameConfirmMnemonicCancel
        , WidgetNameConfirmMnemonicInput
        ]

drawConfirmMnemonicWidget
    :: WidgetName
    -> ConfirmMnemonicWidgetState
    -> WidgetDrawM ConfirmMnemonicWidgetState p WidgetDrawing
drawConfirmMnemonicWidget focus ConfirmMnemonicWidgetState{..} = do
    widget <- ask
    case confirmMnemonicWidgetResultVar of
        Nothing -> return $ singleDrawing B.emptyWidget
        Just _  -> drawInsideDialog "Confirm ConfirmMnemonic" focus
            [WidgetNameConfirmMnemonicCancel, WidgetNameConfirmMnemonicContinue] $
            case confirmMnemonicWidgetConfirmationState of
                Before -> B.vBox
                    [ B.hCenter $ B.txt "WARNING"
                    , B.padTopBottom 1 $ B.txtWrap "A set of random words will \
                      \appear on the next screen. This is your wallet backup \
                      \phrase. It can be entered in any version of Ariadne \
                      \application in order to restore your wallet’s funds \
                      \and private key."
                    ]
                DisplayConfirmMnemonic -> B.vBox $ 
                    [ B.txtWrap "Please make sure you have written down your \
                      \recovery phrase somewhere safe. You will need this \
                      \phrase later for next use and recover. Phrase is case \
                      \sensitive."
                    , B.padTopBottom 1 $ B.txtWrap $ T.intercalate " " confirmMnemonicWidgetValue
                    ]
                RetypeConfirmMnemonic ->
                    B.padLeftRight 1 $
                    B.vBox
                        [ B.txtWrap "Type each word in the correct order to verify your recovery phrase."
                        , B.padTopBottom 1 $ B.hBox
                            [ B.txt "ConfirmMnemonic: "
                            , last $ drawWidgetChild focus widget WidgetNameConfirmMnemonicInput
                            ]
                        ]

handleConfirmMnemonicWidgetKey
    :: KeyboardEvent
    -> WidgetEventM ConfirmMnemonicWidgetState p WidgetEventResult
handleConfirmMnemonicWidgetKey = \case
    KeyEnter -> performContinue *> return WidgetEventHandled
    KeyNavigation -> performCancel *> return WidgetEventHandled
    _ -> return WidgetEventNotHandled

performContinue :: WidgetEventM ConfirmMnemonicWidgetState p ()
performContinue = do
    ConfirmMnemonicWidgetState{..} <- get
    whenJust confirmMnemonicWidgetResultVar $ \resultVar -> do 
        case confirmMnemonicWidgetConfirmationState of
            Before -> do
                confirmMnemonicWidgetConfirmationStateL .= DisplayConfirmMnemonic
            DisplayConfirmMnemonic -> do
                confirmMnemonicWidgetConfirmationStateL .= RetypeConfirmMnemonic
            RetypeConfirmMnemonic -> do
                let mnemonic = confirmMnemonicWidgetValue
                when (words (confirmMnemonicWidgetContent) == mnemonic) $ do
                    liftIO $ putMVar resultVar True
                    UiFace{..} <- use confirmMnemonicWidgetUiFaceL
                    liftIO $ putUiEvent $ UiConfirmEvent UiConfirmDone
                    confirmMnemonicWidgetContentL .= ""
                    confirmMnemonicWidgetValueL .= []
                    confirmMnemonicWidgetConfirmationStateL .= Before
                    confirmMnemonicWidgetResultVarL .= Nothing

performCancel :: WidgetEventM ConfirmMnemonicWidgetState p ()
performCancel = do
    ConfirmMnemonicWidgetState{..} <- get
    whenJust confirmMnemonicWidgetResultVar $ \resultVar -> do
        liftIO $ putMVar resultVar False
        UiFace{..} <- use confirmMnemonicWidgetUiFaceL
        liftIO $ putUiEvent $ UiConfirmEvent UiConfirmDone
        confirmMnemonicWidgetContentL .= ""
        confirmMnemonicWidgetValueL .= []
        confirmMnemonicWidgetConfirmationStateL .= Before
        confirmMnemonicWidgetResultVarL .= Nothing

handleConfirmMnemonicWidgetEvent
    :: UiEvent
    -> WidgetEventM ConfirmMnemonicWidgetState p ()
handleConfirmMnemonicWidgetEvent = \case
    UiConfirmEvent (UiConfirmRequest resVar (UiConfirmMnemonic mnemonic)) -> do
        confirmMnemonicWidgetResultVarL .= Just resVar
        confirmMnemonicWidgetValueL .= mnemonic
    _ -> pass
