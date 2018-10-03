module Ariadne.UI.Vty.Widget.Dialog.Password
    ( initPasswordWidget
    ) where

import qualified Control.Concurrent.Event as CE
import Control.Lens (makeLensesWith, (.=))

import qualified Brick as B

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Dialog.Utils
import Ariadne.UI.Vty.Widget.Form.Edit hiding (initPasswordWidget)
import Ariadne.Util
import Ariadne.UX.PasswordManager

data PasswordWidgetState = PasswordWidgetState
    { passwordWidgetPutPassword :: !PutPassword
    , passwordWidgetUiFace      :: !UiFace
    , passwordWidgetContent     :: !Text
    , passwordWidgetRecipient   :: !(Maybe (WalletId, CE.Event))
    , passwordWidgetDialog      :: !DialogState
    }

makeLensesWith postfixLFields ''PasswordWidgetState

initPasswordWidget :: PutPassword -> UiFace -> Widget p
initPasswordWidget putPassword uiFace = initWidget $ do
    setWidgetDrawWithFocus drawPasswordWidget
    setWidgetHandleKey handlePasswordWidgetKey
    setWidgetHandleEvent handlePasswordWidgetEvent
    setWidgetState PasswordWidgetState
        { passwordWidgetPutPassword = putPassword
        , passwordWidgetUiFace      = uiFace
        , passwordWidgetContent     = ""
        , passwordWidgetRecipient   = Nothing
        , passwordWidgetDialog      = newDialogState "Insert Password"
        }

    addWidgetChild WidgetNamePasswordInput $
        initHiddenPasswordWidget (widgetParentLens passwordWidgetContentL)

    addDialogButton passwordWidgetDialogL
        WidgetNamePasswordContinue "Continue" performContinue

    setWidgetFocusList
        [ WidgetNameSelf
        , WidgetNamePasswordContinue
        , WidgetNamePasswordInput
        ]

drawPasswordWidget
    :: WidgetName
    -> PasswordWidgetState
    -> WidgetDrawM PasswordWidgetState p WidgetDrawing
drawPasswordWidget focus PasswordWidgetState{..} = do
    widget <- ask
    case passwordWidgetRecipient of
        Nothing -> return $ singleDrawing B.emptyWidget
        Just _  -> drawInsideDialog passwordWidgetDialog focus $
            B.padLeftRight 1 $
            B.hBox
                [ B.txt "Password: "
                , last $ drawWidgetChild focus widget WidgetNamePasswordInput
                ]

handlePasswordWidgetKey
    :: KeyboardEvent
    -> WidgetEventM PasswordWidgetState p WidgetEventResult
handlePasswordWidgetKey = \case
    KeyEnter -> performContinue $> WidgetEventHandled
    KeyNavigation -> return WidgetEventHandled
    _ -> return WidgetEventNotHandled

performContinue :: WidgetEventM PasswordWidgetState p ()
performContinue = do
    PasswordWidgetState{..} <- get
    whenJust passwordWidgetRecipient $ \(walletId, cEvent) -> do
        liftIO $ passwordWidgetPutPassword walletId passwordWidgetContent $ Just cEvent
        liftIO $ putUiEvent passwordWidgetUiFace $ UiPasswordEvent UiPasswordSent
        passwordWidgetRecipientL .= Nothing
        passwordWidgetContentL .= ""

handlePasswordWidgetEvent
    :: UiEvent
    -> WidgetEventM PasswordWidgetState p ()
handlePasswordWidgetEvent = \case
    UiPasswordEvent (UiPasswordRequest walletId cEvent) ->
        passwordWidgetRecipientL .= Just (walletId, cEvent)
    _ -> pass
