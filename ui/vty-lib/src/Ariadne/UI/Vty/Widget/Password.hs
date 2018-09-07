module Ariadne.UI.Vty.Widget.Password
    ( initPasswordWidget
    ) where

import Universum

import qualified Control.Concurrent.Event as CE
import Control.Lens (makeLensesWith, (.=))

import qualified Brick as B
import qualified Data.Text as T

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Form.Edit hiding (initPasswordWidget)
import Ariadne.Util
import Ariadne.UX.PasswordManager

data PasswordWidgetState =
  PasswordWidgetState
    { passwordWidgetPutPassword :: !PutPassword
    , passwordWidgetUiFace      :: !UiFace
    , passwordWidgetContent     :: !Text
    , passwordWidgetRecipient   :: !(Maybe (WalletId, CE.Event))
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
        }

    addWidgetChild WidgetNamePasswordInput $
        initHiddenPasswordWidget (widgetParentLens passwordWidgetContentL)

    setWidgetFocusList [WidgetNamePasswordInput]

drawPasswordWidget
    :: WidgetName
    -> PasswordWidgetState
    -> WidgetDrawM PasswordWidgetState p (B.Widget WidgetName)
drawPasswordWidget focus PasswordWidgetState{..} = do
    widget <- ask
    case passwordWidgetRecipient of
        Nothing -> return B.emptyWidget
        Just _  -> return $
            B.padLeftRight 1 $
            appendPrompt $
            drawWidgetChild focus widget WidgetNamePasswordInput
  where
    inputPrompt = "Password: "
    appendPrompt w = B.Widget (B.hSize w) (B.vSize w) $ do
        c <- B.getContext
        result <- B.render $ B.hLimit (c ^. B.availWidthL - T.length inputPrompt) w
        B.render $ B.hBox
            [ B.txt inputPrompt
            , B.Widget (B.hSize w) (B.vSize w) (return result)
            ]

handlePasswordWidgetKey
    :: KeyboardEvent
    -> WidgetEventM PasswordWidgetState p WidgetEventResult
handlePasswordWidgetKey = \case
    KeyEnter -> do
        PasswordWidgetState{..} <- get
        case passwordWidgetRecipient of
            Nothing -> return WidgetEventNotHandled
            Just (walletId, cEvent) -> do
              liftIO $ passwordWidgetPutPassword walletId passwordWidgetContent $ Just cEvent
              liftIO $ putUiEvent passwordWidgetUiFace $ UiPasswordEvent UiPasswordSent
              passwordWidgetRecipientL .= Nothing
              passwordWidgetContentL .= ""
              return WidgetEventHandled
    _ -> return WidgetEventNotHandled

handlePasswordWidgetEvent
    :: UiEvent
    -> WidgetEventM PasswordWidgetState p ()
handlePasswordWidgetEvent = \case
    UiPasswordEvent (UiPasswordRequest walletId cEvent) ->
        passwordWidgetRecipientL .= Just (walletId, cEvent)
    _ -> return ()
