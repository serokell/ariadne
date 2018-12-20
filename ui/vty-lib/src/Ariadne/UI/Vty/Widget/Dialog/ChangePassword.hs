module Ariadne.UI.Vty.Widget.Dialog.ChangePassword
    ( initChangePasswordWidget
    ) where

import qualified Control.Concurrent.Event as CE
import Control.Lens (makeLensesWith, (.=))

import qualified Brick as B
import qualified Data.Text as T

import Ariadne.UIConfig
import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Dialog.Utils
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.Util
import Ariadne.UX.PasswordManager

data ChangePasswordWidgetState = ChangePasswordWidgetState
  { changePasswordPutPassword :: !PutPassword
  , newPasswordWidgetContent :: !Text
  , confirmPasswordWidgetContent :: !Text
  , changePasswordError :: !ChangePasswordErrorMessage
  , changePasswordWidgetRecipient :: !(Maybe (WalletId, CE.Event))
  , changePasswordWidgetDialog :: !DialogState
  }

data ChangePasswordErrorMessage
  = ChangePasswordNoError
  | ChangePasswordPasswordsDontMatch

makeLensesWith postfixLFields ''ChangePasswordWidgetState

initChangePasswordWidget :: PutPassword -> Widget p
initChangePasswordWidget putPassword =
  initWidget $ do
    setWidgetDrawWithFocus drawChangePasswordWidget
    setWidgetHandleKey handleChangePasswordKey
    setWidgetHandleEvent handleChangePasswordEvent
    setWidgetState ChangePasswordWidgetState
      { changePasswordPutPassword = putPassword
      , newPasswordWidgetContent = ""
      , confirmPasswordWidgetContent = ""
      , changePasswordError = ChangePasswordNoError
      , changePasswordWidgetRecipient = Nothing
      , changePasswordWidgetDialog = newDialogState changePasswordHeaderMessage      }
    addWidgetChild WidgetNameChangePasswordNewPassword $
      initPasswordWidget (widgetParentLens newPasswordWidgetContentL)

    addWidgetChild WidgetNameChangePasswordConfirmPassword $
      initPasswordWidget (widgetParentLens confirmPasswordWidgetContentL)

    addDialogButton changePasswordWidgetDialogL
      WidgetNameChangePasswordContinue "Change password" performPasswordChange

    setWidgetFocusList
      [ WidgetNameChangePasswordNewPassword
      , WidgetNameChangePasswordConfirmPassword
      , WidgetNameChangePasswordContinue
      ]

drawChangePasswordWidget 
  :: WidgetName
  -> ChangePasswordWidgetState
  -> WidgetDrawM ChangePasswordWidgetState p WidgetDrawing
drawChangePasswordWidget focus ChangePasswordWidgetState{..} = do
  widget <- ask
  let 
    drawChild = last . drawWidgetChild focus widget
    labelWidth = 17
    fillLeft w = T.takeEnd w . (T.append $ T.replicate w " ")
    padBottom = B.padBottom (B.Pad 1)
    label = B.padRight (B.Pad 1) . B.txt . fillLeft labelWidth
    padLeft = B.padLeft (B.Pad 1)

  case changePasswordWidgetRecipient of
    Nothing -> return $ singleDrawing B.emptyWidget
    Just _ -> drawInsideDialog changePasswordWidgetDialog focus $ 
      B.vBox $
      padBottom <$>
        [ B.padLeftRight 1 $ B.txtWrap changePasswordInfoMessage
        , label "New password:" 
            B.<+> padLeft (drawChild WidgetNameChangePasswordNewPassword)
        , label "Confirm Password:"
            B.<+> padLeft (drawChild WidgetNameChangePasswordConfirmPassword)
        , case changePasswordError of
            ChangePasswordNoError -> B.emptyWidget
            ChangePasswordPasswordsDontMatch -> B.padLeftRight 1 $ B.txt "Passwords don't match"
        ]

handleChangePasswordKey
  :: KeyboardEvent
  -> WidgetEventM ChangePasswordWidgetState p WidgetEventResult
handleChangePasswordKey = \case
  KeyEnter -> performPasswordChange $> WidgetEventHandled
  _ -> return WidgetEventNotHandled

performPasswordChange :: WidgetEventM ChangePasswordWidgetState p ()
performPasswordChange = do
  ChangePasswordWidgetState{..} <- getWidgetState
  if newPasswordWidgetContent /= confirmPasswordWidgetContent
    then zoomWidgetState $ do
      changePasswordErrorL .= ChangePasswordPasswordsDontMatch
    else
      whenJust changePasswordWidgetRecipient $ \(walletId, cEvent) -> do
        liftIO $ changePasswordPutPassword walletId newPasswordWidgetContent $ Just cEvent
        widgetEvent WidgetEventModalExited
        zoomWidgetState $ do
          changePasswordWidgetRecipientL .= Nothing
          newPasswordWidgetContentL .= ""
          confirmPasswordWidgetContentL .= ""
          changePasswordErrorL .= ChangePasswordNoError

handleChangePasswordEvent :: UiEvent -> WidgetEventM ChangePasswordWidgetState p ()
handleChangePasswordEvent = \case
  UiPasswordEvent (UiPasswordRequest requestMode walletId cEvent) ->
    case requestMode of
      RequestCurrentPassword -> pass
      RequestNewPassword -> zoomWidgetState $
        changePasswordWidgetRecipientL .= Just (walletId, cEvent)
  _ -> pass