module Ariadne.UI.Vty.Widget.NewWallet
       ( NewWalletWidgetState
       , initNewWalletWidget
       , drawNewWalletWidget

       , NewWalletWidgetEvent(..)
       , handleNewWalletFocus
       , handleNewWalletFocusIn
       , handleNewWalletWidgetEvent
       ) where

import Universum

import Control.Lens (makeLensesWith, uses, (<%=), (%=), (.=))
import Data.Function (fix)
import IiExtras

import qualified Brick as B
import qualified Brick.Focus as B
import qualified Brick.Forms as B
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Keyboard
import Ariadne.UI.Vty.UI

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

data NewWalletWidgetState =
  NewWalletWidgetState
    { newWalletNewName :: !Text
    , newWalletNewPass :: !Text
    , newWalletNewResult :: !NewResult
    , newWalletRestoreName :: !Text
    , newWalletRestoreMnemonic :: !Text
    , newWalletRestorePass :: !Text
    , newWalletRestoreFull :: !Bool
    , newWalletRestoreResult :: !RestoreResult

    , newWalletFocusRing :: !(B.FocusRing BrickName)
    , newWalletFieldNewName :: B.FormFieldState NewWalletWidgetState UiEvent BrickName
    , newWalletFieldNewPass :: B.FormFieldState NewWalletWidgetState UiEvent BrickName
    , newWalletFieldRestoreName :: B.FormFieldState NewWalletWidgetState UiEvent BrickName
    , newWalletFieldRestoreMnemonic :: B.FormFieldState NewWalletWidgetState UiEvent BrickName
    , newWalletFieldRestorePass :: B.FormFieldState NewWalletWidgetState UiEvent BrickName
    , newWalletFieldRestoreFull :: B.FormFieldState NewWalletWidgetState UiEvent BrickName
    }

data NewResult
  = NewResultNone
  | NewResultWaiting !UiCommandId
  | NewResultError !Text
  | NewResultSuccess ![Text]  -- ^ Mnemonic

data RestoreResult
  = RestoreResultNone
  | RestoreResultWaiting !UiCommandId
  | RestoreResultError !Text
  | RestoreResultSuccess

makeLensesWith postfixLFields ''NewWalletWidgetState

initNewWalletWidget :: NewWalletWidgetState
initNewWalletWidget =
  fix $ \this -> NewWalletWidgetState
    { newWalletNewName = ""
    , newWalletNewPass = ""
    , newWalletNewResult = NewResultNone
    , newWalletRestoreName = ""
    , newWalletRestoreMnemonic = ""
    , newWalletRestorePass = ""
    , newWalletRestoreFull = True
    , newWalletRestoreResult = RestoreResultNone

    , newWalletFocusRing = B.focusRing
        [ BrickNone
        , BrickNewWalletName, BrickNewWalletPass
        , BrickNewWalletCreateButton
        , BrickNewWalletRestoreName, BrickNewWalletRestoreMnemonic
        , BrickNewWalletRestorePass, BrickNewWalletRestoreFull
        , BrickNewWalletRestoreButton
        ]
    , newWalletFieldNewName = B.editTextField newWalletNewNameL BrickNewWalletName (Just 1) this
    , newWalletFieldNewPass = B.editPasswordField newWalletNewPassL BrickNewWalletPass this
    , newWalletFieldRestoreName = B.editTextField newWalletRestoreNameL BrickNewWalletRestoreName (Just 1) this
    , newWalletFieldRestoreMnemonic = B.editTextField newWalletRestoreMnemonicL BrickNewWalletRestoreMnemonic (Just 1) this
    , newWalletFieldRestorePass = B.editPasswordField newWalletRestorePassL BrickNewWalletRestorePass this
    , newWalletFieldRestoreFull = B.checkboxField newWalletRestoreFullL BrickNewWalletRestoreFull "Full restoration (find all used addresses)" this
    }

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

drawNewWalletWidget :: Bool -> NewWalletWidgetState -> B.Widget BrickName
drawNewWalletWidget _hasFocus NewWalletWidgetState{..} =
  B.vBox
    [ pad $ B.txt "Create new wallet"
    , renderField "       Name: " $ newWalletFieldNewName
    , renderField " Passphrase: " $ newWalletFieldNewPass
    , button "[ Create ]" BrickNewWalletCreateButton
    , pad . pad $ drawNewResult

    , pad $ B.txt "Restore wallet from a mnemonic"
    , renderField "       Name: " $ newWalletFieldRestoreName
    , renderField "   Mnemonic: " $ newWalletFieldRestoreMnemonic
    , renderField " Passphrase: " $ newWalletFieldRestorePass
    , pad $ padLeft $ withFocus BrickNewWalletRestoreFull $ B.renderFormFieldState newWalletFocusRing newWalletFieldRestoreFull
    , button "[ Restore ]" BrickNewWalletRestoreButton
    , drawRestoreResult
    ]
  where
    pad = B.padBottom (B.Pad 1)
    padLeft = B.padLeft (B.Pad 13)
    renderField label field =
      pad $
        B.txt label B.<+>
        B.renderFormFieldState newWalletFocusRing field
    withFocus name =
      if B.focusGetCurrent newWalletFocusRing == Just name
      then B.withAttr "selected"
      else identity
    button label name =
      pad . padLeft . B.clickable name . withFocus name $ B.txt label
    drawNewResult = padLeft $ case newWalletNewResult of
      NewResultNone -> B.emptyWidget
      NewResultWaiting _ -> B.txt "Creating..."
      NewResultError err -> B.txt $ "Couldn't create a wallet: " <> err
      NewResultSuccess mnemonic -> B.txt $ "Wallet created, here's your mnemonic:\n\n" <> unwords mnemonic
    drawRestoreResult = padLeft $ case newWalletRestoreResult of
      RestoreResultNone -> B.emptyWidget
      RestoreResultWaiting _ -> B.txt "Restoring..."
      RestoreResultError err -> B.txt $ "Couldn't restore a wallet: " <> err
      RestoreResultSuccess -> B.txt "Wallet successfully restored"

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

data NewWalletWidgetEvent
  = NewWalletMouseDownEvent BrickName B.Location
  | NewWalletKeyEvent KeyboardEvent V.Event
  | NewWalletNewWalletCommandResult UiCommandId UiNewWalletCommandResult
  | NewWalletRestoreWalletCommandResult UiCommandId UiRestoreWalletCommandResult

handleNewWalletFocus
  :: Bool
  -> StateT NewWalletWidgetState (B.EventM BrickName) Bool
handleNewWalletFocus back = do
  newFocus <- newWalletFocusRingL <%= if back then B.focusPrev else B.focusNext
  return $ B.focusGetCurrent newFocus /= Just BrickNone

handleNewWalletFocusIn
  :: Bool
  -> StateT NewWalletWidgetState (B.EventM BrickName) ()
handleNewWalletFocusIn back = do
  newWalletFocusRingL %= (if back then B.focusPrev else B.focusNext) . B.focusSetCurrent BrickNone

handleNewWalletWidgetEvent
  :: UiLangFace
  -> NewWalletWidgetEvent
  -> StateT NewWalletWidgetState (B.EventM BrickName) ()
handleNewWalletWidgetEvent langFace = \case
  NewWalletMouseDownEvent name coords -> do
    newWalletFocusRingL %= B.focusSetCurrent name
    case name of
      BrickNewWalletCreateButton ->
        performCreateWallet langFace
      BrickNewWalletRestoreButton ->
        performRestoreWallet langFace
      BrickNewWalletRestoreFull -> do
        field <- use newWalletFieldRestoreFullL
        get >>= lift . handleFormFieldEvent BrickNewWalletRestoreFull (B.MouseDown name V.BLeft [] coords) newWalletFieldRestoreFullL field >>= put
      _ ->
        return ()
  NewWalletKeyEvent key vtyEv -> do
    name <- uses newWalletFocusRingL $ fromMaybe BrickNone . B.focusGetCurrent
    case name of
      BrickNewWalletCreateButton
        | key `elem` [KeyEnter, KeyChar ' '] ->
            performCreateWallet langFace
        | otherwise ->
            return ()
      BrickNewWalletRestoreButton
        | key `elem` [KeyEnter, KeyChar ' '] ->
            performRestoreWallet langFace
        | otherwise ->
            return ()
      BrickNewWalletName -> do
        field <- use newWalletFieldNewNameL
        get >>= lift . handleFormFieldEvent BrickNewWalletName (B.VtyEvent vtyEv) newWalletFieldNewNameL field >>= put
      BrickNewWalletPass -> do
        field <- use newWalletFieldNewPassL
        get >>= lift . handleFormFieldEvent BrickNewWalletPass (B.VtyEvent vtyEv) newWalletFieldNewPassL field >>= put
      BrickNewWalletRestoreName -> do
        field <- use newWalletFieldRestoreNameL
        get >>= lift . handleFormFieldEvent BrickNewWalletRestoreName (B.VtyEvent vtyEv) newWalletFieldRestoreNameL field >>= put
      BrickNewWalletRestoreMnemonic -> do
        field <- use newWalletFieldRestoreMnemonicL
        get >>= lift . handleFormFieldEvent BrickNewWalletRestoreMnemonic (B.VtyEvent vtyEv) newWalletFieldRestoreMnemonicL field >>= put
      BrickNewWalletRestorePass -> do
        field <- use newWalletFieldRestorePassL
        get >>= lift . handleFormFieldEvent BrickNewWalletRestorePass (B.VtyEvent vtyEv) newWalletFieldRestorePassL field >>= put
      BrickNewWalletRestoreFull -> do
        field <- use newWalletFieldRestoreFullL
        get >>= lift . handleFormFieldEvent BrickNewWalletRestoreFull (B.VtyEvent vtyEv) newWalletFieldRestoreFullL field >>= put
      _ ->
        return ()
  NewWalletNewWalletCommandResult commandId result -> do
    newWalletNewResultL %= \case
      NewResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiNewWalletCommandSuccess mnemonic -> NewResultSuccess mnemonic
          UiNewWalletCommandFailure err -> NewResultError err
      other -> other
  NewWalletRestoreWalletCommandResult commandId result -> do
    newWalletRestoreResultL %= \case
      RestoreResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiRestoreWalletCommandSuccess -> RestoreResultSuccess
          UiRestoreWalletCommandFailure err -> RestoreResultError err
      other -> other

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

performCreateWallet
  :: UiLangFace
  -> StateT NewWalletWidgetState (B.EventM BrickName) ()
performCreateWallet UiLangFace{..} = do
  name <- use newWalletNewNameL
  passphrase <- use newWalletNewPassL
  currentResult <- use newWalletNewResultL
  if
    | NewResultWaiting _ <- currentResult -> return ()
    | otherwise ->
        liftIO (langPutUiCommand $ UiNewWallet name passphrase) >>= \case
          Left err ->
            newWalletNewResultL .= NewResultError err
          Right commandId ->
            newWalletNewResultL .= NewResultWaiting commandId

performRestoreWallet
  :: UiLangFace
  -> StateT NewWalletWidgetState (B.EventM BrickName) ()
performRestoreWallet UiLangFace{..} = do
  name <- use newWalletRestoreNameL
  mnemonic <- use newWalletRestoreMnemonicL
  passphrase <- use newWalletRestorePassL
  currentResult <- use newWalletRestoreResultL
  if
    | RestoreResultWaiting _ <- currentResult -> return ()
    | otherwise ->
        liftIO (langPutUiCommand $ UiRestoreWallet name mnemonic passphrase True) >>= \case
          Left err ->
            newWalletRestoreResultL .= RestoreResultError err
          Right commandId ->
            newWalletRestoreResultL .= RestoreResultWaiting commandId
