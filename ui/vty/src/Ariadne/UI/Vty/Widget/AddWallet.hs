module Ariadne.UI.Vty.Widget.AddWallet
       ( AddWalletWidgetState
       , initAddWalletWidget
       , drawAddWalletWidget

       , AddWalletWidgetEvent(..)
       , handleAddWalletFocus
       , handleAddWalletFocusIn
       , handleAddWalletWidgetEvent
       ) where

import Universum

import Control.Lens (assign, makeLensesWith, uses, (<%=), (%=))
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

data AddWalletWidgetState =
  AddWalletWidgetState
    { addWalletNewName :: !Text
    , addWalletNewPass :: !Text
    , addWalletNewResult :: !NewResult
    , addWalletRestoreName :: !Text
    , addWalletRestoreMnemonic :: !Text
    , addWalletRestorePass :: !Text
    , addWalletRestoreFull :: !Bool
    , addWalletRestoreResult :: !RestoreResult

    , addWalletFocusRing :: !(B.FocusRing BrickName)
    , addWalletFieldNewName :: B.FormFieldState AddWalletWidgetState UiEvent BrickName
    , addWalletFieldNewPass :: B.FormFieldState AddWalletWidgetState UiEvent BrickName
    , addWalletFieldRestoreName :: B.FormFieldState AddWalletWidgetState UiEvent BrickName
    , addWalletFieldRestoreMnemonic :: B.FormFieldState AddWalletWidgetState UiEvent BrickName
    , addWalletFieldRestorePass :: B.FormFieldState AddWalletWidgetState UiEvent BrickName
    , addWalletFieldRestoreFull :: B.FormFieldState AddWalletWidgetState UiEvent BrickName
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

makeLensesWith postfixLFields ''AddWalletWidgetState

initAddWalletWidget :: AddWalletWidgetState
initAddWalletWidget =
  fix $ \this -> AddWalletWidgetState
    { addWalletNewName = ""
    , addWalletNewPass = ""
    , addWalletNewResult = NewResultNone
    , addWalletRestoreName = ""
    , addWalletRestoreMnemonic = ""
    , addWalletRestorePass = ""
    , addWalletRestoreFull = True
    , addWalletRestoreResult = RestoreResultNone

    , addWalletFocusRing = B.focusRing
        [ BrickNone
        , BrickAddWalletName, BrickAddWalletPass
        , BrickAddWalletCreateButton
        , BrickAddWalletRestoreName, BrickAddWalletRestoreMnemonic
        , BrickAddWalletRestorePass, BrickAddWalletRestoreFull
        , BrickAddWalletRestoreButton
        ]
    , addWalletFieldNewName = B.editTextField addWalletNewNameL BrickAddWalletName (Just 1) this
    , addWalletFieldNewPass = B.editPasswordField addWalletNewPassL BrickAddWalletPass this
    , addWalletFieldRestoreName = B.editTextField addWalletRestoreNameL BrickAddWalletRestoreName (Just 1) this
    , addWalletFieldRestoreMnemonic = B.editTextField addWalletRestoreMnemonicL BrickAddWalletRestoreMnemonic (Just 1) this
    , addWalletFieldRestorePass = B.editPasswordField addWalletRestorePassL BrickAddWalletRestorePass this
    , addWalletFieldRestoreFull = B.checkboxField addWalletRestoreFullL BrickAddWalletRestoreFull "Full restoration (find all used addresses)" this
    }

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

drawAddWalletWidget :: Bool -> AddWalletWidgetState -> B.Widget BrickName
drawAddWalletWidget _hasFocus AddWalletWidgetState{..} =
  B.vBox
    [ visible BrickAddWalletName . pad $ B.txt "Create new wallet"
    , visible BrickAddWalletName $ renderField "       Name: " $ addWalletFieldNewName
    , visible BrickAddWalletPass $ renderField " Passphrase: " $ addWalletFieldNewPass
    , button "[ Create ]" BrickAddWalletCreateButton
    , visible BrickAddWalletCreateButton . pad . pad $ drawNewResult

    , pad $ B.txt "Restore wallet from a mnemonic"
    , visible BrickAddWalletRestoreName     $ renderField "       Name: " $ addWalletFieldRestoreName
    , visible BrickAddWalletRestoreMnemonic $ renderField "   Mnemonic: " $ addWalletFieldRestoreMnemonic
    , visible BrickAddWalletRestorePass     $ renderField " Passphrase: " $ addWalletFieldRestorePass
    , pad $ padLeft $ withFocus BrickAddWalletRestoreFull $ B.renderFormFieldState addWalletFocusRing addWalletFieldRestoreFull
    , button "[ Restore ]" BrickAddWalletRestoreButton
    , visible BrickAddWalletRestoreButton . pad $ drawRestoreResult
    ]
  where
    pad = B.padBottom (B.Pad 1)
    padLeft = B.padLeft (B.Pad 13)
    visible name =
      if B.focusGetCurrent addWalletFocusRing == Just name
      then B.visible
      else identity
    renderField label field =
      pad $
        B.txt label B.<+>
        B.renderFormFieldState addWalletFocusRing field
    withFocus name =
      if B.focusGetCurrent addWalletFocusRing == Just name
      then B.visible . B.withAttr "selected"
      else identity
    button label name =
      visible name . pad . padLeft . B.clickable name . withFocus name $ B.txt label
    drawNewResult = padLeft $ case addWalletNewResult of
      NewResultNone -> B.emptyWidget
      NewResultWaiting _ -> B.txt "Creating..."
      NewResultError err -> B.txt $ "Couldn't create a wallet: " <> err
      NewResultSuccess mnemonic -> B.txt $ "Wallet created, here's your mnemonic:\n\n" <> unwords mnemonic
    drawRestoreResult = padLeft $ case addWalletRestoreResult of
      RestoreResultNone -> B.emptyWidget
      RestoreResultWaiting _ -> B.txt "Restoring..."
      RestoreResultError err -> B.txt $ "Couldn't restore a wallet: " <> err
      RestoreResultSuccess -> B.txt "Wallet successfully restored"

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

data AddWalletWidgetEvent
  = AddWalletMouseDownEvent BrickName B.Location
  | AddWalletKeyEvent KeyboardEvent V.Event
  | AddWalletNewWalletCommandResult UiCommandId UiNewWalletCommandResult
  | AddWalletRestoreWalletCommandResult UiCommandId UiRestoreWalletCommandResult

handleAddWalletFocus
  :: Bool
  -> StateT AddWalletWidgetState (B.EventM BrickName) Bool
handleAddWalletFocus back = do
  newFocus <- addWalletFocusRingL <%= if back then B.focusPrev else B.focusNext
  return $ B.focusGetCurrent newFocus /= Just BrickNone

handleAddWalletFocusIn
  :: Bool
  -> StateT AddWalletWidgetState (B.EventM BrickName) ()
handleAddWalletFocusIn back = do
  addWalletFocusRingL %= (if back then B.focusPrev else B.focusNext) . B.focusSetCurrent BrickNone

handleAddWalletWidgetEvent
  :: UiLangFace
  -> AddWalletWidgetEvent
  -> StateT AddWalletWidgetState (B.EventM BrickName) ()
handleAddWalletWidgetEvent langFace = \case
  AddWalletMouseDownEvent name coords -> do
    addWalletFocusRingL %= B.focusSetCurrent name
    case name of
      BrickAddWalletCreateButton ->
        performCreateWallet langFace
      BrickAddWalletRestoreButton ->
        performRestoreWallet langFace
      BrickAddWalletRestoreFull -> do
        field <- use addWalletFieldRestoreFullL
        get >>= lift . handleFormFieldEvent BrickAddWalletRestoreFull (B.MouseDown name V.BLeft [] coords) addWalletFieldRestoreFullL field >>= put
      _ ->
        return ()
  AddWalletKeyEvent key vtyEv -> do
    name <- uses addWalletFocusRingL $ fromMaybe BrickNone . B.focusGetCurrent
    case name of
      BrickAddWalletCreateButton
        | key `elem` [KeyEnter, KeyChar ' '] ->
            performCreateWallet langFace
        | otherwise ->
            return ()
      BrickAddWalletRestoreButton
        | key `elem` [KeyEnter, KeyChar ' '] ->
            performRestoreWallet langFace
        | otherwise ->
            return ()
      BrickAddWalletName -> do
        field <- use addWalletFieldNewNameL
        get >>= lift . handleFormFieldEvent BrickAddWalletName (B.VtyEvent vtyEv) addWalletFieldNewNameL field >>= put
      BrickAddWalletPass -> do
        field <- use addWalletFieldNewPassL
        get >>= lift . handleFormFieldEvent BrickAddWalletPass (B.VtyEvent vtyEv) addWalletFieldNewPassL field >>= put
      BrickAddWalletRestoreName -> do
        field <- use addWalletFieldRestoreNameL
        get >>= lift . handleFormFieldEvent BrickAddWalletRestoreName (B.VtyEvent vtyEv) addWalletFieldRestoreNameL field >>= put
      BrickAddWalletRestoreMnemonic -> do
        field <- use addWalletFieldRestoreMnemonicL
        get >>= lift . handleFormFieldEvent BrickAddWalletRestoreMnemonic (B.VtyEvent vtyEv) addWalletFieldRestoreMnemonicL field >>= put
      BrickAddWalletRestorePass -> do
        field <- use addWalletFieldRestorePassL
        get >>= lift . handleFormFieldEvent BrickAddWalletRestorePass (B.VtyEvent vtyEv) addWalletFieldRestorePassL field >>= put
      BrickAddWalletRestoreFull -> do
        field <- use addWalletFieldRestoreFullL
        get >>= lift . handleFormFieldEvent BrickAddWalletRestoreFull (B.VtyEvent vtyEv) addWalletFieldRestoreFullL field >>= put
      _ ->
        return ()
  AddWalletNewWalletCommandResult commandId result -> do
    addWalletNewResultL %= \case
      NewResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiNewWalletCommandSuccess mnemonic -> NewResultSuccess mnemonic
          UiNewWalletCommandFailure err -> NewResultError err
      other -> other
  AddWalletRestoreWalletCommandResult commandId result -> do
    addWalletRestoreResultL %= \case
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
  -> StateT AddWalletWidgetState (B.EventM BrickName) ()
performCreateWallet UiLangFace{..} = do
  name <- use addWalletNewNameL
  passphrase <- use addWalletNewPassL
  use addWalletNewResultL >>= \case
    NewResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiNewWallet name passphrase) >>=
      assign addWalletNewResultL . either NewResultError NewResultWaiting

performRestoreWallet
  :: UiLangFace
  -> StateT AddWalletWidgetState (B.EventM BrickName) ()
performRestoreWallet UiLangFace{..} = do
  name <- use addWalletRestoreNameL
  mnemonic <- use addWalletRestoreMnemonicL
  passphrase <- use addWalletRestorePassL
  full <- use addWalletRestoreFullL
  use addWalletRestoreResultL >>= \case
    RestoreResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiRestoreWallet name mnemonic passphrase full) >>=
      assign addWalletRestoreResultL . either RestoreResultError RestoreResultWaiting
