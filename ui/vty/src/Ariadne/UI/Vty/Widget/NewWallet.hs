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

import Control.Lens (makeLensesWith, uses, (<%=), (%=))
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
    , newWalletRestoreName :: !Text
    , newWalletRestoreMnemonic :: !Text
    , newWalletRestorePass :: !Text

    , newWalletFocusRing :: !(B.FocusRing BrickName)
    , newWalletFieldNewName :: B.FormFieldState NewWalletWidgetState UiEvent BrickName
    , newWalletFieldNewPass :: B.FormFieldState NewWalletWidgetState UiEvent BrickName
    , newWalletFieldRestoreName :: B.FormFieldState NewWalletWidgetState UiEvent BrickName
    , newWalletFieldRestoreMnemonic :: B.FormFieldState NewWalletWidgetState UiEvent BrickName
    , newWalletFieldRestorePass :: B.FormFieldState NewWalletWidgetState UiEvent BrickName
    }

makeLensesWith postfixLFields ''NewWalletWidgetState

initNewWalletWidget :: NewWalletWidgetState
initNewWalletWidget =
  fix $ \this -> NewWalletWidgetState
    { newWalletNewName = ""
    , newWalletNewPass = ""
    , newWalletRestoreName = ""
    , newWalletRestoreMnemonic = ""
    , newWalletRestorePass = ""

    , newWalletFocusRing = B.focusRing
        [ BrickNone
        , BrickNewWalletName, BrickNewWalletPass, BrickNewWalletCreateButton
        , BrickNewWalletRestoreName, BrickNewWalletRestoreMnemonic
        , BrickNewWalletRestorePass, BrickNewWalletRestoreButton
        ]
    , newWalletFieldNewName = B.editTextField newWalletNewNameL BrickNewWalletName (Just 1) this
    , newWalletFieldNewPass = B.editPasswordField newWalletNewPassL BrickNewWalletPass this
    , newWalletFieldRestoreName = B.editTextField newWalletRestoreNameL BrickNewWalletRestoreName (Just 1) this
    , newWalletFieldRestoreMnemonic = B.editTextField newWalletRestoreMnemonicL BrickNewWalletRestoreMnemonic (Just 1) this
    , newWalletFieldRestorePass = B.editPasswordField newWalletRestorePassL BrickNewWalletRestorePass this
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
    , pad $ button "[ Create ]" BrickNewWalletCreateButton

    , pad $ B.txt "Restore wallet from a mnemonic"
    , renderField "       Name: " $ newWalletFieldRestoreName
    , renderField "   Mnemonic: " $ newWalletFieldRestoreMnemonic
    , renderField " Passphrase: " $ newWalletFieldRestorePass
    , button "[ Restore ]" BrickNewWalletRestoreButton
    ]
  where
    pad = B.padBottom (B.Pad 1)
    renderField label field =
      pad $
        B.txt label B.<+>
        B.renderFormFieldState newWalletFocusRing field
    withFocus name =
      if B.focusGetCurrent newWalletFocusRing == Just name
      then B.withAttr "selected"
      else identity
    button label name =
      pad . B.padLeft (B.Pad 13) . B.clickable name . withFocus name $ B.txt label

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

data NewWalletWidgetEvent
  = NewWalletMouseDownEvent BrickName B.Location
  | NewWalletKeyEvent KeyboardEvent V.Event

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
  NewWalletMouseDownEvent name _coords -> do
    newWalletFocusRingL %= B.focusSetCurrent name
    case name of
      BrickNewWalletCreateButton ->
        performCreateWallet langFace
      BrickNewWalletRestoreButton ->
        performRestoreWallet langFace
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
      _ ->
        return ()

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

performCreateWallet
  :: UiLangFace
  -> StateT NewWalletWidgetState (B.EventM BrickName) ()
performCreateWallet _langFace = return ()

performRestoreWallet
  :: UiLangFace
  -> StateT NewWalletWidgetState (B.EventM BrickName) ()
performRestoreWallet _langFace = return ()
