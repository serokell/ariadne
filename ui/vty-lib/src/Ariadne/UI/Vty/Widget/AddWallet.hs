module Ariadne.UI.Vty.Widget.AddWallet
       ( initAddWalletWidget
       ) where

import Control.Lens (assign, makeLensesWith, (.=))

import qualified Brick as B
import qualified Data.Text as T

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Form.Button
import Ariadne.UI.Vty.Widget.Form.Checkbox
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.Util

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

data AddWalletWidgetState =
  AddWalletWidgetState
    { addWalletLangFace :: !UiLangFace

    , addWalletNewName :: !Text
    , addWalletNewPass :: !Text
    , addWalletNewResult :: !NewResult

    , addWalletRestoreName :: !Text
    , addWalletRestoreMnemonic :: !Text
    , addWalletRestorePass :: !Text
    , addWalletRestoreFull :: !Bool
    , addWalletRestoreResult :: !RestoreResult

    , addWalletRestoreFullEnabled :: !Bool
    , addWalletMnemonicName :: !Text
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

initAddWalletWidget :: UiLangFace -> UiFeatures -> Widget p
initAddWalletWidget langFace features =
  initWidget $ do
    setWidgetDrawWithFocus drawAddWalletWidget
    setWidgetScrollable
    setWidgetHandleEvent handleAddWalletWidgetEvent
    setWidgetState AddWalletWidgetState
      { addWalletLangFace = langFace

      , addWalletNewName = ""
      , addWalletNewPass = ""
      , addWalletNewResult = NewResultNone

      , addWalletRestoreName = ""
      , addWalletRestoreMnemonic = ""
      , addWalletRestorePass = ""
      , addWalletRestoreFull = True
      , addWalletRestoreResult = RestoreResultNone

      , addWalletRestoreFullEnabled = featureFullRestore features
      , addWalletMnemonicName = featureSecretKeyName features
      }

    addWidgetChild WidgetNameAddWalletNewName $
      initEditWidget $ widgetParentLens addWalletNewNameL
    addWidgetChild WidgetNameAddWalletNewPass $
      initPasswordWidget $ widgetParentLens addWalletNewPassL
    addWidgetChild WidgetNameAddWalletNewButton $
      initButtonWidget "Create"

    addWidgetEventHandler WidgetNameAddWalletNewButton $ \case
      WidgetEventButtonPressed -> performCreateWallet
      _ -> pass

    addWidgetChild WidgetNameAddWalletRestoreName $
      initEditWidget $ widgetParentLens addWalletRestoreNameL
    addWidgetChild WidgetNameAddWalletRestoreMnemonic $
      initEditWidget $ widgetParentLens addWalletRestoreMnemonicL
    addWidgetChild WidgetNameAddWalletRestorePass $
      initPasswordWidget $ widgetParentLens addWalletRestorePassL
    addWidgetChild WidgetNameAddWalletRestoreFull $
      initCheckboxWidget "Full restoration (find all used addresses)" $ widgetParentLens addWalletRestoreFullL
    addWidgetChild WidgetNameAddWalletRestoreButton $
      initButtonWidget "Restore"

    addWidgetEventHandler WidgetNameAddWalletRestoreButton $ \case
      WidgetEventButtonPressed -> performRestoreWallet
      _ -> pass

    setWidgetFocusList
      [ WidgetNameAddWalletNewName
      , WidgetNameAddWalletNewPass
      , WidgetNameAddWalletNewButton
      , WidgetNameAddWalletRestoreName
      , WidgetNameAddWalletRestoreMnemonic
      , WidgetNameAddWalletRestorePass
      , WidgetNameAddWalletRestoreFull
      , WidgetNameAddWalletRestoreButton
      ]

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

{-# ANN drawAddWalletWidget ("HLint: ignore Use list comprehension" :: Text) #-}
drawAddWalletWidget :: WidgetName -> AddWalletWidgetState -> WidgetDrawM AddWalletWidgetState p (B.Widget WidgetName)
drawAddWalletWidget focus AddWalletWidgetState{..} = do
  widget <- ask
  widgetName <- getWidgetName

  let
    drawChild = drawWidgetChild focus widget
    label = B.padRight (B.Pad 1) . B.txt . T.takeEnd 13 . (T.append $ T.replicate 13 " ")
    padBottom = B.padBottom (B.Pad 1)

  return $
    scrollingViewport widgetName B.Vertical $
    B.padAll 1 $
    B.vBox $
    padBottom <$>
      [ B.txt "Create new wallet"
      , label       "Name:" B.<+> drawChild WidgetNameAddWalletNewName
      , label "Passphrase:" B.<+> drawChild WidgetNameAddWalletNewPass
      , label            "" B.<+> drawChild WidgetNameAddWalletNewButton
      , case addWalletNewResult of
          NewResultNone -> B.emptyWidget
          NewResultWaiting _ -> B.txt "Creating..."
          NewResultError err -> B.txt $ "Couldn't create a wallet: " <> err
          NewResultSuccess mnemonic -> B.txt $ "Wallet created, here's your " <> T.toLower addWalletMnemonicName <> ":\n\n" <> unwords mnemonic

      , B.txt $ "Restore wallet from a " <> T.toLower addWalletMnemonicName
      , label       "Name:" B.<+> drawChild WidgetNameAddWalletRestoreName
      , label (addWalletMnemonicName <> ":") B.<+> drawChild WidgetNameAddWalletRestoreMnemonic
      , label "Passphrase:" B.<+> drawChild WidgetNameAddWalletRestorePass
      ] ++
      (if addWalletRestoreFullEnabled then [label "" B.<+> drawChild WidgetNameAddWalletRestoreFull] else []) ++
      [ label            "" B.<+> drawChild WidgetNameAddWalletRestoreButton
      , case addWalletRestoreResult of
          RestoreResultNone -> B.emptyWidget
          RestoreResultWaiting _ -> B.txt "Restoring..."
          RestoreResultError err -> B.txt $ "Couldn't restore a wallet: " <> err
          RestoreResultSuccess -> B.txt "Wallet successfully restored"
      ]

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

handleAddWalletWidgetEvent
  :: UiEvent
  -> WidgetEventM AddWalletWidgetState p ()
handleAddWalletWidgetEvent = \case
  UiWalletEvent UiWalletUpdate{..}
    | isJust wuSelectionInfo -> do
        addWalletNewResultL .= NewResultNone
        addWalletRestoreResultL .= RestoreResultNone
  UiCommandResult commandId (UiNewWalletCommandResult result) -> do
    use addWalletNewResultL >>= \case
      NewResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiNewWalletCommandSuccess mnemonic -> do
            addWalletNewNameL .= ""
            addWalletNewPassL .= ""
            addWalletNewResultL .= NewResultSuccess mnemonic
          UiNewWalletCommandFailure err -> do
            addWalletNewResultL .= NewResultError err
      _ ->
        pass
  UiCommandResult commandId (UiRestoreWalletCommandResult result) -> do
    use addWalletRestoreResultL >>= \case
      RestoreResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiRestoreWalletCommandSuccess -> do
            addWalletRestoreNameL .= ""
            addWalletRestoreMnemonicL .= ""
            addWalletRestorePassL .= ""
            addWalletRestoreResultL .= RestoreResultSuccess
          UiRestoreWalletCommandFailure err -> do
            addWalletRestoreResultL .= RestoreResultError err
      _ ->
        pass
  _ ->
    pass

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

performCreateWallet :: WidgetEventM AddWalletWidgetState p ()
performCreateWallet = do
  UiLangFace{..} <- use addWalletLangFaceL
  name <- use addWalletNewNameL
  passphrase <- use addWalletNewPassL
  use addWalletNewResultL >>= \case
    NewResultWaiting _ -> pass
    _ -> liftIO (langPutUiCommand $ UiNewWallet $ UiNewWalletArgs name passphrase) >>=
      assign addWalletNewResultL . either NewResultError NewResultWaiting

performRestoreWallet :: WidgetEventM AddWalletWidgetState p ()
performRestoreWallet = do
  UiLangFace{..} <- use addWalletLangFaceL
  name <- use addWalletRestoreNameL
  mnemonic <- use addWalletRestoreMnemonicL
  passphrase <- use addWalletRestorePassL
  full <- use addWalletRestoreFullL
  use addWalletRestoreResultL >>= \case
    RestoreResultWaiting _ -> pass
    _ -> liftIO (langPutUiCommand $ UiRestoreWallet $ UiRestoreWalletArgs name mnemonic passphrase full) >>=
      assign addWalletRestoreResultL . either RestoreResultError RestoreResultWaiting
