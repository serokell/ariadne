module Ariadne.UI.Vty.Widget.Wallet
       ( initWalletWidget
       ) where

import Universum

import Control.Exception (handle)
import Control.Lens (assign, ix, makeLensesWith, (%=), (.=))
import IiExtras
import System.Hclip (ClipboardException, setClipboard)

import qualified Brick as B
import qualified Data.Text as T

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Form.Button
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.UI.Vty.Widget.Form.List
import Ariadne.UI.Vty.Widget.Form.Send

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

data WalletAccount =
  WalletAccount
    { walletAccountIdx :: !Word32
    , walletAccountName :: !Text
    , walletAccountBalance :: !(Maybe Text)
    , walletAccountSelected :: Bool
    }

data WalletWidgetState =
  WalletWidgetState
    { walletLangFace :: !UiLangFace
    , walletInfo :: !(Maybe UiWalletInfo)

    , walletName :: !Text
    , walletId :: !Text
    , walletRenameResult :: !RenameResult
    , walletBalance :: !BalanceResult
    , walletExportResult :: !ExportResult

    , walletAccounts :: ![WalletAccount]
    , walletNewAccountName :: !Text
    , walletNewAccountResult :: !NewAccountResult

    , walletExportEnabled :: !Bool
    , walletAccountsEnabled :: !Bool
    }

data RenameResult
  = RenameResultNone
  | RenameResultWaiting !UiCommandId
  | RenameResultError !Text
  | RenameResultSuccess

data BalanceResult
  = BalanceResultNone
  | BalanceResultWaiting !UiCommandId
  | BalanceResultError !Text
  | BalanceResultSuccess !Text  -- ^ Balance

data ExportResult
  = ExportResultNone
  | ExportResultWaiting !UiCommandId
  | ExportResultError !Text
  | ExportResultSuccess !Text  -- ^ Key
  | ExportResultCopied !Text  -- ^ Key

data NewAccountResult
  = NewAccountResultNone
  | NewAccountResultWaiting !UiCommandId
  | NewAccountResultError !Text
  | NewAccountResultSuccess

makeLensesWith postfixLFields ''WalletAccount
makeLensesWith postfixLFields ''WalletWidgetState

initWalletWidget :: UiLangFace -> UiFeatures -> Widget p
initWalletWidget langFace features =
  initWidget $ do
    setWidgetDrawWithFocus drawWalletWidget
    setWidgetScrollable
    setWidgetHandleEvent handleWalletWidgetEvent
    setWidgetState WalletWidgetState
      { walletLangFace = langFace
      , walletInfo = Nothing

      , walletName = ""
      , walletId = ""
      , walletRenameResult = RenameResultNone
      , walletBalance = BalanceResultNone
      , walletExportResult = ExportResultNone

      , walletAccounts = []
      , walletNewAccountName = ""
      , walletNewAccountResult = NewAccountResultNone

      , walletExportEnabled = featureExport features
      , walletAccountsEnabled = featureAccounts features
      }

    addWidgetChild WidgetNameWalletName $
      initEditWidget $ widgetParentLens walletNameL
    addWidgetChild WidgetNameWalletRenameButton $
      initButtonWidget "Rename"
    addWidgetEventHandler WidgetNameWalletRenameButton $ \case
      WidgetEventButtonPressed -> performRename
      _ -> return ()

    addWidgetChild WidgetNameWalletExportButton $
      initButtonWidget "Export"
    addWidgetEventHandler WidgetNameWalletExportButton $ \case
      WidgetEventButtonPressed -> performExport
      _ -> return ()

    addWidgetChild WidgetNameWalletAccountList $
      initListWidget (widgetParentGetter walletAccounts) drawAccountRow
    addWidgetEventHandler WidgetNameWalletAccountList $ \case
      WidgetEventListSelected idx -> toggleAccount idx
      _ -> return ()

    addWidgetChild WidgetNameWalletNewAccountName $
      initEditWidget $ widgetParentLens walletNewAccountNameL
    addWidgetChild WidgetNameWalletNewAccountButton $
      initButtonWidget "Create"
    addWidgetEventHandler WidgetNameWalletNewAccountButton $ \case
      WidgetEventButtonPressed -> performNewAccount
      _ -> return ()

    addWidgetChild WidgetNameWalletSend $
      initSendWidget langFace $
        Just $ widgetParentGetter
        (\WalletWidgetState{..} -> map walletAccountIdx $ filter walletAccountSelected $ walletAccounts)

    withWidgetState updateFocusList

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

drawAccountRow :: Bool -> WalletAccount -> B.Widget WidgetName
drawAccountRow focused WalletAccount{..} =
  (if focused then B.withAttr "selected" else id) $
  B.txt $
  (if walletAccountSelected then "[X] " else "[ ] ") <> walletAccountName

drawWalletWidget :: WidgetName -> WalletWidgetState -> WidgetDrawM WalletWidgetState p (B.Widget WidgetName)
drawWalletWidget focus WalletWidgetState{..} = do
  widget <- ask
  widgetName <- getWidgetName

  let
    padBottom = B.padBottom (B.Pad 1)
    padLeft = B.padLeft (B.Pad 1)
    fillLeft w = T.takeEnd w . (T.append $ T.replicate w " ")

    labelWidth = 16

    drawChild = drawWidgetChild focus widget
    label = B.padRight (B.Pad 1) . B.txt . fillLeft labelWidth

  return $
    B.viewport widgetName B.Vertical $
    B.padAll 1 $
    B.vBox $
    padBottom <$>
      [ label "Wallet name:"
          B.<+> drawChild WidgetNameWalletName
          B.<+> padLeft (drawChild WidgetNameWalletRenameButton)
      , label "Wallet id:"
          B.<+> B.txt walletId
      , case walletRenameResult of
          RenameResultNone -> B.emptyWidget
          RenameResultWaiting _ -> B.txt "Renaming..."
          RenameResultError err -> B.txt $ "Couldn't rename a wallet: " <> err
          RenameResultSuccess -> B.emptyWidget
      , label "Balance:" B.<+> case walletBalance of
          BalanceResultNone -> B.emptyWidget
          BalanceResultWaiting _ -> B.txt "requesting..."
          BalanceResultError err -> B.txt err
          BalanceResultSuccess balance -> B.txt balance
      ] ++
      (if not walletExportEnabled then [] else
        [ B.padLeft (B.Pad labelWidth) $ B.hBox
          [ padLeft $ drawChild WidgetNameWalletExportButton
          , padLeft $ case walletExportResult of
              ExportResultNone -> B.emptyWidget
              ExportResultWaiting _ -> B.txt "Exporting..."
              ExportResultError err -> B.txt err
              ExportResultSuccess key -> B.txt $ "Secret key: " <> key
              ExportResultCopied key -> B.txt $ "Copied to clipboard: " <> key
          ]
        ]
      ) ++
      (if not walletAccountsEnabled then [] else
        (if null walletAccounts then [] else
          [ label "Accounts:" B.<+> drawChild WidgetNameWalletAccountList
          ]
        ) ++
        [ label "New account:"
            B.<+> drawChild WidgetNameWalletNewAccountName
            B.<+> padLeft (drawChild WidgetNameWalletNewAccountButton)
        , padBottom $ case walletNewAccountResult of
            NewAccountResultNone -> B.emptyWidget
            NewAccountResultWaiting _ -> B.txt "Creating..."
            NewAccountResultError err -> B.txt $ "Couldn't create an account: " <> err
            NewAccountResultSuccess -> B.emptyWidget
        ]
      ) ++
      [ drawChild WidgetNameWalletSend
      ]

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

handleWalletWidgetEvent
  :: UiEvent
  -> WidgetEventM WalletWidgetState p ()
handleWalletWidgetEvent = \case
  UiWalletEvent UiWalletUpdate{..} -> do
    whenJust wuSelectionInfo $ \case
      UiSelectionWallet newInfo@UiWalletInfo{..} -> do
        UiLangFace{..} <- use walletLangFaceL

        curInfo <- use walletInfoL
        when (curInfo /= Just newInfo) $ do
          walletRenameResultL .= RenameResultNone
          walletExportResultL .= ExportResultNone
          walletNewAccountResultL .= NewAccountResultNone
        walletInfoL .= Just newInfo

        walletNameL .= fromMaybe "" uwiLabel
        walletIdL .= uwiId
        walletAccountsL .= map
          (\(idx, UiAccountInfo{..}) -> WalletAccount idx (fromMaybe "" uaciLabel) uaciBalance False)
          (zip [0..] uwiAccounts)
        case uwiBalance of
          Just balance -> walletBalanceL .= BalanceResultSuccess balance
          Nothing -> do
            use walletBalanceL >>= \case
              BalanceResultWaiting commandId
                | Just taskId <- cmdTaskId commandId ->
                    void . liftIO . langPutUiCommand $ UiKill taskId
              _ -> return ()
            liftIO (langPutUiCommand UiBalance) >>=
              assign walletBalanceL . either BalanceResultError BalanceResultWaiting
        updateFocusList
      _ -> return ()
  UiCommandResult commandId (UiRenameCommandResult result) -> do
    walletRenameResultL %= \case
      RenameResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiRenameCommandSuccess -> RenameResultSuccess
          UiRenameCommandFailure err -> RenameResultError err
      other -> other
  UiCommandResult commandId (UiBalanceCommandResult result) -> do
    walletBalanceL %= \case
      BalanceResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiBalanceCommandSuccess balance -> BalanceResultSuccess balance
          UiBalanceCommandFailure err -> BalanceResultError err
      other -> other
  UiCommandResult commandId (UiExportCommandResult result) -> do
    use walletExportResultL >>= \case
      ExportResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiExportCommandSuccess key -> do
            dispResult <- liftIO . handle (\(_ :: ClipboardException) -> return $ ExportResultSuccess key) $ do
              setClipboard . toString $ key
              return $ ExportResultCopied key
            walletExportResultL .= dispResult
          UiExportCommandFailure err -> do
            walletExportResultL .= ExportResultError err
      _ ->
        return ()
  UiCommandResult commandId (UiNewAccountCommandResult result) -> do
    use walletNewAccountResultL >>= \case
      NewAccountResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiNewAccountCommandSuccess -> do
            walletNewAccountResultL .= NewAccountResultSuccess
            walletNewAccountNameL .= ""
          UiNewAccountCommandFailure err -> do
            walletNewAccountResultL .= NewAccountResultError err
      _ ->
        return ()
  _ ->
    return ()

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

updateFocusList :: Monad m => StateT WalletWidgetState (StateT (WidgetInfo WalletWidgetState p) m) ()
updateFocusList = do
  accounts <- use walletAccountsL
  accountsEnabled <- use walletAccountsEnabledL
  exportEnabled <- use walletExportEnabledL
  lift $ setWidgetFocusList $
    [ WidgetNameWalletName
    , WidgetNameWalletRenameButton
    ] ++
    (if not exportEnabled then [] else [WidgetNameWalletExportButton]) ++
    (if not accountsEnabled then [] else
      (if null accounts then [] else [WidgetNameWalletAccountList]) ++
      [ WidgetNameWalletNewAccountName
      , WidgetNameWalletNewAccountButton
      ]
    ) ++
    [ WidgetNameWalletSend
    ]

performRename :: WidgetEventM WalletWidgetState p ()
performRename = do
  UiLangFace{..} <- use walletLangFaceL
  name <- use walletNameL
  use walletRenameResultL >>= \case
    RenameResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiRename $ UiRenameArgs name) >>=
      assign walletRenameResultL . either RenameResultError RenameResultWaiting

performExport :: WidgetEventM WalletWidgetState p ()
performExport = do
  UiLangFace{..} <- use walletLangFaceL
  use walletExportResultL >>= \case
    ExportResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiExport) >>=
      assign walletExportResultL . either ExportResultError ExportResultWaiting

toggleAccount :: Int -> WidgetEventM WalletWidgetState p ()
toggleAccount idx = do
  walletAccountsL . ix idx . walletAccountSelectedL %= not

performNewAccount :: WidgetEventM WalletWidgetState p ()
performNewAccount = do
  UiLangFace{..} <- use walletLangFaceL
  name <- use walletNewAccountNameL
  use walletNewAccountResultL >>= \case
    NewAccountResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiNewAccount $ UiNewAccountArgs name) >>=
      assign walletNewAccountResultL . either NewAccountResultError NewAccountResultWaiting
