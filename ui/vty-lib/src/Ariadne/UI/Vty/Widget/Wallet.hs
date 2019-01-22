module Ariadne.UI.Vty.Widget.Wallet
       ( initWalletWidget
       ) where

import Control.Exception (handle)
import Control.Lens (assign, ix, makeLensesWith, uses, (%=), (.=))
import System.Hclip (ClipboardException, setClipboard)

import qualified Brick as B
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Scrolling
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Form.Button
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.UI.Vty.Widget.Form.List
import Ariadne.UI.Vty.Widget.Form.Send
import Ariadne.Util

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

data WalletAccount =
  WalletAccount
    { walletAccountIdx :: !Word32
    , walletAccountName :: !Text
    , walletAccountBalance :: !(UiCurrency Vty)
    , walletAccountSelected :: Bool
    }

instance Eq WalletAccount where
  WalletAccount a1 b1 c1 _ == WalletAccount a2 b2 c2 _ = a1 == a2 && b1 == b2 && c1 == c2

data WalletWidgetState =
  WalletWidgetState
    { walletLangFace :: !(UiLangFace Vty)
    , walletInfo :: !(Maybe (UiWalletInfo Vty))

    , walletName :: !Text
    , walletNameEdit :: !Text
    , walletId :: !Text
    , walletRenameResult :: !RenameResult
    , walletRemoveResult :: !RemoveResult
    , walletChangePasswordResult :: !ChangePasswordResult

    , walletBalance :: !BalanceResult
    , walletExportResult :: !ExportResult

    , walletAccounts :: ![WalletAccount]
    , walletAccountsEdit :: ![WalletAccount]
    , walletNewAccountName :: !Text
    , walletNewAccountResult :: !NewAccountResult

    , walletTxHistory :: !TxHistoryResult

    , walletExportEnabled :: !Bool
    , walletAccountsEnabled :: !Bool
    , walletTxHistoryEnabled :: !Bool
    }

data RenameResult
  = RenameResultNone
  | RenameResultWaiting !UiCommandId
  | RenameResultError !Text
  | RenameResultSuccess

data RemoveResult
  = RemoveResultNone
  | RemoveResultWaiting !UiCommandId
  | RemoveResultError !Text
  | RemoveResultSuccess

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

data TxHistoryResult
  = TxHistoryResultNone
  | TxHistoryResultWaiting !UiCommandId
  | TxHistoryResultError !Text
  | TxHistoryResultSuccess ![UiTxHistoryRow]

data ChangePasswordResult
  = ChangePasswordResultNone
  | ChangePasswordResultWaiting !UiCommandId
  | ChangePasswordResultError !Text
  | ChangePasswordResultSuccess

makeLensesWith postfixLFields ''WalletAccount
makeLensesWith postfixLFields ''WalletWidgetState

initWalletWidget :: UiLangFace Vty -> UiFeatures -> Widget p
initWalletWidget langFace UiFeatures{..} =
  initWidget $ do
    setWidgetDrawWithFocus drawWalletWidget
    setWidgetScrollable
    setWidgetHandleEvent handleWalletWidgetEvent
    setWidgetState WalletWidgetState
      { walletLangFace = langFace
      , walletInfo = Nothing

      , walletName = ""
      , walletNameEdit = ""
      , walletId = ""
      , walletRenameResult = RenameResultNone
      , walletRemoveResult = RemoveResultNone
      , walletChangePasswordResult = ChangePasswordResultNone
      , walletBalance = BalanceResultNone
      , walletExportResult = ExportResultNone

      , walletAccounts = []
      , walletAccountsEdit = []
      , walletNewAccountName = ""
      , walletNewAccountResult = NewAccountResultNone

      , walletTxHistory = TxHistoryResultNone

      , walletExportEnabled = featureExport
      , walletAccountsEnabled = featureAccounts
      , walletTxHistoryEnabled = featureTxHistory
      }

    addWidgetChild WidgetNameWalletName $
      initEditWidget $ widgetParentLens walletNameEditL
    addWidgetChild WidgetNameWalletRenameButton $
      initButtonWidget "Rename"
    addWidgetEventHandler WidgetNameWalletRenameButton $ \case
      WidgetEventButtonPressed -> zoomWidgetState performRename
      _ -> pass

    addWidgetChild WidgetNameWalletChangePasswordButton $
      initButtonWidget "Change Password"
    addWidgetEventHandler WidgetNameWalletChangePasswordButton $ \case
      WidgetEventButtonPressed -> zoomWidgetState performPasswordChange
      _ -> pass

    addWidgetChild WidgetNameWalletRemoveButton $
      initButtonWidget "Remove"
    addWidgetEventHandler WidgetNameWalletRemoveButton $ \case
      WidgetEventButtonPressed -> zoomWidgetState performRemove
      _ -> pass

    addWidgetChild WidgetNameWalletExportButton $
      initButtonWidget "Export"
    addWidgetEventHandler WidgetNameWalletExportButton $ \case
      WidgetEventButtonPressed -> zoomWidgetState performExport
      _ -> pass

    addWidgetChild WidgetNameWalletAccountList $
      initListWidget (widgetParentGetter walletAccountsEdit) drawAccountRow
    addWidgetEventHandler WidgetNameWalletAccountList $ \case
      WidgetEventListSelected idx -> zoomWidgetState $ toggleAccount idx
      _ -> pass

    addWidgetChild WidgetNameWalletNewAccountName $
      initEditWidget $ widgetParentLens walletNewAccountNameL
    addWidgetChild WidgetNameWalletNewAccountButton $
      initButtonWidget "Create"
    addWidgetEventHandler WidgetNameWalletNewAccountButton $ \case
      WidgetEventButtonPressed -> zoomWidgetState performNewAccount
      _ -> pass

    addWidgetChild WidgetNameWalletSend $
      initSendWidget langFace
        (Just $ widgetParentGetter $ map uwiWalletIdx . walletInfo)
        (Just $ widgetParentGetter $
          map walletAccountIdx . filter walletAccountSelected . walletAccountsEdit)

    updateFocusList

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

drawAccountRow :: Bool -> WalletAccount -> B.Widget WidgetName
drawAccountRow focused WalletAccount{..} =
  (if focused then B.withAttr "selected" else id) $
  B.txt $
  (if walletAccountSelected then "[X] " else "[ ] ") <> walletAccountName

drawTxRow :: UiTxHistoryRow -> B.Widget WidgetName
drawTxRow UiTxHistoryRow{..} = B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Fixed
    , B.render = render
    }
  where
    render = do
      rdrCtx <- B.getContext
      let
        attr = rdrCtx ^. B.attrL

        cut w txt = if w >= T.length txt
          then txt
          else
            T.take ((w - 3) `div` 2) txt <>
            "..." <>
            T.takeEnd (w - 3 - (w - 3) `div` 2) txt

        coinColumn = V.pad 2 0 0 0 . V.vertCat $ V.text' attr <$> uthrTotal : (uthrpAmount <$> uthrTo)
        width = rdrCtx ^. B.availWidthL - V.imageWidth coinColumn
        addrWidth = (width - 4) `div` 2
        txIdRow = V.text' attr . cut width $ uthrId
        txColumn = V.horizCat
          [ V.vertCat $ V.text' attr . cut addrWidth . uthrpAddress <$> uthrFrom
          , V.text' attr " -> "
          , V.vertCat $ V.text' attr . cut addrWidth . uthrpAddress <$> uthrTo
          ]

        img = V.pad 0 0 0 1 $ (txIdRow V.<-> txColumn) V.<|> coinColumn

      return $
        B.emptyResult
          & B.imageL .~ img

drawWalletWidget :: WidgetName -> WalletWidgetState -> WidgetDrawM WalletWidgetState p WidgetDrawing
drawWalletWidget focus WalletWidgetState{..} = do
  widget <- ask
  widgetName <- getWidgetName

  let
    padBottom = B.padBottom (B.Pad 1)
    padLeft = B.padLeft (B.Pad 1)
    fillLeft w = T.takeEnd w . (T.append $ T.replicate w " ")

    labelWidth = 17

    drawChild = last . drawWidgetChild focus widget
    label = B.padRight (B.Pad 1) . B.txt . fillLeft labelWidth

  return . singleDrawing $
    scrollingViewport widgetName B.Vertical $
    B.padAll 1 $
    B.vBox $
    padBottom <$>
      [ label "Wallet name:"
          B.<+> drawChild WidgetNameWalletName
          B.<+> padLeft (drawChild WidgetNameWalletRenameButton)
          B.<+> padLeft (drawChild WidgetNameWalletRemoveButton)
      , padLeft (drawChild WidgetNameWalletChangePasswordButton)
      , case walletChangePasswordResult of
          ChangePasswordResultNone -> B.emptyWidget
          ChangePasswordResultWaiting _ -> B.txt "Changing password..."
          ChangePasswordResultError err -> B.txtWrap $ "Couldn't change password: " <> err
          ChangePasswordResultSuccess -> B.txt "Password was sucessfully changed"
      , label "Wallet id:"
          B.<+> B.txt walletId
      , case walletRenameResult of
          RenameResultNone -> B.emptyWidget
          RenameResultWaiting _ -> B.txt "Renaming..."
          RenameResultError err -> B.txt $ "Couldn't rename a wallet: " <> err
          RenameResultSuccess -> B.emptyWidget
      , case walletRemoveResult of
          RemoveResultNone -> B.emptyWidget
          RemoveResultWaiting _ -> B.txt "Deleting..."
          RemoveResultError err -> B.txt $ "Couldn't delete a wallet: " <> err
          RemoveResultSuccess -> B.emptyWidget
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
        (if null walletAccountsEdit then [] else
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
      ] ++
      (if not walletTxHistoryEnabled then [] else
        [ B.txt "Transaction history"
        , padBottom $ case walletTxHistory of
            TxHistoryResultNone -> B.emptyWidget
            TxHistoryResultWaiting _ -> B.txt "Loading..."
            TxHistoryResultError err -> B.txt $ "Couldn't retrieve transaction history: " <> err
            TxHistoryResultSuccess rows -> B.vBox $ drawTxRow <$> rows
        ]
      )

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

handleWalletWidgetEvent
  :: UiEvent Vty
  -> WidgetEventM WalletWidgetState p ()
handleWalletWidgetEvent = \case
  UiWalletEvent UiWalletUpdate{..} -> do
    whenJust wuSelectionInfo $ \case
      UiSelectionWallet newInfo@UiWalletInfo{..} -> do
        zoomWidgetState $ do
          UiLangFace{..} <- use walletLangFaceL
          curInfo <- use walletInfoL
          when (curInfo /= Just newInfo) $ do
            walletRenameResultL .= RenameResultNone
            walletRemoveResultL .= RemoveResultNone
            walletExportResultL .= ExportResultNone
            walletNewAccountResultL .= NewAccountResultNone
          walletInfoL .= Just newInfo

          walletIdL .= uwiId
          whenJust uwiLabel $ updateEditable walletNameL walletNameEditL
          let converted =
                map (\(idx, UiAccountInfo{..}) ->
                    WalletAccount idx (fromMaybe "" uaciLabel) uaciBalance False)
                (zip [0..] uwiAccounts)
          updateEditable walletAccountsL walletAccountsEditL converted
          walletBalanceL .= BalanceResultSuccess (getUiCurrency uwiBalance)
          whenM (use walletTxHistoryEnabledL) $ do
            use walletTxHistoryL >>= \case
              TxHistoryResultWaiting commandId
                | Just taskId <- cmdTaskId commandId ->
                    void . liftIO . langPutUISilentCommand $ UiKill taskId
              _ -> pass
            liftIO (langPutUISilentCommand UiTxHistory) >>=
              assign walletTxHistoryL . either TxHistoryResultError TxHistoryResultWaiting

        updateFocusList
      _ -> pass
  UiCommandResult commandId (UiRenameCommandResult result) ->
    zoomWidgetState $ do
      walletRenameResultL %= \case
        RenameResultWaiting commandId' | commandId == commandId' ->
          case result of
            UiRenameCommandSuccess -> RenameResultSuccess
            UiRenameCommandFailure err -> RenameResultError err
        other -> other
  UiCommandResult commandId (UiRemoveCommandResult result) ->
    zoomWidgetState $ do
      walletRemoveResultL %= \case
        RemoveResultWaiting commandId' | commandId == commandId' ->
          case result of
            UiRemoveCommandSuccess -> RemoveResultSuccess
            UiRemoveCommandFailure err -> RemoveResultError err
        other -> other

  UiCommandResult commandId (UiBalanceCommandResult result) ->
    zoomWidgetState $ do
      walletBalanceL %= \case
        BalanceResultWaiting commandId' | commandId == commandId' ->
          case result of
            UiBalanceCommandSuccess balance -> BalanceResultSuccess balance
            UiBalanceCommandFailure err -> BalanceResultError err
        other -> other
  UiCommandResult commandId (UiTxHistoryCommandResult result) ->
    zoomWidgetState $ do
      walletTxHistoryL %= \case
        TxHistoryResultWaiting commandId' | commandId == commandId' ->
          case result of
            UiTxHistoryCommandSuccess history -> TxHistoryResultSuccess history
            UiTxHistoryCommandFailure err -> TxHistoryResultError err
        other -> other
  UiCommandResult commandId (UiExportCommandResult result) ->
    zoomWidgetState $ do
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
          pass
  UiCommandResult commandId (UiChangePasswordCommandResult result) ->
    zoomWidgetState $ do
      walletChangePasswordResultL %= \case
        ChangePasswordResultWaiting commandId' | commandId == commandId' ->
          case result of
            UiChangePasswordCommandSuccess -> ChangePasswordResultSuccess
            UiChangePasswordCommandFailure err -> ChangePasswordResultError err
        other -> other
  UiCommandResult commandId (UiNewAccountCommandResult result) ->
    zoomWidgetState $ do
      use walletNewAccountResultL >>= \case
        NewAccountResultWaiting commandId' | commandId == commandId' ->
          case result of
            UiNewAccountCommandSuccess -> do
              walletNewAccountResultL .= NewAccountResultSuccess
              walletNewAccountNameL .= ""
            UiNewAccountCommandFailure err -> do
              walletNewAccountResultL .= NewAccountResultError err
        _ ->
          pass
  _ ->
    pass

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

updateFocusList :: Monad m => StateT (WidgetInfo WalletWidgetState p) m ()
updateFocusList = do
  accounts <- use (widgetStateL . walletAccountsEditL)
  accountsEnabled <- use (widgetStateL . walletAccountsEnabledL)
  exportEnabled <- use (widgetStateL . walletExportEnabledL)
  setWidgetFocusList $
    [ WidgetNameWalletName
    , WidgetNameWalletRenameButton
    , WidgetNameWalletRemoveButton
    , WidgetNameWalletChangePasswordButton
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

performRename ::
  StateT WalletWidgetState (StateT p (B.EventM WidgetName)) ()
performRename = do
  UiLangFace{..} <- use walletLangFaceL
  name <- use walletNameEditL
  use walletRenameResultL >>= \case
    RenameResultWaiting _ -> pass
    _ -> liftIO (langPutUiCommand $ UiRename $ UiRenameArgs name) >>=
      assign walletRenameResultL . either RenameResultError RenameResultWaiting

performRemove ::
  StateT WalletWidgetState (StateT p (B.EventM WidgetName)) ()
performRemove = do
  UiLangFace{..} <- use walletLangFaceL
  use walletRemoveResultL >>= \case
    RemoveResultWaiting _ -> pass
    _ -> liftIO (langPutUiCommand $ UiRemove) >>=
      assign walletRemoveResultL . either RemoveResultError RemoveResultWaiting

performExport ::
  StateT WalletWidgetState (StateT p (B.EventM WidgetName)) ()
performExport = do
  UiLangFace{..} <- use walletLangFaceL
  use walletExportResultL >>= \case
    ExportResultWaiting _ -> pass
    _ -> liftIO (langPutUiCommand $ UiExport) >>=
      assign walletExportResultL . either ExportResultError ExportResultWaiting

toggleAccount ::
  Int -> StateT WalletWidgetState (StateT p (B.EventM WidgetName)) ()
toggleAccount idx = do
  walletAccountsEditL . ix idx . walletAccountSelectedL %= not

performNewAccount ::
  StateT WalletWidgetState (StateT p (B.EventM WidgetName)) ()
performNewAccount = do
  UiLangFace{..} <- use walletLangFaceL
  name <- use walletNewAccountNameL
  wIdx <- uses walletInfoL $ map uwiWalletIdx
  use walletNewAccountResultL >>= \case
    NewAccountResultWaiting _ -> pass
    _ -> liftIO (langPutUiCommand $ UiNewAccount $ UiNewAccountArgs wIdx name) >>=
      assign walletNewAccountResultL . either NewAccountResultError NewAccountResultWaiting

performPasswordChange ::
  StateT WalletWidgetState (StateT p (B.EventM WidgetName)) ()
performPasswordChange = do
  UiLangFace{..} <- use walletLangFaceL
  use walletChangePasswordResultL >>= \case
    ChangePasswordResultWaiting _ -> pass
    _ -> liftIO (langPutUiCommand $ UiChangePassword $ UiChangePasswordArgs) >>=
      assign walletChangePasswordResultL . either ChangePasswordResultError ChangePasswordResultWaiting
