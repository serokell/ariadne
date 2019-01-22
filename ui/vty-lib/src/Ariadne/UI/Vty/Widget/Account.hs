module Ariadne.UI.Vty.Widget.Account
       ( initAccountWidget
       , cutAddressHash
       ) where

import Control.Exception (handle)
import Control.Lens (assign, ix, makeLensesWith, (%=), (.=))
import System.Hclip (ClipboardException, setClipboard)

import qualified Brick as B
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Data.List.NonEmpty as NE

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

data AccountAddress =
  AccountAddress
    { accountAddressHash :: !Text
    , accountAddressBalance :: !(UiCurrency Vty)
    }

data AccountWidgetState =
  AccountWidgetState
    { accountLangFace :: !(UiLangFace Vty)
    , accountInfo :: !(Maybe (UiAccountInfo Vty))

    , accountName :: !Text
    , accountNameEdit :: !Text
    , accountRenameResult :: !RenameResult
    , accountRemoveResult :: !RemoveResult
    , accountBalance :: !BalanceResult

    , accountAddressResult :: !AddressResult
    , accountAddresses :: ![AccountAddress]
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

data AddressResult
  = AddressResultNone
  | AddressResultWaiting !UiCommandId
  | AddressResultError !Text
  | AddressNewResultSuccess
  | AddressCopyResultSuccess !Text  -- ^ Address

makeLensesWith postfixLFields ''AccountAddress
makeLensesWith postfixLFields ''AccountWidgetState

initAccountWidget :: UiLangFace Vty -> Widget p
initAccountWidget langFace =
  initWidget $ do
    setWidgetDrawWithFocus drawAccountWidget
    setWidgetScrollable
    setWidgetHandleEvent handleAccountWidgetEvent
    setWidgetState AccountWidgetState
      { accountLangFace = langFace
      , accountInfo = Nothing

      , accountName = ""
      , accountNameEdit = ""
      , accountRenameResult = RenameResultNone
      , accountRemoveResult = RemoveResultNone
      , accountBalance = BalanceResultNone

      , accountAddressResult = AddressResultNone
      , accountAddresses = []
      }

    addWidgetChild WidgetNameAccountName $
      initEditWidget $ widgetParentLens accountNameEditL
    addWidgetChild WidgetNameAccountRenameButton $
      initButtonWidget "Rename"
    addWidgetEventHandler WidgetNameAccountRenameButton $ \case
      WidgetEventButtonPressed -> performRename
      _ -> pass

    addWidgetChild WidgetNameAccountRemoveButton $
      initButtonWidget "Remove"
    addWidgetEventHandler WidgetNameAccountRemoveButton $ \case
      WidgetEventButtonPressed -> performRemove
      _ -> pass

    addWidgetChild WidgetNameAccountSend $
      initSendWidget langFace
        (Just $ widgetParentGetter $ map uaciWalletIdx . accountInfo)
        (Just $ widgetParentGetter $
          maybe [] (NE.take 1 . map fromIntegral . uaciPath) . accountInfo)

    addWidgetChild WidgetNameAccountAddressGenerateButton $
      initButtonWidget "Generate"
    addWidgetEventHandler WidgetNameAccountAddressGenerateButton $ \case
      WidgetEventButtonPressed -> performNewAddress
      _ -> pass

    addWidgetChild WidgetNameAccountAddressList $
      initListWidget (widgetParentGetter accountAddresses) drawAddressRow
    addWidgetEventHandler WidgetNameAccountAddressList $ \case
      WidgetEventListSelected idx -> performCopyAddress idx
      _ -> pass

    updateFocusList

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

drawAddressRow :: Bool -> AccountAddress -> B.Widget WidgetName
drawAddressRow focused AccountAddress{..} =
  (if focused then B.withAttr "selected" else id) $
  B.Widget
    { B.hSize = B.Greedy
    , B.vSize = B.Fixed
    , B.render = render
    }
  where
    render = do
      rdrCtx <- B.getContext
      let
        attr = rdrCtx ^. B.attrL
        width = rdrCtx ^. B.availWidthL

        padBalance b = T.replicate (max 0 (15 - T.length b)) " " <> b
        balance = padBalance $ getUiCurrency accountAddressBalance

        addressWidth = width - (T.length balance) - 1
        addressLength = T.length accountAddressHash
        address = cutAddressHash accountAddressHash addressWidth addressLength
            <> T.replicate (addressWidth - addressLength) " "

        img = V.horizCat $ V.text' attr <$> [address, " ", balance]

      return $
        B.emptyResult
          & B.imageL .~ img

cutAddressHash :: Text -> Int -> Int -> Text
cutAddressHash addressHash availableWidth addressLength
    | availableWidth <= 3 = T.replicate availableWidth "."
    | availableWidth >= addressLength = addressHash
    | otherwise =
        T.take ((availableWidth - 3) `div` 2) addressHash <>
        "..." <>
        T.takeEnd (availableWidth - 3 - (availableWidth - 3) `div` 2) addressHash

drawAccountWidget :: WidgetName -> AccountWidgetState -> WidgetDrawM AccountWidgetState p WidgetDrawing
drawAccountWidget focus AccountWidgetState{..} = do
  widget <- ask
  widgetName <- getWidgetName

  let
    padBottom = B.padBottom (B.Pad 1)
    padLeft = B.padLeft (B.Pad 1)
    fillLeft w = T.takeEnd w . (T.append $ T.replicate w " ")

    labelWidth = 16

    drawChild = last . drawWidgetChild focus widget
    label = B.padRight (B.Pad 1) . B.txt . fillLeft labelWidth

  return . singleDrawing $
    scrollingViewport widgetName B.Vertical $
    B.padAll 1 $
    B.vBox $
    padBottom <$>
      [ label "Account name:"
          B.<+> drawChild WidgetNameAccountName
          B.<+> padLeft (drawChild WidgetNameAccountRenameButton)
          B.<+> padLeft (drawChild WidgetNameAccountRemoveButton)
      , case accountRenameResult of
          RenameResultNone -> B.emptyWidget
          RenameResultWaiting _ -> B.txt "Renaming..."
          RenameResultError err -> B.txt $ "Couldn't rename the account: " <> err
          RenameResultSuccess -> B.emptyWidget
      , case accountRemoveResult of
          RemoveResultNone -> B.emptyWidget
          RemoveResultWaiting _ -> B.txt "Removing..."
          RemoveResultError err -> B.txt $ "Couldn't remove the account: " <> err
          RemoveResultSuccess -> B.emptyWidget
      , padBottom $ label "Balance:" B.<+> case accountBalance of
          BalanceResultNone -> B.emptyWidget
          BalanceResultWaiting _ -> B.txt "requesting..."
          BalanceResultError err -> B.txt err
          BalanceResultSuccess balance -> B.txt balance

      , drawChild WidgetNameAccountSend

      , B.txt "Addresses"
      , B.hBox
        [ drawChild WidgetNameAccountAddressGenerateButton
        , padLeft $ case accountAddressResult of
            AddressResultNone -> B.emptyWidget
            AddressResultWaiting _ -> B.txt "Generating..."
            AddressResultError err -> B.txt err
            AddressNewResultSuccess -> B.txt "Generated"
            AddressCopyResultSuccess address -> B.txt $ "Copied to clipboard: " <> address
        ]
      , drawChild WidgetNameAccountAddressList
      ]

----------------------------------------------------------------------------
-- Events
----------------------------------------------------------------------------

handleAccountWidgetEvent
  :: UiEvent Vty
  -> WidgetEventM AccountWidgetState p ()
handleAccountWidgetEvent = \case
  UiWalletEvent UiWalletUpdate{..} -> do
    whenJust wuSelectionInfo $ \case
      UiSelectionAccount newInfo@UiAccountInfo{..} -> do
        zoomWidgetState $ do
          UiLangFace{..} <- use accountLangFaceL

          curInfo <- use accountInfoL
          when (curInfo /= Just newInfo) $ do
            accountRenameResultL .= RenameResultNone
            accountRemoveResultL .= RemoveResultNone
            accountAddressResultL .= AddressResultNone
          accountInfoL .= Just newInfo

          whenJust uaciLabel $ updateEditable accountNameL accountNameEditL
          accountAddressesL .=
            map (\UiAddressInfo{..} -> AccountAddress uadiAddress uadiBalance) uaciAddresses
          accountBalanceL .= BalanceResultSuccess (getUiCurrency uaciBalance)
        updateFocusList
      _ -> pass
  UiCommandResult commandId (UiRenameCommandResult result) ->
    zoomWidgetState $ do
      accountRenameResultL %= \case
        RenameResultWaiting commandId' | commandId == commandId' ->
          case result of
            UiRenameCommandSuccess -> RenameResultSuccess
            UiRenameCommandFailure err -> RenameResultError err
        other -> other
  UiCommandResult commandId (UiRemoveCommandResult result) ->
    zoomWidgetState $ do
      accountRemoveResultL %= \case
        RemoveResultWaiting commandId' | commandId == commandId' ->
          case result of
            UiRemoveCommandSuccess -> RemoveResultSuccess
            UiRemoveCommandFailure err -> RemoveResultError err
        other -> other
  UiCommandResult commandId (UiBalanceCommandResult result) ->
    zoomWidgetState $ do
      accountBalanceL %= \case
        BalanceResultWaiting commandId' | commandId == commandId' ->
          case result of
            UiBalanceCommandSuccess balance -> BalanceResultSuccess balance
            UiBalanceCommandFailure err -> BalanceResultError err
        other -> other
  UiCommandResult commandId (UiFrontendCommandResult (UiNewAddressCommandResult result)) ->
    zoomWidgetState $ do
      accountAddressResultL %= \case
        AddressResultWaiting commandId' | commandId == commandId' ->
          case result of
            UiNewAddressCommandSuccess -> AddressNewResultSuccess
            UiNewAddressCommandFailure err -> AddressResultError err
        other -> other
  _ ->
    pass

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

updateFocusList :: Monad m => StateT (WidgetInfo AccountWidgetState p) m ()
updateFocusList = do
  addresses <- use (widgetStateL . accountAddressesL)
  setWidgetFocusList $
    [ WidgetNameAccountName
    , WidgetNameAccountRenameButton
    , WidgetNameAccountRemoveButton
    , WidgetNameAccountSend
    , WidgetNameAccountAddressGenerateButton
    ] ++
    (if null addresses then [] else [WidgetNameAccountAddressList])

performRename :: WidgetEventM AccountWidgetState p ()
performRename = zoomWidgetState $ do
  UiLangFace{..} <- use accountLangFaceL
  name <- use accountNameEditL
  use accountRenameResultL >>= \case
    RenameResultWaiting _ -> pass
    _ -> liftIO (langPutUiCommand $ UiRename $ UiRenameArgs name) >>=
      assign accountRenameResultL . either RenameResultError RenameResultWaiting

performRemove :: WidgetEventM AccountWidgetState p ()
performRemove = zoomWidgetState $ do
  UiLangFace{..} <- use accountLangFaceL
  use accountRemoveResultL >>= \case
    RemoveResultWaiting _ -> pass
    _ -> liftIO (langPutUiCommand UiRemove) >>=
      assign accountRemoveResultL . either RemoveResultError RemoveResultWaiting

performNewAddress :: WidgetEventM AccountWidgetState p ()
performNewAddress = zoomWidgetState $ do
  UiLangFace{..} <- use accountLangFaceL
  mUiAccountInfo <- use accountInfoL
  let wIdx = uaciWalletIdx <$> mUiAccountInfo
      aIdx = case uaciPath <$> mUiAccountInfo of
        Just (x:|_) -> Just x
        _ -> Nothing
  use accountAddressResultL >>= \case
    AddressResultWaiting _ -> pass
    _ -> liftIO (langPutUiCommand . UiFrontendCommand . UiNewAddress $ UiNewAddressArgs wIdx aIdx) >>=
      assign accountAddressResultL . either AddressResultError AddressResultWaiting

performCopyAddress :: Int -> WidgetEventM AccountWidgetState p ()
performCopyAddress idx = zoomWidgetState $ do
  whenJustM ((^? ix idx) <$> use accountAddressesL) $ \AccountAddress{..} -> do
    result <- liftIO . handle (\e -> return $ AddressResultError $ fromString $ displayException (e :: ClipboardException)) $ do
      setClipboard . toString $ accountAddressHash
      return $ AddressCopyResultSuccess accountAddressHash
    accountAddressResultL .= result
