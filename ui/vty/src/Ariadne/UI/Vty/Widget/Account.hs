module Ariadne.UI.Vty.Widget.Account
       ( initAccountWidget
       ) where

import Universum

import Control.Exception (handle)
import Control.Lens (assign, ix, makeLensesWith, (%=), (.=))
import IiExtras
import System.Hclip (ClipboardException, setClipboard)

import qualified Brick as B
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Ariadne.UI.Vty.Face
import Ariadne.UI.Vty.Widget
import Ariadne.UI.Vty.Widget.Form.Button
import Ariadne.UI.Vty.Widget.Form.Edit
import Ariadne.UI.Vty.Widget.Form.List
import Ariadne.UI.Vty.Widget.Form.Send

----------------------------------------------------------------------------
-- Model
----------------------------------------------------------------------------

data AccountAddress =
  AccountAddress
    { accountAddressHash :: !Text
    , accountAddressBalance :: !Text
    }

data AccountWidgetState =
  AccountWidgetState
    { accountLangFace :: !UiLangFace

    , accountName :: !Text
    , accountRenameResult :: !RenameResult
    , accountBalance :: !Text

    , accountAddressResult :: !AddressResult
    , accountAddresses :: ![AccountAddress]
    }

data RenameResult
  = RenameResultNone
  | RenameResultWaiting !UiCommandId
  | RenameResultError !Text
  | RenameResultSuccess

data AddressResult
  = AddressResultNone
  | AddressResultWaiting !UiCommandId
  | AddressResultError !Text
  | AddressNewResultSuccess
  | AddressCopyResultSuccess !Text  -- ^ Address

makeLensesWith postfixLFields ''AccountAddress
makeLensesWith postfixLFields ''AccountWidgetState

initAccountWidget :: UiLangFace -> Widget p
initAccountWidget langFace =
  initWidget $ do
    setWidgetDrawWithFocus drawAccountWidget
    setWidgetScrollable
    setWidgetHandleEvent handleAccountWidgetEvent
    setWidgetState AccountWidgetState
      { accountLangFace = langFace

      , accountName = ""
      , accountRenameResult = RenameResultNone
      , accountBalance = ""

      , accountAddressResult = AddressResultNone
      , accountAddresses = []
      }

    addWidgetChild WidgetNameAccountName $
      initEditWidget $ widgetParentLens accountNameL
    addWidgetChild WidgetNameAccountRenameButton $
      initButtonWidget "Rename"
    addWidgetEventHandler WidgetNameAccountRenameButton $ \case
      WidgetEventButtonPressed -> performRename
      _ -> return ()

    addWidgetChild WidgetNameAccountSend $
      initSendWidget langFace Nothing

    addWidgetChild WidgetNameAccountAddressGenerateButton $
      initButtonWidget "Generate"
    addWidgetEventHandler WidgetNameAccountAddressGenerateButton $ \case
      WidgetEventButtonPressed -> performNewAddress
      _ -> return ()

    addWidgetChild WidgetNameAccountAddressList $
      initListWidget (widgetParentGetter accountAddresses) drawAddressRow
    addWidgetEventHandler WidgetNameAccountAddressList $ \case
      WidgetEventListSelected idx -> performCopyAddress idx
      _ -> return ()

    setWidgetFocusList
      [ WidgetNameAccountName
      , WidgetNameAccountRenameButton
      , WidgetNameAccountSend
      , WidgetNameAccountAddressGenerateButton
      , WidgetNameAccountAddressList
      ]

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

        balance = T.replicate (max 0 (15 - T.length accountAddressBalance)) " " <> accountAddressBalance

        addressWidth = width - (T.length balance) - 1
        addressLength = T.length accountAddressHash
        address = if addressWidth >= addressLength
          then accountAddressHash <> T.replicate (addressWidth - addressLength) " "
          else
            T.take ((addressWidth - 3) `div` 2) accountAddressHash <>
            "..." <>
            T.takeEnd (addressWidth - 3 - (addressWidth - 3) `div` 2) accountAddressHash

        img = V.horizCat $ V.text' attr <$> [address, " ", balance]

      return $
        B.emptyResult
          & B.imageL .~ img

drawAccountWidget :: WidgetName -> AccountWidgetState -> WidgetDrawM AccountWidgetState p (B.Widget WidgetName)
drawAccountWidget focus AccountWidgetState{..} = do
  widget <- ask
  widgetName <- getWidgetName

  let
    padBottom = B.padBottom (B.Pad 1)
    padLeft = B.padLeft (B.Pad 1)
    fillLeft w = T.takeEnd w . (T.append $ T.replicate w " ")

    labelWidth = 16

    visible namePart = if focus == widgetName ++ [namePart] then B.visible else identity
    drawChild namePart = visible namePart $ drawWidgetChild focus widget namePart
    label = B.padRight (B.Pad 1) . B.txt . fillLeft labelWidth

  return $
    B.viewport widgetName B.Vertical $
    B.padAll 1 $
    B.vBox $
    padBottom <$>
      [ label "Account name:"
          B.<+> drawChild WidgetNameAccountName
          B.<+> padLeft (drawChild WidgetNameAccountRenameButton)
      , case accountRenameResult of
          RenameResultNone -> B.emptyWidget
          RenameResultWaiting _ -> B.txt "Renaming..."
          RenameResultError err -> B.txt $ "Couldn't rename the account: " <> err
          RenameResultSuccess -> B.emptyWidget
      , padBottom $ label "Balance:" B.<+> B.txt accountBalance

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
  :: UiEvent
  -> WidgetEventM AccountWidgetState p ()
handleAccountWidgetEvent = \case
  UiWalletEvent UiWalletUpdate{..} -> do
    whenJust wuSelectionInfo $ \case
      UiSelectionAccount UiAccountInfo{..} -> do
        accountNameL .= fromMaybe "" uaciLabel
        accountBalanceL .= uaciBalance
        accountAddressesL .= map (\UiAddressInfo{..} -> AccountAddress uadiAddress uadiBalance) uaciAddresses
      _ -> return ()
  UiCommandResult commandId (UiRenameCommandResult result) -> do
    accountRenameResultL %= \case
      RenameResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiRenameCommandSuccess -> RenameResultSuccess
          UiRenameCommandFailure err -> RenameResultError err
      other -> other
  UiCommandResult commandId (UiNewAddressCommandResult result) -> do
    accountAddressResultL %= \case
      AddressResultWaiting commandId' | commandId == commandId' ->
        case result of
          UiNewAddressCommandSuccess -> AddressNewResultSuccess
          UiNewAddressCommandFailure err -> AddressResultError err
      other -> other
  _ ->
    return ()

----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------

performRename :: WidgetEventM AccountWidgetState p ()
performRename = do
  UiLangFace{..} <- use accountLangFaceL
  name <- use accountNameL
  use accountRenameResultL >>= \case
    RenameResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiRename $ UiRenameArgs name) >>=
      assign accountRenameResultL . either RenameResultError RenameResultWaiting

performNewAddress :: WidgetEventM AccountWidgetState p ()
performNewAddress = do
  UiLangFace{..} <- use accountLangFaceL
  use accountAddressResultL >>= \case
    AddressResultWaiting _ -> return ()
    _ -> liftIO (langPutUiCommand $ UiNewAddress) >>=
      assign accountAddressResultL . either AddressResultError AddressResultWaiting

performCopyAddress :: Int -> WidgetEventM AccountWidgetState p ()
performCopyAddress idx = do
  whenJustM ((^? ix idx) <$> use accountAddressesL) $ \AccountAddress{..} -> do
    result <- liftIO . handle (\e -> return $ AddressResultError $ fromString $ displayException (e :: ClipboardException)) $ do
      setClipboard . toString $ accountAddressHash
      return $ AddressCopyResultSuccess accountAddressHash
    accountAddressResultL .= result
