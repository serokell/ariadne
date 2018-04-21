-- | Glue code between the frontend and the backends.

module Glue
       (
         -- * Knit ↔ Vty
         knitFaceToUI

         -- * Cardano ↔ Vty
       , putCardanoEventToUI

         -- * Wallet ↔ Vty
       , walletEventToUI
       , putWalletEventToUI
       ) where

import Universum

import Control.Exception (displayException)
import Control.Lens (ix)
import Data.Text (pack)
import Data.Tree (Tree(..))
import Data.Unique
import NType
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.Knit.Face
import Ariadne.TaskManager.Face
import Ariadne.UI.Vty.Face
import Ariadne.Wallet.Face

import qualified Knit

----------------------------------------------------------------------------
-- Glue between Knit backend and Vty frontend
----------------------------------------------------------------------------

knitFaceToUI
  :: forall components.
     ( KnownSpine components
     , AllConstrained (Knit.ComponentTokenizer components) components
     , AllConstrained (Knit.ComponentLitGrammar components) components
     , AllConstrained (Knit.ComponentInflate components) components
     , AllConstrained Knit.ComponentPrinter components
     )
  => UiFace
  -> KnitFace components
  -> UiLangFace
knitFaceToUI UiFace{..} KnitFace{..} =
  UiLangFace
    { langPutCommand = \expr -> do
        cid <- newUnique
        fmap (commandIdToUI cid) . putKnitCommand (commandHandle cid) $ expr
    , langParse = Knit.parse
    , langPpExpr = Knit.ppExpr
    , langPpParseError = Knit.ppParseError
    , langParseErrSpans = Knit.parseErrorSpans
    }
  where
    commandHandle commandId = KnitCommandHandle
      { putCommandResult = \mtid result ->
          whenJust (knitCommandResultToUI (commandIdToUI commandId mtid) result) putUiEvent
      , putCommandOutput = \tid doc ->
          putUiEvent $ knitCommandOutputToUI (commandIdToUI commandId (Just tid)) doc
      }

commandIdToUI :: Unique -> Maybe TaskId -> UiCommandId
commandIdToUI u mi =
  UiCommandId
    { cmdIdEqObject = fromIntegral (hashUnique u)
    , cmdIdRendered = fmap (\(TaskId i) -> pack $ '<' : show i ++ ">") mi
    }

-- The 'Maybe' here is not used for now, but in the future might be, if some
-- event couldn't be mapped to a UI event.
knitCommandResultToUI
  :: forall components.
     ( AllConstrained Knit.ComponentPrinter components
     , AllConstrained (Knit.ComponentInflate components) components
     )
  => UiCommandId
  -> KnitCommandResult components
  -> Maybe UiEvent
knitCommandResultToUI commandId = Just . UiCommandEvent commandId . \case
  KnitCommandSuccess v ->
    UiCommandSuccess $ Knit.ppValue v
  KnitCommandEvalError e ->
    UiCommandFailure $ Knit.ppEvalError e
  KnitCommandProcError e ->
    UiCommandFailure $ Knit.ppResolveErrors e
  KnitCommandException e ->
    UiCommandFailure $ PP.text (displayException e)

knitCommandOutputToUI :: UiCommandId -> PP.Doc -> UiEvent
knitCommandOutputToUI commandId doc = UiCommandEvent commandId (UiCommandOutput doc)

----------------------------------------------------------------------------
-- Glue between the Cardano backend and Vty frontend
----------------------------------------------------------------------------

-- The 'Maybe' here is not used for now, but in the future might be, if some
-- event couldn't be mapped to a UI event.
cardanoEventToUI :: CardanoEvent -> Maybe UiEvent
cardanoEventToUI = \case
  CardanoLogEvent message ->
    Just $ UiCardanoEvent $
      UiCardanoLogEvent message
  CardanoStatusUpdateEvent CardanoStatusUpdate{..} ->
    Just $ UiCardanoEvent $
      UiCardanoStatusUpdateEvent UiCardanoStatusUpdate
        { tipHeaderHash = pretty tipHeaderHash
        , tipSlot = pretty tipEpochOrSlot
        , currentSlot = pretty currentSlot
                     <> if isInaccurate then " (inaccurate)" else ""
        }

putCardanoEventToUI :: UiFace -> CardanoEvent -> IO ()
putCardanoEventToUI UiFace{..} ev =
  whenJust (cardanoEventToUI ev) putUiEvent

----------------------------------------------------------------------------
-- Glue between the Wallet backend and Vty frontend
----------------------------------------------------------------------------

-- The 'Maybe' here is not used for now, but in the future might be, if some
-- event couldn't be mapped to a UI event.
walletEventToUI :: WalletEvent -> Maybe UiEvent
walletEventToUI = \case
  WalletUserSecretSetEvent us sel ->
    Just $ UiWalletEvent $
      UiWalletUpdate
        (userSecretToTree us)
        (walletSelectionToUI <$> sel)
        (userSecretToPane us <$> sel)

walletSelectionToUI :: WalletSelection -> UiWalletTreeSelection
walletSelectionToUI WalletSelection{..} =
  UiWalletTreeSelection { wtsWalletIdx = wsWalletIndex, wtsPath = wsPath }

putWalletEventToUI :: UiFace -> WalletEvent -> IO ()
putWalletEventToUI UiFace{..} ev =
  whenJust (walletEventToUI ev) putUiEvent

userSecretToTree :: UserSecret -> [UiWalletTree]
userSecretToTree = map toTree . view usWallets
  where
    toTree :: WalletData -> UiWalletTree
    toTree WalletData {..} =
        Node
            { rootLabel = UiWalletTreeItem (Just _wdName) [] False
            , subForest = toList $ map toAccountNode _wdAccounts
            }
      where
        toAccountNode :: AccountData -> UiWalletTree
        toAccountNode AccountData {..} =
            Node
                { rootLabel =
                      UiWalletTreeItem
                          { wtiLabel = Just _adName
                          , wtiPath = [fromIntegral _adPath]
                          , wtiShowPath = True
                          }
                , subForest = toList $ map (toAddressNode _adPath) _adAddresses
                }
        toAddressNode :: Word32 -> (Word32, Address) -> UiWalletTree
        toAddressNode accIdx (addrIdx, address) =
            pure $
            UiWalletTreeItem
                { wtiLabel = Just (pretty address)
                , wtiPath = map fromIntegral [accIdx, addrIdx]
                , wtiShowPath = True
                }

userSecretToPane :: UserSecret -> WalletSelection -> UiWalletPaneInfo
userSecretToPane us WalletSelection{..} =
  case wsPath of
    [] ->
      let
        walletName =
          case us ^. usWallets ^? ix (fromIntegral wsWalletIndex) of
            Nothing ->
              "FIXME: Internal invariant violated. We should prevent \
              \selection of non-existent wallets"
            Just wd -> _wdName wd
      in
        UiWalletPaneWalletInfo walletName
    [i] -> UiWalletPaneAccountInfo (pretty i)
    _ -> UiWalletPaneAddressInfo
