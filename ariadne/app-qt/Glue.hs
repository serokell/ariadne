-- | Glue code between the frontend and the backends.

module Glue
       (
         -- * Knit ↔ Qt
         knitFaceToUI
       , putKnitEventToUI

         -- * Cardano ↔ Qt
       , putCardanoEventToUI

         -- * Wallet ↔ Qt
       , walletEventToUI
       , putWalletEventToUI
       ) where

import Universum

import Control.Exception (displayException)
import Control.Lens (at, non)
import Data.Text (pack)
import Data.Tree (Tree(..))
import Data.Unique
import IiExtras
import Numeric
import Prelude ((!!))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.CommandId
import Ariadne.Knit.Face
import Ariadne.UI.Qt.Face
  (UiCardanoEvent(..), UiCardanoStatusUpdate(..), UiCommandEvent(..),
  UiCommandId(..), UiEvent(..), UiFace(..), UiLangFace(..), UiWalletEvent(..),
  UiWalletTree, UiWalletTreeItem(..), UiWalletTreeSelection(..))
import Ariadne.Wallet.Face

import qualified Knit

----------------------------------------------------------------------------
-- Glue between Knit backend and Qt frontend
----------------------------------------------------------------------------

knitFaceToUI
  :: forall components.
     ( KnownSpine components
     , AllConstrained (Knit.ComponentTokenizer components) components
     , AllConstrained (Knit.ComponentLitGrammar components) components
     , AllConstrained Knit.ComponentPrinter components
     )
  => KnitFace components
  -> UiLangFace
knitFaceToUI KnitFace{..} =
  UiLangFace
    { langPutCommand = fmap commandIdToUI . putKnitCommand
    , langParse = Knit.parse
    , langPpExpr = Knit.ppExpr
    , langPpParseError = Knit.ppParseError
    , langParseErrSpans = Knit.parseErrorSpans
    }

commandIdToUI :: CommandId -> UiCommandId
commandIdToUI (CommandId u) =
  UiCommandId
    { cmdIdEqObject = fromIntegral i
    , cmdIdRendered = pack $ '<' : showIntAtBase 36 base36Char i ">"
    }
  where
    i = hashUnique u
    base36Char = (alphabet!!)
    alphabet = "0123456789" ++ ['a'..'z']

-- The 'Maybe' here is not used for now, but in the future might be, if some
-- event couldn't be mapped to a UI event.
knitEventToUI
  :: forall components.
     ( AllConstrained Knit.ComponentPrinter components
     , AllConstrained (Knit.ComponentInflate components) components
     )
  => KnitEvent components
  -> Maybe UiEvent
knitEventToUI = \case
  KnitCommandResultEvent commandId commandResult ->
    Just $ UiCommandEvent (commandIdToUI commandId) $
      case commandResult of
        KnitCommandSuccess v ->
          UiCommandSuccess $ Knit.ppValue v
        KnitCommandEvalError e ->
          UiCommandFailure $ Knit.ppEvalError e
        KnitCommandProcError e ->
          UiCommandFailure $ Knit.ppResolveErrors e
        KnitCommandException e ->
          UiCommandFailure $ PP.text (displayException e)
  KnitCommandOutputEvent commandId doc ->
    Just $ UiCommandEvent (commandIdToUI commandId) (UiCommandOutput doc)

putKnitEventToUI
  :: forall components.
     ( AllConstrained Knit.ComponentPrinter components
     , AllConstrained (Knit.ComponentInflate components) components
     )
  => UiFace
  -> KnitEvent components
  -> IO ()
putKnitEventToUI UiFace{..} ev =
  whenJust (knitEventToUI ev) putUiEvent

----------------------------------------------------------------------------
-- Glue between the Cardano backend and Qt frontend
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
-- Glue between the Wallet backend and Qt frontend
----------------------------------------------------------------------------

-- The 'Maybe' here is not used for now, but in the future might be, if some
-- event couldn't be mapped to a UI event.
walletEventToUI :: WalletEvent -> Maybe UiEvent
walletEventToUI = \case
  WalletUserSecretSetEvent us sel ->
    Just $ UiWalletEvent $
      UiWalletTreeUpdate
        (userSecretToTree us)
        (walletSelectionToUI <$> sel)

walletSelectionToUI :: WalletSelection -> UiWalletTreeSelection
walletSelectionToUI WalletSelection{..} =
  UiWalletTreeSelection { wtsWalletIdx = wsWalletIndex, wtsPath = wsPath }


putWalletEventToUI :: UiFace -> WalletEvent -> IO ()
putWalletEventToUI UiFace{..} ev =
  whenJust (walletEventToUI ev) putUiEvent

userSecretToTree :: UserSecret -> [UiWalletTree]
userSecretToTree = map toTree . maybeToList . view usWallet
  where
    toTree :: WalletUserSecret -> UiWalletTree
    toTree WalletUserSecret {..} =
        Node
            { rootLabel = UiWalletTreeItem (Just _wusWalletName) [] False
            , subForest = map toAccountNode _wusAccounts
            }
      where
        foldlStep ::
               Map Word32 [Word32] -> (Word32, Word32) -> Map Word32 [Word32]
        foldlStep m (acc, addr) = m & at acc . non [] %~ (addr :)
        addrsMap :: Map Word32 [Word32]
        addrsMap = foldl' foldlStep mempty _wusAddrs
        toAccountNode :: (Word32, Text) -> UiWalletTree
        toAccountNode (accIdx, accName) =
            Node
                { rootLabel =
                      UiWalletTreeItem
                          { wtiLabel = Just accName
                          , wtiPath = [fromIntegral accIdx]
                          , wtiShowPath = True
                          }
                , subForest =
                      map (toAddressNode accIdx) $ addrsMap ^. at accIdx .
                      non []
                }
        toAddressNode :: Word32 -> Word32 -> UiWalletTree
        toAddressNode accIdx addrIdx =
            pure $
            UiWalletTreeItem
                { wtiLabel = Nothing
                , wtiPath = map fromIntegral [accIdx, addrIdx]
                , wtiShowPath = True
                }
