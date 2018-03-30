-- | Glue code between the frontend and the backends.

module Ariadne.Glue
       (
         -- * Knit ↔ Vty
         knitFaceToUI
       , putKnitEventToUI

         -- * Cardano ↔ Vty
       , userSecretToTree
       ) where

import Universum

import Control.Exception (displayException)
import Control.Lens (at, non)
import qualified Data.Foldable
import Data.Text (pack)
import Data.Tree (Tree(..))
import Data.Unique
import IiExtras
import Numeric
import Prelude ((!!))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.Cardano.Face (UserSecret, WalletUserSecret(..), usWallet)
import Ariadne.CommandId
import Ariadne.Knit.Face
import Ariadne.UI.Vty.Face

import qualified Knit

----------------------------------------------------------------------------
-- Glue between Knit backend and Vty frontend
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
  Data.Foldable.traverse_ putUiEvent (knitEventToUI ev)

----------------------------------------------------------------------------
-- Glue between Cardano backend and Vty frontend
----------------------------------------------------------------------------

userSecretToTree :: UserSecret -> [WalletTree]
userSecretToTree = map toTree . maybeToList . view usWallet
  where
    toTree :: WalletUserSecret -> WalletTree
    toTree WalletUserSecret {..} =
        Node
            { rootLabel = WalletTreeItem (Just _wusWalletName) [] False
            , subForest = map toAccountNode _wusAccounts
            }
      where
        foldlStep ::
               Map Word32 [Word32] -> (Word32, Word32) -> Map Word32 [Word32]
        foldlStep m (acc, addr) = m & at acc . non [] %~ (addr :)
        addrsMap :: Map Word32 [Word32]
        addrsMap = foldl' foldlStep mempty _wusAddrs
        toAccountNode :: (Word32, Text) -> WalletTree
        toAccountNode (accIdx, accName) =
            Node
                { rootLabel =
                      WalletTreeItem
                          { wtiLabel = Just accName
                          , wtiPath = [fromIntegral accIdx]
                          , wtiShowPath = True
                          }
                , subForest =
                      map (toAddressNode accIdx) $ addrsMap ^. at accIdx .
                      non []
                }
        toAddressNode :: Word32 -> Word32 -> WalletTree
        toAddressNode accIdx addrIdx =
            pure $
            WalletTreeItem
                { wtiLabel = Nothing
                , wtiPath = map fromIntegral [accIdx, addrIdx]
                , wtiShowPath = True
                }
