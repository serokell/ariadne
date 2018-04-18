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
       , uiGetSelectedItem
       ) where

import Universum

import Control.Exception (displayException)
import Control.Lens (ix)
import Data.Text (pack)
import Data.Tree (Tree(..))
import Data.Unique
import IiExtras
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.Knit.Face
import Ariadne.TaskManager.Face
import Ariadne.UI.Vty.Face
import Ariadne.Wallet.Face

import qualified Ariadne.TaskManager.Knit as Knit
import qualified Ariadne.Wallet.Knit as Knit
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
     , Elem components Knit.Core
     , Elem components Knit.TaskManager
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
    , langMkExpr = convertOperation
    }
  where
    convertOperation = \case
      UiSelect ws ->
        Knit.ExprProcCall
          (Knit.ProcCall Knit.selectCommandName
           (map (Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitNumber . fromIntegral) ws)
          )
      UiBalance -> Knit.ExprProcCall (Knit.ProcCall Knit.balanceCommandName [])
      UiKill commandId ->
        Knit.ExprProcCall
          (Knit.ProcCall Knit.killCommandName
            [Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitTaskId . TaskId $ commandId]
          )
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
    , cmdTaskIdRendered = fmap (\(TaskId i) -> pack $ '<' : show i ++ ">") mi
    , cmdTaskId = fmap (\(TaskId i) -> i) mi
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
knitCommandResultToUI commandId = Just . UiCommandResultEvent commandId . \case
  KnitCommandSuccess v ->
    UiCommandSuccess $ Knit.ppValue v
  KnitCommandEvalError e ->
    UiCommandFailure $ Knit.ppEvalError e
  KnitCommandProcError e ->
    UiCommandFailure $ Knit.ppResolveErrors e
  KnitCommandException e ->
    UiCommandFailure $ PP.text (displayException e)

knitCommandOutputToUI :: UiCommandId -> PP.Doc -> UiEvent
knitCommandOutputToUI commandId doc = UiCommandResultEvent commandId (UiCommandOutput doc)

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
        (walletSelectionToPane us <$> sel)

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

walletSelectionToPane :: UserSecret -> WalletSelection -> UiWalletPaneInfo
walletSelectionToPane us WalletSelection{..} = UiWalletPaneInfo{..}
  where
    wpiWalletIdx = wsWalletIndex
    wpiPath = wsPath
    (wpiType, wpiLabel) = case us ^. usWallets ^? ix (fromIntegral wsWalletIndex) of
      Nothing -> error "Invalid wallet index"
      Just WalletData{..} -> case wsPath of
        [] -> (Just UiWalletPaneInfoWallet, Just _wdName)
        accIdx:accPath -> case _wdAccounts ^? ix (fromIntegral accIdx) of
          Nothing -> error "Invalid account index"
          Just AccountData{..} -> case accPath of
            [] -> (Just UiWalletPaneInfoAccount, Just _adName)
            addrIdx:_ -> case _adAddresses ^? ix (fromIntegral addrIdx) of
              Nothing -> error "Invalid address index"
              Just (_, address) -> (Just UiWalletPaneInfoAddress, Just $ pretty address)

-- | Get currently selected item from the backend and convert it to
-- 'UiSelectedItem'.
uiGetSelectedItem :: WalletFace -> IO UiSelectedItem
uiGetSelectedItem WalletFace {walletGetSelection} =
    walletGetSelection <&> \case
        (Nothing, _) -> UiNoSelection
        (Just WalletSelection {..}, us) ->
            getItem (us ^? usWallets . ix (fromIntegral wsWalletIndex)) wsPath
  where
    getItem :: Maybe WalletData -> [Word] -> UiSelectedItem
    getItem Nothing _ = error "Non-existing wallet is selected"
    getItem (Just wd) [] = UiSelectedWallet (_wdName wd)
    getItem (Just wd) (accIdx:rest) =
        case wd ^? wdAccounts . ix (fromIntegral accIdx) of
            Nothing -> error "Non-existing account is selected"
            Just ad ->
                case rest of
                    [] -> UiSelectedAccount (_adName ad)
                    [addrIdx] ->
                        case ad ^? adAddresses . ix (fromIntegral addrIdx) of
                            Nothing -> error "Non-existing address is selected"
                            Just (_, addr) -> UiSelectedAddress (pretty addr)
                    _ -> error "Invalid selection: too long"
