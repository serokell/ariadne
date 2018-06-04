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

         -- * Update ↔ Vty
       , putUpdateEventToUI
       ) where

import Universum

import Control.Exception (displayException)
import Control.Lens (ix)
import Data.Text (pack)
import Data.Tree (Tree(..))
import Data.Unique
import Data.Version (Version)
import IiExtras
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.Knit.Face
import Ariadne.TaskManager.Face
import Ariadne.UI.Vty.Face
import Ariadne.Wallet.Face

import qualified Ariadne.Cardano.Knit as Knit
import qualified Ariadne.TaskManager.Knit as Knit
import qualified Ariadne.UI.Vty.Knit as Knit
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
     , Elem components Knit.Cardano
     , Elem components Knit.TaskManager
     )
  => UiFace
  -> KnitFace components
  -> UiLangFace
knitFaceToUI UiFace{..} KnitFace{..} =
  UiLangFace
    { langPutCommand = putCommand commandHandle
    , langPutUiCommand = putUiCommand
    , langParse = Knit.parse
    , langPpExpr = Knit.ppExpr
    , langPpParseError = Knit.ppParseError
    , langParseErrSpans = Knit.parseErrorSpans
    , langGetHelp = getKnitHelp (Proxy @components)
    }
  where
    putCommand handle expr = do
      cid <- newUnique
      fmap (commandIdToUI cid) . putKnitCommand (handle cid) $ expr
    commandHandle commandId = KnitCommandHandle
      { putCommandResult = \mtid result ->
          whenJust (knitCommandResultToUI (commandIdToUI commandId mtid) result) putUiEvent
      , putCommandOutput = \tid doc ->
          putUiEvent $ knitCommandOutputToUI (commandIdToUI commandId (Just tid)) doc
      }

    putUiCommand op = case opToExpr op of
      Left err -> return $ Left err
      Right expr -> fmap Right $ putCommand (uiCommandHandle op) expr
    uiCommandHandle op commandId = KnitCommandHandle
      { putCommandResult = \mtid result ->
          whenJust (resultToUI result op) $ putUiEvent . UiCommandResult (commandIdToUI commandId mtid)
      , putCommandOutput = \_ _ ->
          return ()
      }

    opToExpr = \case
      UiSelect ws ->
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.selectCommandName
           (map (Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitNumber . fromIntegral) ws)
          )
      UiBalance ->
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.balanceCommandName [])
      UiSend address amount passphrase -> do
        argAddress <- decodeTextAddress address
        argCoin <- readEither amount
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.sendCommandName $
            [ Knit.ArgKw "out" . Knit.ExprProcCall $ Knit.ProcCall Knit.txOutCommandName
                [ Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitAddress $ argAddress
                , Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitNumber $ argCoin
                ]
            ] ++
            (if null passphrase then [] else [Knit.ArgKw "pass" . Knit.ExprLit . Knit.toLit . Knit.LitString $ passphrase])
          )
      UiKill commandId ->
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.killCommandName
            [Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitTaskId . TaskId $ commandId]
          )
      UiCopySelection -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.copySelectionCommandName [])
      UiNewWallet name passphrase -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.newWalletCommandName $
            (if null name then [] else [Knit.ArgKw "name" . Knit.ExprLit . Knit.toLit . Knit.LitString $ name]) ++
            (if null passphrase then [] else [Knit.ArgKw "pass" . Knit.ExprLit . Knit.toLit . Knit.LitString $ passphrase])
          )
      UiRestoreWallet name mnemonic passphrase full -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.restoreCommandName $
            [ Knit.ArgKw "mnemonic" . Knit.ExprLit . Knit.toLit . Knit.LitString $ mnemonic
            , Knit.ArgKw "full" . Knit.componentInflate . Knit.ValueBool $ full
            ] ++
            (if null name then [] else [Knit.ArgKw "name" . Knit.ExprLit . Knit.toLit . Knit.LitString $ name]) ++
            (if null passphrase then [] else [Knit.ArgKw "pass" . Knit.ExprLit . Knit.toLit . Knit.LitString $ passphrase])
          )

    resultToUI result = \case
      UiBalance ->
        Just . UiBalanceCommandResult . either UiBalanceCommandFailure UiBalanceCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueCoin n -> Right $ let (amount, unit) = Knit.showCoin n in amount <> " " <> unit
            _ -> Left "Unrecognized return value"
      UiSend _ _ _ ->
        Just . UiSendCommandResult . either UiSendCommandFailure UiSendCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueHash h -> Right $ pretty h
            _ -> Left "Unrecognized return value"
      UiNewWallet _ _ ->
        Just . UiNewWalletCommandResult . either UiNewWalletCommandFailure UiNewWalletCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueList l -> Right [s | Just (Knit.ValueString s) <- Knit.fromValue <$> l]
            _ -> Left "Unrecognized return value"
      UiRestoreWallet _ _ _ _ ->
        Just . UiRestoreWalletCommandResult . either UiRestoreWalletCommandFailure (const UiRestoreWalletCommandSuccess) $
          fromResult result
      _ -> Nothing

    fromResult = \case
      KnitCommandSuccess v -> Right v
      KnitCommandEvalError _ -> Left $ "Invalid arguments"
      KnitCommandException e -> Left $ fromString $ displayException e
      KnitCommandProcError _ -> error "Undefined command used"

    fromValue
      :: Elem components component
      => Knit.Value components
      -> Either Text (Knit.ComponentValue components component)
    fromValue = maybeToRight "Unrecognized return value" . Knit.fromValue

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
        (walletSelectionToPane us <$> sel)

walletSelectionToUI :: WalletSelection -> UiTreeSelection
walletSelectionToUI WalletSelection{..} =
  UiTreeSelection { wtsWalletIdx = wsWalletIndex, wtsPath = wsPath }

putWalletEventToUI :: UiFace -> WalletEvent -> IO ()
putWalletEventToUI UiFace{..} ev =
  whenJust (walletEventToUI ev) putUiEvent

userSecretToTree :: UserSecret -> [UiTree]
userSecretToTree = map toTree . view usWallets
  where
    toTree :: WalletData -> UiTree
    toTree WalletData {..} =
        Node
            { rootLabel = UiTreeItem (Just _wdName) [] False
            , subForest = toList $ map toAccountNode _wdAccounts
            }
      where
        toAccountNode :: AccountData -> UiTree
        toAccountNode AccountData {..} =
            Node
                { rootLabel =
                      UiTreeItem
                          { wtiLabel = Just _adName
                          , wtiPath = [fromIntegral _adPath]
                          , wtiShowPath = True
                          }
                , subForest = toList $ map (toAddressNode _adPath) _adAddresses
                }
        toAddressNode :: Word32 -> (Word32, Address) -> UiTree
        toAddressNode accIdx (addrIdx, address) =
            pure $
            UiTreeItem
                { wtiLabel = Just (pretty address)
                , wtiPath = map fromIntegral [accIdx, addrIdx]
                , wtiShowPath = True
                }

walletSelectionToPane :: UserSecret -> WalletSelection -> UiWalletInfo
walletSelectionToPane us WalletSelection{..} = UiWalletInfo{..}
  where
    wpiWalletIdx = wsWalletIndex
    wpiPath = wsPath
    (wpiType, wpiLabel) = case us ^. usWallets ^? ix (fromIntegral wsWalletIndex) of
      Nothing -> error "Invalid wallet index"
      Just WalletData{..} -> case wsPath of
        [] -> (Just UiWalletInfoWallet, Just _wdName)
        accIdx:accPath -> case _wdAccounts ^? ix (fromIntegral accIdx) of
          Nothing -> error "Invalid account index"
          Just AccountData{..} -> case accPath of
            [] -> (Just $ UiWalletInfoAccount [_adPath], Just _adName)
            addrIdx:_ -> case _adAddresses ^? ix (fromIntegral addrIdx) of
              Nothing -> error "Invalid address index"
              Just (addrPath, address) -> (Just $ UiWalletInfoAddress [_adPath, addrPath], Just $ pretty address)

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

----------------------------------------------------------------------------
-- Glue between the Update backend and Vty frontend
----------------------------------------------------------------------------

putUpdateEventToUI :: UiFace -> Version -> IO ()
putUpdateEventToUI UiFace{..} ver = putUiEvent $ UiNewVersionEvent ver
