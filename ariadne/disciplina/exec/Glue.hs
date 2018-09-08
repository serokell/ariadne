-- | Glue code between the frontend and the backends.

module Glue
       (
         -- * Knit ↔ Vty
         knitFaceToUI

         -- * Wallet ↔ Vty
       , createWalletState
       , walletEventToUI
       , putWalletEventToUI

         -- * Command history ↔ Vty
       , historyToUI
       ) where

import Control.Exception (displayException)
import Control.Lens (ix)
import Control.Monad.Component (ComponentM, buildComponent_)
import Data.Text (pack)
import Data.Tree (Tree (..))
import Data.Unique
import Dscp.Core (TxOut (..), addrFromText, coinToInteger)
import Dscp.Util (toHex)
import NType (AllConstrained, Elem, KnownSpine)

import qualified Serokell.Util.Base64 as Base64
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Ariadne.Knit.Face
import Ariadne.TaskManager.Face
import Ariadne.UI.Vty.Face
import Dscp.Wallet.Face

import qualified Ariadne.TaskManager.Knit as Knit
import qualified Dscp.Wallet.Knit as Knit
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
     , Elem components Knit.Wallet
     )
  => WalletStateRef
  -> UiFace
  -> WalletFace
  -> KnitFace components
  -> UiLangFace
knitFaceToUI walletStateRef UiFace{..} WalletFace{..} KnitFace{..} =
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

    putUiCommand op = do
      walletState <- readIORef walletStateRef
      case op of
        UiSelect ws
          | [i] <- ws -> do
              writeIORef walletStateRef $ walletState{ selection = mfilter ((< length (accounts walletState)) . fromIntegral) $ Just i }
              walletRefreshState
              return $ Left "No further action"
          | otherwise -> do
              writeIORef walletStateRef $ walletState{ selection = Nothing }
              walletRefreshState
              return $ Left "No further action"
        UiExport -> do
          case selection walletState >>= (accounts walletState ^?) . ix . fromIntegral of
            Nothing -> return $ Left "No account selected"
            Just Account{..} -> do
              cid <- flip commandIdToUI Nothing <$> newUnique
              putUiEvent . UiCommandResult cid . UiExportCommandResult . UiExportCommandSuccess . show $ accountSecretKey
              return $ Right cid
        _ -> case opToExpr walletState op of
          Left err   -> return $ Left err
          Right expr -> fmap Right $ putCommand (uiCommandHandle op) expr
    uiCommandHandle op commandId = KnitCommandHandle
      { putCommandResult = \mtid result ->
          whenJust (resultToUI result op) $ putUiEvent . UiCommandResult (commandIdToUI commandId mtid)
      , putCommandOutput = \_ _ ->
          return ()
      }

    optString key value = if null value then [] else [Knit.ArgKw key . Knit.ExprLit . Knit.toLit . Knit.LitString $ value]

    opToExpr WalletState{..} = \case
      UiBalance -> do
        account <- maybeToRight "No account selected" $ selection >>= (accounts ^?) . ix . fromIntegral
        Right $ Knit.ExprProcCall
          (Knit.ProcCall "get-balance"
            [Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitAddress . accountAddress $ account]
          )
      UiTxHistory -> do
        account <- maybeToRight "No account selected" $ selection >>= (accounts ^?) . ix . fromIntegral
        Right $ Knit.ExprProcCall
          (Knit.ProcCall "tx-history"
            [Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitAddress . accountAddress $ account]
          )
      UiSend UiSendArgs{..} -> do
        account <- maybeToRight "No account selected" $ selection >>= (accounts ^?) . ix . fromIntegral
        argOutputs <- forM usaOutputs $ \UiSendOutput{..} -> do
          argAddress <- addrFromText usoAddress
          argCoin <- readEither usoAmount
          Right $ Knit.ArgKw "out" . Knit.ExprProcCall $ Knit.ProcCall "tx-out"
            [ Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitAddress $ argAddress
            , Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitNumber $ argCoin
            ]
        Right $ Knit.ExprProcCall
          (Knit.ProcCall "send-tx" $
            [ Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitString . show . accountSecretKey $ account ] ++
            argOutputs ++
            optString "pass" usaPassphrase
          )
      UiKill commandId ->
        Right $ Knit.ExprProcCall
          (Knit.ProcCall Knit.killCommandName
            [Knit.ArgPos . Knit.ExprLit . Knit.toLit . Knit.LitTaskId . TaskId $ commandId]
          )
      UiNewWallet UiNewWalletArgs{..} -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall "gen-key-pair" $
            optString "name" unwaName ++
            optString "pass" unwaPassphrase
          )
      UiRestoreWallet UiRestoreWalletArgs{..} -> do
        Right $ Knit.ExprProcCall
          (Knit.ProcCall "restore-key" $
            [ Knit.ArgKw "secretkey" . Knit.ExprLit . Knit.toLit . Knit.LitString $ urwaMnemonic
            ] ++
            optString "name" urwaName ++
            optString "pass" urwaPassphrase
          )
      _ -> Left "Not implemented yet"

    resultToUI result = \case
      UiBalance{} ->
        Just . UiBalanceCommandResult . either UiBalanceCommandFailure UiBalanceCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueList l ->
              mapM fromValue l >>= \case
                [Knit.ValueCoin confirmed, Knit.ValueCoin total] ->
                  let diff = coinToInteger total - coinToInteger confirmed
                      sign = if diff < 0 then " - " else " + "
                      pending = if diff == 0 then ""
                                else sign <> pretty (abs diff) <> " pending \
                                     \= " <> pretty total
                  in Right $ pretty confirmed <> pending
                _ -> Left "Unrecognized return value"
            _ -> Left "Unrecognized return value"
      UiTxHistory{} ->
        Just . UiTxHistoryCommandResult . either UiTxHistoryCommandFailure UiTxHistoryCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueList l -> forM l $ fromValue >=> \case
              Knit.ValueTx txId inAddr inValue outs -> Right $
                UiTxHistoryRow (toHex txId) (pretty inValue) [UiTxHistoryRowPart (pretty inAddr) (pretty inValue)] $
                map (\TxOut{..} -> UiTxHistoryRowPart (pretty txOutAddr) (pretty txOutValue)) outs
              _ -> Left "Unrecognized return value"
            _ -> Left "Unrecognized return value"
      UiSend{} ->
        Just . UiSendCommandResult . either UiSendCommandFailure UiSendCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueString h -> Right h
            _ -> Left "Unrecognized return value"
      UiNewWallet{} ->
        Just . UiNewWalletCommandResult . either UiNewWalletCommandFailure UiNewWalletCommandSuccess $
          fromResult result >>= fromValue >>= \case
            Knit.ValueList l -> maybeToRight "Unrecognized return value" (l ^? ix 1) >>= fromValue >>= \case
              Knit.ValueCryptoKey k -> Right [Base64.encode k]
              _ -> Left "Unrecognized return value"
            _ -> Left "Unrecognized return value"
      UiRestoreWallet{} ->
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
-- Glue between the Wallet backend and Vty frontend
----------------------------------------------------------------------------

data WalletState = WalletState
  { selection :: Maybe Word
  , accounts  :: [Account]
  }

type WalletStateRef = IORef WalletState

createWalletState :: ComponentM WalletStateRef
createWalletState = buildComponent_ "WalletState" $ newIORef WalletState
  { selection = Nothing
  , accounts = []
  }

putWalletEventToUI :: WalletStateRef -> UiFace -> WalletEvent -> IO ()
putWalletEventToUI walletStateRef UiFace{..} ev = do
  walletState <- readIORef walletStateRef
  whenJust (walletEventToUI walletState ev) $ \(walletState', event) -> do
    writeIORef walletStateRef walletState'
    putUiEvent event

-- The 'Maybe' here is not used for now, but in the future might be, if some
-- event couldn't be mapped to a UI event.
walletEventToUI :: WalletState -> WalletEvent -> Maybe (WalletState, UiEvent)
walletEventToUI WalletState{ selection } = \case
    WalletStateUpdateEvent accounts ->
      Just $
        ( WalletState{ selection = clampSelection accounts, accounts = accounts }
        , UiWalletEvent $
            UiWalletUpdate
              { wuTrees = toTree <$> accounts
              , wuSelection = toTreeSelection accounts
              , wuSelectionInfo = toSelectionInfo accounts
              }
        )
  where
    clampSelection accounts = mfilter ((< length accounts) . fromIntegral) selection
    toTree Account{..} = Node
      { rootLabel = UiTreeItem
          { wtiLabel = Just $ fromMaybe (pretty accountAddress) accountName
          , wtiPath = []
          , wtiShowPath = False
          }
      , subForest = []
      }

    toTreeSelection accounts =
      clampSelection accounts >>=
      \i -> Just UiTreeSelection
        { wtsWalletIdx = i
        , wtsPath = []
        }

    toSelectionInfo accounts = do
      i <- clampSelection accounts
      Account{..} <- accounts ^? ix (fromIntegral i)
      Just $ UiSelectionWallet $ UiWalletInfo
        { uwiLabel = Just $ fromMaybe (pretty accountAddress) accountName
        , uwiId = pretty accountAddress
        , uwiWalletIdx = i
        , uwiBalance = Nothing
        , uwiAccounts = []
        }

----------------------------------------------------------------------------
-- Glue between command history and Vty frontend
----------------------------------------------------------------------------

historyToUI :: UiHistoryFace
historyToUI = UiHistoryFace
  { historyAddCommand = const $ return ()
  , historySetPrefix = const $ return ()
  , historyNextCommand = return Nothing
  , historyPrevCommand = return Nothing
  }
