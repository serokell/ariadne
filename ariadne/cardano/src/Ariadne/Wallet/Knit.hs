module Ariadne.Wallet.Knit
       ( Wallet

       , ComponentValue(..)
       , ComponentInflate(..)
       , ComponentLit(..)
       , ComponentToken(..)
       , ComponentTokenizer(..)
       , ComponentDetokenizer(..)
       , ComponentTokenToLit(..)
       , ComponentPrinter(..)
       , ComponentCommandRepr(..)
       , ComponentLitToValue(..)
       , ComponentExecContext(..)
       , ComponentCommandExec(..)
       , ComponentCommandProcs(..)

       , refreshStateCommandName
       , newAddressCommandName
       , newAccountCommandName
       , newWalletCommandName
       , restoreCommandName
       , restoreFromFileCommandName
       , selectCommandName
       , sendCommandName
       , sumCoinsCommamdName
       , balanceCommandName
       , renameCommandName
       , changePasswordCommandName
       , removeCommandName
       , feeCommandName

       , getWalletRefArg
       ) where

import Control.Lens (makePrisms)
import Serokell.Data.Memory.Units (fromBytes)

import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..))
import Pos.Client.Txp.Util (defaultInputSelectionPolicy)
import Pos.Core (AddressHash)
import Pos.Crypto (PublicKey, decodeAbstractHash)
import Pos.Crypto.Hashing (unsafeCheatingHashCoerce)
import Pos.Util.Util (toParsecError)
import qualified Text.Megaparsec.Char as P

import Ariadne.Cardano.Knit (Cardano, ComponentValue(..), tyCoin, tyTxOut)
import Ariadne.Cardano.Orphans ()
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Face
import Ariadne.Wallet.UiAdapter (formatAddressHash)

import Knit

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- Component type for Knit
data Wallet

type PAddressHash = AddressHash PublicKey

data instance ComponentValue _ Wallet
  = ValueInputSelectionPolicy !InputSelectionPolicy
  | ValueAddressHash PAddressHash
  deriving (Eq, Ord, Show)

makePrisms 'ValueInputSelectionPolicy

-- Name of input selection policy as used by Knit. We intentionally
-- don't use `Show` or `Buidable` or whatever else, so that it won't
-- accidentally change.
inputSelectionPolicyName :: IsString s => InputSelectionPolicy -> s
inputSelectionPolicyName =
    \case
        OptimizeForSecurity -> "security"
        OptimizeForHighThroughput -> "high-throughput"

instance
  ( Elem components Wallet
  ) => ComponentInflate components Wallet where
  componentInflate = \case
    ValueAddressHash ah -> ExprLit NoExt $ toLit (LitAddressHash ah)
    ValueInputSelectionPolicy policy ->
      let commandName = inputSelectionPolicyName policy
      in ExprProcCall NoExt $ ProcCall NoExt commandName []

data instance ComponentLit Wallet =
  LitAddressHash PAddressHash deriving (Eq, Ord, Show)

data instance ComponentToken Wallet =
  TokenAddressHash PAddressHash deriving (Eq, Ord, Show)

makePrisms 'TokenAddressHash

instance Elem components Wallet => ComponentTokenizer components Wallet where
  componentTokenizer =
    [ toToken . TokenAddressHash <$> pAddrHash ]
    where
      pAddrHash :: Tokenizer PAddressHash
      pAddrHash = do
        void $ P.string "#"
        str <- pSomeAlphaNum
        toParsecError $ decodeAbstractHash str

instance ComponentDetokenizer Wallet where
  componentTokenRender = \case
    TokenAddressHash h -> formatAddressHash h

instance Elem components Wallet => ComponentTokenToLit components Wallet where
  componentTokenToLit t = asum
    [ (toLit . LitAddressHash) <$> preview (_Token . uprism . _TokenAddressHash) t ]

instance ComponentPrinter Wallet where
  componentPpLit = \case
    LitAddressHash x -> text (componentTokenRender (TokenAddressHash x))
  componentPpToken = \case
    TokenAddressHash _ -> "AddressHash"

data instance ComponentCommandRepr components Wallet
  = CommandAction (WalletFace -> IO (Value components))

instance ComponentLitToValue components Wallet where
  componentLitToValue = \case
    LitAddressHash x -> ValueAddressHash x

data instance ComponentExecContext _ _ Wallet =
  WalletExecCtx WalletFace

instance MonadIO m => ComponentCommandExec m components Wallet where
  componentCommandExec (WalletExecCtx walletFace) (CommandAction act) =
    liftIO $ act walletFace

instance (Elem components Wallet, Elem components Core, Elem components Cardano) =>
         ComponentCommandProcs components Wallet where
  componentCommandProcs =
    [
      CommandProc
        { cpName = refreshStateCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pass
        , cpRepr = \() -> CommandAction $ \WalletFace{..} -> do
            walletRefreshState
            return $ toValue ValueUnit
        , cpHelp = "Internal function to update the UI."
        }
    , CommandProc
        { cpName = newAddressCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            accountRef <- getAccountRefArgOpt
            chain <- getArgOpt tyBool "external" <&>
              \case Nothing -> HdChainExternal
                    Just True -> HdChainExternal
                    Just False -> HdChainInternal
            pure (accountRef, chain)
        , cpRepr = \(accountRef, chain) -> CommandAction $ \WalletFace{..} -> do
            newAddr <- walletNewAddress accountRef chain
            return . toValue $ ValueAddress newAddr
        , cpHelp = "Generate and add a new address to the specified account. When \
                   \no account is specified, uses the selected account."
        }
    , CommandProc
        { cpName = newAccountCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            walletRef <- getWalletRefArgOpt
            name <- getArgOpt tyString "name"
            pure (walletRef, name)
        , cpRepr = \(walletRef, name) -> CommandAction $ \WalletFace{..} -> do
            walletNewAccount walletRef (AccountName <$> name)
            return $ toValue ValueUnit
        , cpHelp = "Create and add a new account to the specified wallet. When \
                   \no wallet is specified, uses the selected wallet."
        }
    , CommandProc
        { cpName = newWalletCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            name <- fmap WalletName <$> getArgOpt tyString "name"
            mbEntropySize <- (fmap . fmap) (fromBytes . toInteger) (getArgOpt tyInt "entropy-size")
            noConfirm <- getNoConfirmArgOpt
            return (name, mbEntropySize, noConfirm)
        , cpRepr = \(name, mbEntropySize, noConfirm) -> CommandAction $ \WalletFace{..} -> do
            mnemonic <- walletNewWallet noConfirm name mbEntropySize
            return $ toValue $ ValueList $ map (toValue . ValueString) mnemonic
        , cpHelp = "Generate a new wallet and add to the storage. \
                   \The result is the mnemonic to restore this wallet."
        }
    , CommandProc
        { cpName = restoreCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            name <- fmap WalletName <$> getArgOpt tyString "name"
            mnemonic <- Mnemonic <$> getArg tyString "mnemonic"
            return (name, mnemonic)
        , cpRepr = \(name, mnemonic) -> CommandAction $ \WalletFace{..} -> do
            toValue ValueUnit <$ walletRestore name mnemonic
        , cpHelp = "Restore a wallet from mnemonic. " <>
                   "A passphrase can be specified to encrypt the resulting " <>
                   "wallet (it doesn't have to be the same as the one used " <>
                   "to encrypt the old wallet)."
        }
    , CommandProc
        { cpName = restoreFromFileCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            name <- fmap WalletName <$> getArgOpt tyString "name"
            filename <- getArg tyFilePath "file"
            return (name, filename)
        , cpRepr = \(name, filename) -> CommandAction $ \WalletFace{..} ->
            toValue ValueUnit <$ walletRestoreFromFile name filename
        , cpHelp = "Restore a wallet from Daedalus secrets file"
        }
    , CommandProc
        { cpName = selectCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            walletRef <- getArgOpt tyWalletRef "wallet"
            path <- getArgMany tyWord "a" -- account or address
            return (walletRef, path)
        , cpRepr = \(walletRef, path) -> CommandAction $ \WalletFace{..} -> do
            walletSelect walletRef path
            return $ toValue ValueUnit
        , cpHelp = "Select a wallet, account, or address."
        }
    , CommandProc
        { cpName = sendCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            walletRef <- getWalletRefArgOpt
            accRefs <- getArgMany tyLocalAccountRef "account"
            outs <- getArgSome tyTxOut "out"
            isp <- fromMaybe defaultInputSelectionPolicy <$>
                getArgOpt tyInputSelectionPolicy "policy"
            noConfirm <- getNoConfirmArgOpt
            return (walletRef, accRefs, isp, outs, noConfirm)
        , cpRepr = \(walletRef, accRefs, isp, outs, noConfirm) -> CommandAction $
          \WalletFace{..} -> do
            txId <- walletSend noConfirm walletRef accRefs isp outs
            return . toValue . ValueHash . unsafeCheatingHashCoerce $ txId
        , cpHelp =
            "Send a transaction from the specified wallet. When no wallet \
            \is specified, uses the selected wallet. A list of accounts \
            \can be specified. Subset of these accounts will be used as \
            \inputs. If no account is specified, behavior depends on \
            \current selection. If an account in the input wallet is \
            \selected, only this account will be used as input. \
            \Otherwise, all accounts from the input wallet can be used \
            \as inputs. You can also specify a policy for input selection. \
            \By default the \"" <>
            inputSelectionPolicyName defaultInputSelectionPolicy <>
            "\" policy will be used."
        }
    , CommandProc
        { cpName = sumCoinsCommamdName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = getArgMany tyCoin "amount"
        , cpRepr = \amounts -> CommandAction $ \WalletFace{..} -> do
            toValue . ValueCoin <$> walletSumCoins amounts
        , cpHelp = "Calculate total sum of given coin amounts"
        }
    , CommandProc
        { cpName = feeCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            walletRef <- getWalletRefArgOpt
            accRefs <- getArgMany tyLocalAccountRef "account"
            outs <- getArgSome tyTxOut "out"
            isp <- fromMaybe defaultInputSelectionPolicy <$>
                getArgOpt tyInputSelectionPolicy "policy"
            return (walletRef, accRefs, isp, outs)
        , cpRepr = \(walletRef, accRefs, isp, outs) -> CommandAction $ \WalletFace{..} ->
            toValue . ValueCoin <$> walletFee walletRef accRefs isp outs
        , cpHelp =
            "Estimate a fee for a transaction. \
            \Parameters have the same meaning as the \"send\" command parameters."
        }
    , CommandProc
        { cpName = balanceCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pass
        , cpRepr = \() -> CommandAction $ \WalletFace{..} ->
            toValue . ValueCoin <$> walletBalance
        , cpHelp = "Get balance of the currently selected item"
        }
    , CommandProc
        { cpName = renameCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = getArg tyString "name"
        , cpRepr = \name -> CommandAction $ \WalletFace{..} -> do
            walletRename name
            return $ toValue ValueUnit
        , cpHelp = "Rename currently selected wallet or account"
        }
    , CommandProc
        { cpName = changePasswordCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pass
        , cpRepr = \() -> CommandAction $ \WalletFace{..} -> do
            walletChangePassword
            return $ toValue ValueUnit
        , cpHelp = "Change spending-password of currently selected wallet"
        }
    , CommandProc
        { cpName = removeCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = getNoConfirmArgOpt
        , cpRepr = \(noConfirm) -> CommandAction $ \WalletFace{..} -> do
            walletRemove noConfirm
            return $ toValue ValueUnit
        , cpHelp = "Remove currently selected item"
        }
    ] ++ map mkInputSelectionPolicyProc [minBound .. maxBound]
    where
      mkInputSelectionPolicyProc :: InputSelectionPolicy -> CommandProc components Wallet
      mkInputSelectionPolicyProc policy =
        let name :: IsString s => s
            name = inputSelectionPolicyName policy
        in CommandProc
          { cpName = name
          , cpArgumentPrepare = id
          , cpArgumentConsumer = pass
          , cpRepr = \() -> CommandAction $ \_ ->
              pure (toValue (ValueInputSelectionPolicy policy))
          , cpHelp =
              "The \"" <> name <> "\" input selection policy for \
              \transaction creation"
          }

----------------------------------------------------------------------------
-- Command names
----------------------------------------------------------------------------

refreshStateCommandName :: CommandId
refreshStateCommandName = "refresh-wallet-state"

newAddressCommandName :: CommandId
newAddressCommandName = "new-address"

newAccountCommandName :: CommandId
newAccountCommandName = "new-account"

newWalletCommandName :: CommandId
newWalletCommandName = "new-wallet"

restoreCommandName :: CommandId
restoreCommandName = "restore"

restoreFromFileCommandName :: CommandId
restoreFromFileCommandName = "restore-from-daedalus-file"

selectCommandName :: CommandId
selectCommandName = "select"

sendCommandName :: CommandId
sendCommandName = "send"

sumCoinsCommamdName :: CommandId
sumCoinsCommamdName = "sum-coins"

feeCommandName :: CommandId
feeCommandName = "fee"

balanceCommandName :: CommandId
balanceCommandName = "balance"

renameCommandName :: CommandId
renameCommandName = "rename"

changePasswordCommandName :: CommandId
changePasswordCommandName = "change-password"

removeCommandName :: CommandId
removeCommandName = "remove"

----------------------------------------------------------------------------
-- Type projections
----------------------------------------------------------------------------

tyWalletRef
  :: (Elem components Core, Elem components Wallet)
  => TyProjection components WalletReference
tyWalletRef =
    either WalletRefByUIindex (WalletRefByHdRootId . HdRootId . InDb) <$>
    tyWord `tyEither` tyAddressHash

tyAddressHash :: Elem components Wallet => TyProjection components PAddressHash
tyAddressHash = TyProjection "AddressHash" (preview _ValueAddressHash <=< fromValue)

tyLocalAccountRef ::
       Elem components Core => TyProjection components LocalAccountReference
tyLocalAccountRef = LocalAccountRefByIndex <$> tyWord

tyInputSelectionPolicy ::
       Elem components Wallet => TyProjection components InputSelectionPolicy
tyInputSelectionPolicy =
    TyProjection
        "InputSelectionPolicy"
        (preview _ValueInputSelectionPolicy <=< fromValue)

----------------------------------------------------------------------------
-- Other helpers
----------------------------------------------------------------------------

-- Maybe "wallet" shouldn't be hardcoded here, but currently it's
-- always "wallet", we can move it outside if it appears to be
-- necessary.
getWalletRefArgOpt
    :: (Elem components Core, Elem components Wallet)
    => ArgumentConsumer components WalletReference
getWalletRefArgOpt =
    fromMaybe WalletRefSelection <$> getArgOpt tyWalletRef "wallet"

getWalletRefArg
    :: (Elem components Core, Elem components Wallet)
    => ArgumentConsumer components WalletReference
getWalletRefArg = getArg tyWalletRef "wallet"

-- Maybe "account" shouldn't be hardcoded here, but currently it's
-- always "account", we can move it outside if it appears to be
-- necessary.
getAccountRefArgOpt ::
       (Elem components Core, Elem components Wallet) => ArgumentConsumer components AccountReference
getAccountRefArgOpt =
    convert <$> getWalletRefArgOpt <*>
    getArgOpt tyWord "account"
  where
    convert :: WalletReference -> Maybe Word -> AccountReference
    convert walletRef = \case
        Nothing -> AccountRefSelection
        Just e -> AccountRefByUIindex e walletRef

getNoConfirmArgOpt
    :: (Elem components Core)
    => ArgumentConsumer components Bool
getNoConfirmArgOpt = fromMaybe False <$> getArgOpt tyBool "no-confirm"
