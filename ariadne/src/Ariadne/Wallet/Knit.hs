module Ariadne.Wallet.Knit where

import Universum hiding (preview)

import qualified Data.ByteArray as ByteArray

import Control.Lens hiding (parts, (<&>))
import Formatting (sformat, (%))
import Serokell.Data.Memory.Units (fromBytes)
import Text.Earley

import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..))
import Pos.Client.Txp.Util (defaultInputSelectionPolicy)
import Pos.Core (AddressHash)
import Pos.Crypto (PassPhrase, PublicKey, decodeAbstractHash)
import Pos.Crypto.Hashing (hashHexF, hashRaw, unsafeCheatingHashCoerce)
import Pos.Crypto.Signing (emptyPassphrase)
import Pos.Util.Util (toParsecError)
import qualified Text.Megaparsec.Char as P

import Ariadne.Cardano.Knit (Cardano, ComponentValue(..), tyTxOut)
import Ariadne.Cardano.Orphans ()
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Face
import IiExtras

import Knit

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
    ValueAddressHash ah -> ExprLit $ toLit (LitAddressHash ah)
    ValueInputSelectionPolicy policy ->
      let commandName = inputSelectionPolicyName policy
      in ExprProcCall $ ProcCall commandName []

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
    TokenAddressHash h -> sformat ("#"%hashHexF) h

instance Elem components Wallet => ComponentLitGrammar components Wallet where
  componentLitGrammar = rule $ asum
    [ toLit . LitAddressHash <$> tok (_Token . uprismElem . _TokenAddressHash) ]

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
        , cpArgumentConsumer = pure ()
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
            passphrase <- getPassPhraseArg
            pure (accountRef, chain, passphrase)
        , cpRepr = \(accountRef, chain, passphrase) -> CommandAction $ \WalletFace{..} -> do
            walletNewAddress accountRef chain passphrase
            return $ toValue ValueUnit
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
            passPhrase <- getPassPhraseArg
            name <- fmap WalletName <$> getArgOpt tyString "name"
            mbEntropySize <- (fmap . fmap) (fromBytes . toInteger) (getArgOpt tyInt "entropy-size")
            return (passPhrase, name, mbEntropySize)
        , cpRepr = \(passPhrase, name, mbEntropySize) -> CommandAction $ \WalletFace{..} -> do
            mnemonic <- walletNewWallet passPhrase name mbEntropySize
            return $ toValue $ ValueList $ map (toValue . ValueString) mnemonic
        , cpHelp = "Generate a new wallet and add to the storage. \
                   \The result is the mnemonic to restore this wallet."
        }
    , CommandProc
        { cpName = restoreCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            passPhrase <- getPassPhraseArg
            name <- fmap WalletName <$> getArgOpt tyString "name"
            mnemonic <- Mnemonic <$> getArg tyString "mnemonic"
            restoreType <- getArg tyBool "full" <&>
                \case False -> WalletRestoreQuick
                      True -> WalletRestoreFull
            return (passPhrase, name, mnemonic, restoreType)
        , cpRepr = \(passPhrase, name, mnemonic, restoreType) -> CommandAction $ \WalletFace{..} ->
            toValue ValueUnit <$ walletRestore passPhrase name mnemonic restoreType
        , cpHelp = "Restore a wallet from mnemonic. " <>
                   "A passphrase can be specified to encrypt the resulting " <>
                   "wallet (it doesn't have to be the same as the one used " <>
                   "to encrypt the old wallet). " <>
                   "There are two types of restoration: full restoration " <>
                   "finds all used addresses (and their accounts), but is " <>
                   "slow, while quick restoration only adds a wallet with " <>
                   "secret key derived from the specified mnemonic."
        }
    , CommandProc
        { cpName = restoreFromFileCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            name <- fmap WalletName <$> getArgOpt tyString "name"
            filename <- getArg tyFilePath "file"
            restoreType <- getArg tyBool "full" <&>
                \case False -> WalletRestoreQuick
                      True -> WalletRestoreFull
            return (name, filename, restoreType)
        , cpRepr = \(name, filename, restoreType) -> CommandAction $ \WalletFace{..} ->
            toValue ValueUnit <$ walletRestoreFromFile name filename restoreType
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
            passPhrase <- getPassPhraseArg
            outs <- getArgSome tyTxOut "out"
            isp <- fromMaybe defaultInputSelectionPolicy <$>
                getArgOpt tyInputSelectionPolicy "policy"
            return (walletRef, accRefs, passPhrase, isp, outs)
        , cpRepr = \(walletRef, accRefs, passPhrase, isp, outs) -> CommandAction $
          \WalletFace{..} -> do
            txId <- walletSend passPhrase walletRef accRefs isp outs
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
        { cpName = balanceCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pure ()
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
        { cpName = removeCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandAction $ \WalletFace{..} -> do
            walletRemove
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
          , cpArgumentConsumer = pure ()
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

balanceCommandName :: CommandId
balanceCommandName = "balance"

renameCommandName :: CommandId
renameCommandName = "rename"

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
getWalletRefArgOpt ::
       (Elem components Core, Elem components Wallet) => ArgumentConsumer components WalletReference
getWalletRefArgOpt =
    fromMaybe WalletRefSelection <$> getArgOpt tyWalletRef "wallet"

getWalletRefArg ::
       (Elem components Core, Elem components Wallet) => ArgumentConsumer components WalletReference
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

mkPassPhrase :: Maybe Text -> PassPhrase
mkPassPhrase = maybe emptyPassphrase (ByteArray.convert . hashRaw . encodeUtf8)

getPassPhraseArg :: Elem components Core => ArgumentConsumer components PassPhrase
getPassPhraseArg = mkPassPhrase <$> getArgOpt tyString "pass"
