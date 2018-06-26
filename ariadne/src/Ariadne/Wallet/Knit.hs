module Ariadne.Wallet.Knit where

import Universum

import qualified Data.ByteArray as ByteArray

import IiExtras
import Pos.Crypto.Hashing (hashRaw, unsafeCheatingHashCoerce)
import Pos.Crypto.Signing (emptyPassphrase)
import Serokell.Data.Memory.Units (fromBytes)
import Text.Earley

import Ariadne.Cardano.Knit (Cardano, ComponentValue(..), tyTxOut)
import Ariadne.Wallet.Face

import Knit

-- Component type for Knit
data Wallet

data instance ComponentValue _ Wallet

deriving instance Eq (ComponentValue components Wallet)
deriving instance Ord (ComponentValue components Wallet)
deriving instance Show (ComponentValue components Wallet)

instance ComponentInflate components Wallet where
  componentInflate = \case{}

data instance ComponentLit Wallet

deriving instance Eq (ComponentLit Wallet)
deriving instance Ord (ComponentLit Wallet)
deriving instance Show (ComponentLit Wallet)

data instance ComponentToken Wallet

deriving instance Eq (ComponentToken Wallet)
deriving instance Ord (ComponentToken Wallet)
deriving instance Show (ComponentToken Wallet)

instance ComponentTokenizer components Wallet where
  componentTokenizer = []

instance ComponentDetokenizer Wallet where
  componentTokenRender = \case{}

instance ComponentLitGrammar components Wallet where
  componentLitGrammar = rule empty

instance ComponentPrinter Wallet where
  componentPpLit = \case{}
  componentPpToken = \case{}

data instance ComponentCommandRepr components Wallet
  = CommandAction (WalletFace -> IO (Value components))

instance ComponentLitToValue components Wallet where
  componentLitToValue = \case{}

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
        { cpName = refreshUserSecretCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandAction $ \WalletFace{..} -> do
            walletRefreshUserSecret
            return $ toValue ValueUnit
        , cpHelp = "Internal function to update the UI."
        }
    , CommandProc
        { cpName = newAddressCommandName
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = (,) <$> getAccountRefArgOpt <*> getPassPhraseArg
        , cpRepr = \(accountRef, passphrase) -> CommandAction $ \WalletFace{..} -> do
            walletNewAddress accountRef passphrase
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
            walletNewAccount walletRef name
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
            return (walletRef, accRefs, passPhrase, outs)
        , cpRepr = \(walletRef, accRefs, passPhrase, outs) -> CommandAction $
          \WalletFace{..} -> do
            txId <- walletSend passPhrase walletRef accRefs outs
            return . toValue . ValueHash . unsafeCheatingHashCoerce $ txId
        , cpHelp =
            "Send a transaction from the specified wallet. When no wallet \
            \is specified, uses the selected wallet. A list of accounts \
            \can be specified. Subset of these accounts will be used as \
            \inputs. If no account is specified, behavior depends on \
            \current selection. If an account in the input wallet is \
            \selected, only this account will be used as input. \
            \Otherwise, all accounts from the input wallet can be used \
            \as inputs."
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
    ]

refreshUserSecretCommandName :: CommandId
refreshUserSecretCommandName = "refresh-user-secret"

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

-- Maybe "wallet" shouldn't be hardcoded here, but currently it's
-- always "wallet", we can move it outside if it appears to be
-- necessary.
getWalletRefArgOpt ::
       Elem components Core => ArgumentConsumer components WalletReference
getWalletRefArgOpt =
    fromMaybe WalletRefSelection <$> getArgOpt tyWalletRef "wallet"

getWalletRefArg ::
       Elem components Core => ArgumentConsumer components WalletReference
getWalletRefArg = getArg tyWalletRef "wallet"

-- Maybe "account" shouldn't be hardcoded here, but currently it's
-- always "account", we can move it outside if it appears to be
-- necessary.
getAccountRefArgOpt ::
       Elem components Core => ArgumentConsumer components AccountReference
getAccountRefArgOpt =
    convert <$> getWalletRefArgOpt <*>
    getArgOpt (tyString `tyEither` tyWord32) "account"
  where
    convert :: WalletReference -> Maybe (Either Text Word32) -> AccountReference
    convert walletRef = \case
        Nothing -> AccountRefSelection
        Just e -> (either
            (flip AccountRefByName walletRef)
            (flip AccountRefByIndex walletRef)
                  ) e


tyWalletRef :: Elem components Core => TyProjection components WalletReference
tyWalletRef =
    either (WalletRefByName . WalletName) WalletRefByIndex <$>
    tyString `tyEither` tyWord

tyLocalAccountRef ::
       Elem components Core => TyProjection components LocalAccountReference
tyLocalAccountRef =
    either LocalAccountRefByName LocalAccountRefByIndex <$>
    tyString `tyEither` tyWord

mkPassPhrase :: Maybe Text -> PassPhrase
mkPassPhrase = maybe emptyPassphrase (ByteArray.convert . hashRaw . encodeUtf8)

getPassPhraseArg :: Elem components Core => ArgumentConsumer components PassPhrase
getPassPhraseArg = mkPassPhrase <$> getArgOpt tyString "pass"
