module Ariadne.Wallet.Knit where

import Universum

import qualified Data.ByteArray as ByteArray

import IiExtras
import Pos.Core (unsafeGetCoin)
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
        { cpName = "refresh-user-secret"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandAction $ \WalletFace{..} -> do
            walletRefreshUserSecret
            return $ toValue ValueUnit
        , cpHelp = "Internal function to update the UI."
        }
    , CommandProc
        { cpName = "new-address"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = (,) <$> getAccountRefArgOpt <*> getPassPhraseArg
        , cpRepr = \(accountRef, passphrase) -> CommandAction $ \WalletFace{..} -> do
            walletNewAddress accountRef passphrase
            return $ toValue ValueUnit
        , cpHelp = "Generate and add a new address to the specified account. When \
                   \no account is specified, uses the selected account."
        }
    , CommandProc
        { cpName = "new-account"
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
        { cpName = "new-wallet"
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
        { cpName = "restore"
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
        { cpName = "select"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            walletRef <- getWalletRefArg
            path <- getArgMany tyWord "a" -- account or address
            return (walletRef, path)
        , cpRepr = \(walletRef, path) -> CommandAction $ \WalletFace{..} -> do
            walletSelect (Just walletRef) path
            return $ toValue ValueUnit
        , cpHelp = "Select a wallet, account, or address."
        }
    , CommandProc
        { cpName = "send"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            walletRef <- getWalletRefArgOpt
            passPhrase <- getPassPhraseArg
            outs <- getArgSome tyTxOut "out"
            return (walletRef, passPhrase, outs)
        , cpRepr = \(walletRef, passPhrase, outs) -> CommandAction $ \WalletFace{..} -> do
            txId <- walletSend passPhrase walletRef outs
            return . toValue . ValueHash . unsafeCheatingHashCoerce $ txId
        , cpHelp = "Send a transaction from the specified wallet. When no wallet \
                   \is specified, uses the selected wallet."
        }
    , CommandProc
        { cpName = "balance"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandAction $ \WalletFace{..} ->
            toValue . ValueNumber . fromIntegral . unsafeGetCoin <$> walletBalance
        , cpHelp = "Get balance of the currently selected item"
        }
    ]

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

mkPassPhrase :: Maybe Text -> PassPhrase
mkPassPhrase = maybe emptyPassphrase (ByteArray.convert . hashRaw . encodeUtf8)

getPassPhraseArg :: Elem components Core => ArgumentConsumer components PassPhrase
getPassPhraseArg = mkPassPhrase <$> getArgOpt tyString "pass"
