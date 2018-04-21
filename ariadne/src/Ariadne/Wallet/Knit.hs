module Ariadne.Wallet.Knit where

import Universum

import qualified Data.ByteArray as ByteArray

import IiExtras
import Pos.Crypto.Hashing (hashRaw, unsafeCheatingHashCoerce)
import Pos.Crypto.Signing (emptyPassphrase)
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
        { cpName = "add-address"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = (,) <$> getAccountRefArgOpt <*> getPassPhraseArg
        , cpRepr = \(accountRef, passphrase) -> CommandAction $ \WalletFace{..} -> do
            walletAddAddress accountRef passphrase
            return $ toValue ValueUnit
        , cpHelp = "Add an account to the specified wallet. When no wallet \
                   \is specified, uses the selected wallet."
        }
    , CommandProc
        { cpName = "add-account"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            walletRef <- getWalletRefArgOpt
            name <- getArgOpt tyString "name"
            pure (walletRef, name)
        , cpRepr = \(walletRef, name) -> CommandAction $ \WalletFace{..} -> do
            walletAddAccount walletRef name
            return $ toValue ValueUnit
        , cpHelp = "Add an account to the specified wallet. When no wallet \
                   \is specified, uses the selected wallet."
        }
    , CommandProc
        { cpName = "add-wallet"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            passPhrase <- getPassPhraseArg
            name <- getArgOpt tyString "name"
            return (passPhrase, name)
        , cpRepr = \(passPhrase, name) -> CommandAction $ \WalletFace{..} -> do
            mnemonic <- walletAddWallet passPhrase name
            return $ toValue $ ValueList $ map (toValue . ValueString) mnemonic
        , cpHelp = "Create a new wallet. \
                   \The result is the mnemonic to restore this wallet."
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
    either WalletRefByName WalletRefByIndex <$> tyString `tyEither` tyWord

mkPassPhrase :: Maybe Text -> PassPhrase
mkPassPhrase = maybe emptyPassphrase (ByteArray.convert . hashRaw . encodeUtf8)

getPassPhraseArg :: Elem components Core => ArgumentConsumer components PassPhrase
getPassPhraseArg = mkPassPhrase <$> getArgOpt tyString "pass"
