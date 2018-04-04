module Ariadne.Wallet.Knit where

import IiExtras
import Text.Earley
import Universum

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

data instance ComponentExecContext _ Wallet =
  WalletExecCtx WalletFace

instance MonadIO m => ComponentCommandExec m components Wallet where
  componentCommandExec (WalletExecCtx walletFace) (CommandAction act) =
    liftIO $ act walletFace

instance (Elem components Wallet, Elem components Core) => ComponentCommandProcs components Wallet where
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
        { cpName = "add-account"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            walletRef <-
              maybe WalletRefSelection (either WalletRefByName WalletRefByIndex) <$>
              getArgOpt (tyString `tyEither` tyWord) "wallet"
            name <- fromMaybe "new account" <$> getArgOpt tyString "name"
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
        , cpArgumentConsumer =
            fromMaybe "new wallet" <$> getArgOpt tyString "name"
        , cpRepr = \name -> CommandAction $ \WalletFace{..} -> do
            walletAddWallet name
            return $ toValue ValueUnit
        , cpHelp = "Create a new wallet."
        }
    , CommandProc
        { cpName = "select"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            walletRef <-
              either WalletRefByName WalletRefByIndex <$>
              getArg (tyString `tyEither` tyWord) "wallet"
            path <- getArgMany tyWord "a" -- account or address
            return (walletRef, path)
        , cpRepr = \(walletRef, path) -> CommandAction $ \WalletFace{..} -> do
            walletSelect (Just walletRef) path
            return $ toValue ValueUnit
        , cpHelp = "Select a wallet, account, or address."
        }
    ]
