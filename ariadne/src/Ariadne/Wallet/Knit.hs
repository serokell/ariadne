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

data instance ComponentExecContext Wallet =
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
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandAction $ \WalletFace{..} -> do
           walletAddAccount
           return $ toValue ValueUnit
        , cpHelp = "Add an account to the specified wallet. When no wallet \
                   \is specified, uses the selected wallet."
        }
    , CommandProc
        { cpName = "select"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = do
            wsaWalletIndex <- getArg tyWord "wallet"
            wsaPath <- getArgMany tyWord "a" -- account or address
            return WalletSelectByIndex{..}
        , cpRepr = \wsa -> CommandAction $ \WalletFace{..} -> do
            walletSelect wsa
            return $ toValue ValueUnit
        , cpHelp = "Select a wallet by index."
        }
    ]
