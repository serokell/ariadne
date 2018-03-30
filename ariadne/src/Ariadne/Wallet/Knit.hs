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
  = CommandAction (WalletFace -> CardanoMode (Value components))

instance ComponentLitToValue components Wallet where
  componentLitToValue = \case{}

data instance ComponentExecContext Wallet =
  WalletExecCtx WalletFace (CardanoMode ~> IO)

instance MonadIO m => ComponentCommandExec m components Wallet where
  componentCommandExec
    (WalletExecCtx walletFace runCardanoMode)
    (CommandAction act) =
      liftIO $ runCardanoMode (act walletFace)

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
        , cpHelp = "Internal function to update the UI"
        }
    , CommandProc
        { cpName = "add-random-key"
        , cpArgumentPrepare = identity
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandAction $ \WalletFace{..} -> do
           walletAddRandomKey
           walletRefreshUserSecret
           return $ toValue ValueUnit
        , cpHelp = ""
        }
    ]
