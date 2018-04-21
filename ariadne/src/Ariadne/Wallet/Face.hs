module Ariadne.Wallet.Face
  ( module Ariadne.Cardano.Face
  , WalletFace(..)
  , WalletEvent(..)
  , WalletReference(..)
  , AccountReference(..)
  , WalletSelection(..)
  ) where

import Universum

import Ariadne.Cardano.Face
import Serokell.Data.Memory.Units (Byte)

data WalletSelection =
  WalletSelection
    { wsWalletIndex :: Word
    , wsPath :: [Word]
    }

data WalletReference
  = WalletRefSelection
  | WalletRefByIndex Word
  | WalletRefByName Text

data AccountReference
  = AccountRefSelection
  | AccountRefByIndex !Word32 !WalletReference
  | AccountRefByName !Text !WalletReference

data WalletFace =
  WalletFace
    { walletAddAddress :: AccountReference -> PassPhrase -> IO ()
    , walletAddAccount :: WalletReference -> Maybe Text -> IO ()
    , walletAddWallet :: PassPhrase -> Maybe Text -> Maybe Byte -> IO [Text]
    , walletRefreshUserSecret :: IO ()
    , walletSelect :: Maybe WalletReference -> [Word] -> IO ()
    , walletSend :: PassPhrase -> WalletReference -> NonEmpty TxOut -> IO TxId
    , walletSelection :: IO (Maybe WalletSelection)
    }

-- | Events as generated by the Wallet. They will be translated into
-- UI-compatible events in the 'Glue' module. They must be independent from the
-- UI and capture /what the backend can generate/, not what the frontend can
-- handle.
data WalletEvent =
  WalletUserSecretSetEvent UserSecret (Maybe WalletSelection)
