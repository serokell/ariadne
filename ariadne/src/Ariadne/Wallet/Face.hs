module Ariadne.Wallet.Face
  ( module Ariadne.Cardano.Face
  , InputSelectionPolicy (..)

  , WalletFace(..)
  , WalletEvent(..)
  , WalletReference(..)
  , AccountReference(..)
  , LocalAccountReference(..)
  , WalletSelection(..)
  , WalletName(..)
  , Mnemonic(..)
  , WalletRestoreType (..)
  , HdPath (..)
  ) where

import Universum

import Serokell.Data.Memory.Units (Byte)

import Pos.Client.Txp.Util (InputSelectionPolicy(..))

import Ariadne.Cardano.Face
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (DB)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet

-- TODO: remove redundant wsPath
data WalletSelection =
  WalletSelection { wsPath :: HdPath }

-- | Each *Id has a parent field, so it represents a path itself.
data HdPath
  = RootPath HdRootId
  | AccountPath HdAccountId
  | AddressPath HdAddressId
  deriving (Eq)

data WalletReference
  = WalletRefSelection
  -- UI indexation should be separated from backend, so it is better to have it in only
  -- in one place in 'select'. But `WalletRefByIndex` will be here for some time.
  -- Note: Add/remove wallets cause changes in indexation
  | WalletRefByUIindex Word
  | WalletRefByHdRootId HdRootId

data AccountReference
  = AccountRefSelection
  | AccountRefByHdAccountId HdAccountId
  -- same as for WalletSelection:
  | AccountRefByUIindex !Word32 !WalletReference

-- | Reference to an account inside a wallet.
data LocalAccountReference
  = LocalAccountRefByIndex !Word
  -- ^ Reference by index in UI.

-- | Single string representing a mnemonic, presumably space-separated
-- list of words.
newtype Mnemonic = Mnemonic
    { unMnemonic :: Text
    }

-- | We support various types of wallet restoration.
data WalletRestoreType
    = WalletRestoreQuick
    -- ^ Quickly restore a wallet without restoring its accounts and addresses.
    | WalletRestoreFull
    -- ^ Restore a wallet fully, find our accounts and addresses.

data WalletFace =
  WalletFace
    { walletNewAddress :: AccountReference -> HdAddressChain -> PassPhrase -> IO ()
    , walletNewAccount :: WalletReference -> Maybe AccountName -> IO ()
    , walletNewWallet :: PassPhrase -> Maybe WalletName -> Maybe Byte -> IO [Text]
    , walletRestore ::
        PassPhrase -> Maybe WalletName -> Mnemonic -> WalletRestoreType -> IO ()
    , walletRestoreFromFile ::
        Maybe WalletName -> FilePath -> WalletRestoreType -> IO ()
    , walletRename :: Text -> IO ()
    , walletRemove :: IO ()
    , walletRefreshState :: IO ()
    , walletSelect :: Maybe WalletReference -> [Word] -> IO ()
    , walletSend ::
        PassPhrase -> WalletReference -> [LocalAccountReference] ->
        InputSelectionPolicy -> NonEmpty TxOut -> IO TxId
    , walletGetSelection :: IO (Maybe WalletSelection, UserSecret)
    , walletBalance :: IO Coin
    }

-- | Events as generated by the Wallet. They will be translated into
-- UI-compatible events in the 'Glue' module. They must be independent from the
-- UI and capture /what the backend can generate/, not what the frontend can
-- handle.
data WalletEvent =
  WalletStateSetEvent DB (Maybe WalletSelection)
