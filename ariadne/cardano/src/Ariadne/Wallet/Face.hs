module Ariadne.Wallet.Face
  ( module Ariadne.Cardano.Face
  , InputSelectionPolicy (..)

  , WalletFace(..)
  , WalletEvent(..)
  , ConfirmationType(..)
  , ConfirmSendInfo(..)
  , WalletReference(..)
  , AccountReference(..)
  , LocalAccountReference(..)
  , WalletSelection(..)
  , WalletName(..)
  , Mnemonic(..)
  , WalletRestoreType (..)
  , WalletUIFace(..)

  , SomeWalletPassException(..)
  , walletPassExceptionToException
  , walletPassExceptionFromException
  ) where

import Data.Typeable (cast)
import qualified GHC.Show as Show (Show (show))

import Serokell.Data.Memory.Units (Byte)

import Pos.Client.Txp.Util (InputSelectionPolicy(..))

import Ariadne.Cardano.Face
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (DB)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet

-- | Each *Id has a parent field, so it represents a path itself.
data WalletSelection
    = WSRoot !HdRootId
    | WSAccount !HdAccountId

data WalletReference
  = WalletRefSelection
  | WalletRefByHdRootId HdRootId
  -- UI indexation should be separated from backend, so it is better to have it in only
  -- in one place in 'select'. But `WalletRefByIndex` will be here for some time.
  -- Note: Add/remove wallets cause changes in indexation
  | WalletRefByUIindex Word

data AccountReference
  = AccountRefSelection
  | AccountRefByHdAccountId HdAccountId
  -- same as for WalletSelection:
  | AccountRefByUIindex !Word !WalletReference

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
    { walletNewAddress :: AccountReference -> HdAddressChain -> IO Address
    , walletNewAccount :: WalletReference -> Maybe AccountName -> IO ()
    , walletNewWallet :: Maybe WalletName -> Maybe Byte -> IO [Text]
    , walletRestore ::
        Maybe WalletName -> Mnemonic -> WalletRestoreType -> IO ()
    , walletRestoreFromFile ::
        Maybe WalletName -> FilePath -> WalletRestoreType -> IO ()
    , walletRename :: Text -> IO ()
    , walletRemove :: IO ()
    , walletRefreshState :: IO ()
    , walletSelect :: Maybe WalletReference -> [Word] -> IO ()
    , walletSend ::
        WalletReference -> [LocalAccountReference] ->
        InputSelectionPolicy -> NonEmpty TxOut -> IO TxId
    , walletGetSelection :: IO (Maybe WalletSelection, DB)
    , walletBalance :: IO Coin
    }

-- | Events as generated by the Wallet. They will be translated into
-- UI-compatible events in the 'Glue' module. They must be independent from the
-- UI and capture /what the backend can generate/, not what the frontend can
-- handle.
data WalletEvent
  = WalletStateSetEvent DB (Maybe WalletSelection)
  | WalletRequireConfirm (MVar Bool) ConfirmationType

data ConfirmationType
  = ConfirmMnemonic [Text]           -- ^ mnemonic
  | ConfirmRemove DB WalletSelection -- ^ walletDB, selection
  | ConfirmSend [ConfirmSendInfo]    -- ^ lists of outputs' info

data ConfirmSendInfo =
  ConfirmSendInfo
    { confirmSendAddress :: Text
    , confirmSendAmount  :: Text
    , confirmSendCoin    :: Text
    }

data WalletUIFace =
  WalletUIFace
    { walletGenerateMnemonic :: Byte -> IO [Text]
    , walletDefaultEntropySize :: Byte
    , walletValidateAddress :: Text -> Bool
    , walletCoinPrecision :: Int
    }

-- | Superclass for wallet password exceptions
data SomeWalletPassException =
  forall e . Exception e => SomeWalletPassException e

instance Show SomeWalletPassException where
  show (SomeWalletPassException e) = displayException e

instance Exception SomeWalletPassException

walletPassExceptionToException :: Exception e => e -> SomeException
walletPassExceptionToException = toException . SomeWalletPassException

walletPassExceptionFromException :: Exception e => SomeException -> Maybe e
walletPassExceptionFromException e = case onlyWalletPassFromException e of
    Just walletPassException -> Just walletPassException
    Nothing -> do
        SomeException a <- fromException e
        cast a
  where
    onlyWalletPassFromException :: Exception e => SomeException -> Maybe e
    onlyWalletPassFromException x = do
        SomeWalletPassException a <- fromException x
        cast a
