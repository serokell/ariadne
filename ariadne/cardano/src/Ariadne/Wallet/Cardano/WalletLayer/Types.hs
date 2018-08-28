module Ariadne.Wallet.Cardano.WalletLayer.Types
    ( PassiveWalletLayer (..)
    ) where

import Universum

import qualified Ariadne.Wallet.Cardano.Kernel.Accounts as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Addresses as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.DB.AcidState as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet as Kernel
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet (IxSet)
import qualified Ariadne.Wallet.Cardano.Kernel.PrefilterTx as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Wallets as Kernel

import Pos.Block.Types (Blund)
import Pos.Core.Chrono (NE, NewestFirst(..), OldestFirst(..))
import Pos.Crypto (EncryptedSecretKey, PassPhrase)

------------------------------------------------------------
-- Passive wallet layer
------------------------------------------------------------

-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    { -- * wallets
      pwlCreateWallet   :: EncryptedSecretKey
                        -> Kernel.HasNonemptyPassphrase
                        -> Kernel.AssuranceLevel
                        -> Kernel.WalletName
                        -> Map Kernel.HdAccountId Kernel.PrefilteredUtxo
                        -> m (Either Kernel.CreateWalletError Kernel.HdRoot)
    , pwlGetWalletIds   :: m (IxSet Kernel.HdRoot)
    , pwlGetWallet      :: Kernel.HdRootId -> m (Either Kernel.UnknownHdRoot Kernel.HdRoot)
    , pwlUpdateWallet   :: Kernel.HdRootId
                        -> Kernel.AssuranceLevel
                        -> Kernel.WalletName
                        -> m (Either Kernel.UnknownHdRoot Kernel.HdRoot)
    , pwlDeleteWallet   :: Kernel.HdRootId -> m (Either Kernel.UnknownHdRoot ())
    -- * accounts
    , pwlCreateAccount  :: Kernel.HdRootId
                        -> Kernel.AccountName
                        -> m (Either Kernel.CreateAccountError Kernel.HdAccount)
    , pwlGetAccounts    :: Kernel.HdRootId -> m (Either Kernel.UnknownHdRoot (IxSet Kernel.HdAccount))
    , pwlGetAccount     :: Kernel.HdAccountId -> m (Either Kernel.UnknownHdAccount Kernel.HdAccount)
    , pwlUpdateAccount  :: Kernel.HdAccountId
                        -> Kernel.AccountName
                        -> m (Either Kernel.UnknownHdAccount Kernel.HdAccount)
    , pwlDeleteAccount  :: Kernel.HdAccountId -> m (Either Kernel.UnknownHdAccount ())
    -- * addresses
    , pwlCreateAddress  :: PassPhrase
                        -> Kernel.HdAccountId
                        -> Kernel.HdAddressChain
                        -> m (Either Kernel.CreateAddressError Kernel.HdAddress)
    , pwlGetAddresses   :: Kernel.HdRootId -> m (Either Kernel.UnknownHdRoot (IxSet Kernel.HdAddress))
    -- * core API
    , pwlApplyBlocks    :: OldestFirst NE Blund -> m ()
    , pwlRollbackBlocks :: NewestFirst NE Blund -> m ()
    -- * internal, hopefully it will go in the future
    , pwlGetDBSnapshot  :: m Kernel.DB
    }
