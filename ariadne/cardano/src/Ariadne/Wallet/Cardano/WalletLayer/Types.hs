module Ariadne.Wallet.Cardano.WalletLayer.Types
       ( PassiveWalletLayer (..)
       ) where

import qualified Ariadne.Wallet.Cardano.Kernel.Accounts as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Addresses as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.DB.AcidState as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Delete as Kernel
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet (IxSet)
import qualified Ariadne.Wallet.Cardano.Kernel.PrefilterTx as Kernel
import qualified Ariadne.Wallet.Cardano.Kernel.Wallets as Kernel

import Pos.Block.Types (Blund)
import Pos.Core (Coin)
import Pos.Core.Chrono (NE, NewestFirst(..), OldestFirst(..))
import Pos.Crypto (EncryptedSecretKey, PassPhrase)

type Mnemonic = [Text]

------------------------------------------------------------
-- Passive wallet layer
------------------------------------------------------------

-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    { -- | wallets
      pwlCreateWallet
          :: EncryptedSecretKey
          -> Kernel.HasNonemptyPassphrase
          -> Kernel.CreateWithAddress
          -> Kernel.AssuranceLevel
          -> Kernel.WalletName
          -> Map Kernel.HdAccountId Kernel.PrefilteredUtxo
          -> m (Either Kernel.CreateWalletError Kernel.HdRoot)
    , pwlGetWalletIds
          :: m (IxSet Kernel.HdRoot)
    , pwlGetWallet
          :: Kernel.HdRootId
          -> m (Either Kernel.UnknownHdRoot Kernel.HdRoot)
    , pwlUpdateWalletName
          :: Kernel.HdRootId
          -> Kernel.WalletName
          -> m (Either Kernel.UnknownHdRoot Kernel.HdRoot)
    , pwlUpdateWalletAssurance
          :: Kernel.HdRootId
          -> Kernel.AssuranceLevel
          -> m (Either Kernel.UnknownHdRoot Kernel.HdRoot)
    , pwlGetAccountBalance
          :: Kernel.HdAccountId
          -> m Coin
    , pwlGetRootBalance
          :: Kernel.HdRootId
          -> m Coin
    , pwlDeleteWallet
          :: Kernel.HdRootId
          -> m (Either Kernel.DeleteHdRootError ())

    -- | accounts
    , pwlCreateAccount
          :: Kernel.HdRootId
          -> Kernel.AccountName
          -> m (Either Kernel.CreateAccountError Kernel.HdAccount)
    , pwlGetAccounts
          :: Kernel.HdRootId
          -> m (Either Kernel.UnknownHdRoot (IxSet Kernel.HdAccount))
    , pwlGetAccount
          :: Kernel.HdAccountId
          -> m (Either Kernel.UnknownHdAccount Kernel.HdAccount)
    , pwlUpdateAccountName
          :: Kernel.HdAccountId
          -> Kernel.AccountName
          -> m (Either Kernel.UnknownHdAccount Kernel.HdAccount)
    , pwlDeleteAccount
          :: Kernel.HdAccountId
          -> m (Either Kernel.DeleteHdAccountError ())

    -- | addresses
    , pwlCreateAddress
          :: PassPhrase
          -> Kernel.HdAccountId
          -> Kernel.HdAddressChain
          -> m (Either Kernel.CreateAddressError Kernel.HdAddress)
    , pwlGetAddresses
          :: Kernel.HdRootId
          -> m (Either Kernel.UnknownHdRoot (IxSet Kernel.HdAddress))

    -- | core API
    , pwlApplyBlocks
          :: OldestFirst NE Blund
          -> m ()
    , pwlRollbackBlocks
          :: NewestFirst NE Blund
          -> m ()

    -- | generate EncryptedSecretKey
    , pwlCreateEncryptedKey
        :: PassPhrase
        -> Mnemonic
        -> m EncryptedSecretKey

    -- | internal, hopefully these will go in the future
    , pwlGetDBSnapshot
          :: m Kernel.DB
    , pwlLookupKeystore
          :: Kernel.HdRootId
          -> m (Maybe EncryptedSecretKey)
    }
