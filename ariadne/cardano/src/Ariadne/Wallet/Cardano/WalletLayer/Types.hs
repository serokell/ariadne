module Ariadne.Wallet.Cardano.WalletLayer.Types
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
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
import Pos.Client.Txp.Util (InputSelectionPolicy)
import Pos.Core.Chrono (NE, NewestFirst(..), OldestFirst(..))
import Pos.Core.Txp (TxId, TxOut)
import Pos.Crypto (EncryptedSecretKey, PassPhrase)

------------------------------------------------------------
-- Passive wallet layer
------------------------------------------------------------

-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    {
    -- * wallets
      _pwlCreateWallet   :: EncryptedSecretKey
                         -> Kernel.HasNonemptyPassphrase
                         -> Kernel.AssuranceLevel
                         -> Kernel.WalletName
                         -> Map Kernel.HdAccountId Kernel.PrefilteredUtxo
                         -> m (Either Kernel.CreateWalletError Kernel.HdRoot)
    , _pwlGetWalletIds   :: m (IxSet Kernel.HdRoot)
    , _pwlGetWallet      :: Kernel.HdRootId -> m (Either Kernel.UnknownHdRoot Kernel.HdRoot)
    , _pwlUpdateWallet   :: Kernel.HdRootId
                         -> Kernel.AssuranceLevel
                         -> Kernel.WalletName
                         -> m (Either Kernel.UnknownHdRoot Kernel.HdRoot)
    , _pwlDeleteWallet   :: Kernel.HdRootId -> m (Either Kernel.UnknownHdRoot ())
    -- * accounts
    , _pwlCreateAccount  :: Kernel.HdRootId
                         -> Maybe Kernel.AccountName
                         -> m (Either Kernel.CreateAccountError Kernel.HdAccount)
    , _pwlGetAccounts    :: Kernel.HdRootId -> m (Either Kernel.UnknownHdRoot (IxSet Kernel.HdAccount))
    , _pwlGetAccount     :: Kernel.HdAccountId -> m (Either Kernel.UnknownHdAccount Kernel.HdAccount)
    , _pwlUpdateAccount  :: Kernel.HdAccountId
                         -> Kernel.AccountName
                         -> m (Either Kernel.UnknownHdAccount Kernel.HdAccount)
    , _pwlDeleteAccount  :: Kernel.HdAccountId -> m (Either Kernel.UnknownHdAccount ())
    -- * addresses
    , _pwlCreateAddress  :: PassPhrase
                         -> Kernel.HdAccountId
                         -> Kernel.HdAddressChain
                         -> m (Either Kernel.CreateAddressError Kernel.HdAddress)
    , _pwlGetAddresses   :: Kernel.HdRootId -> m (Either Kernel.UnknownHdRoot (IxSet Kernel.HdAddress))
    -- * core API
    , _pwlApplyBlocks    :: OldestFirst NE Blund -> m ()
    , _pwlRollbackBlocks :: NewestFirst NE Blund -> m ()
    -- * internal, hopefully it will go in the future
    , _pwlGetDBSnapshot  :: m Kernel.DB
    }

------------------------------------------------------------
-- Active wallet layer
------------------------------------------------------------

-- An active wallet layer. See @ActiveWallet@.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet layer
      walletPassiveLayer :: PassiveWalletLayer m

      -- | Performs a payment.
    , pay :: Kernel.HdAccountId
          -> NonEmpty TxOut
          -> InputSelectionPolicy
          -> PassPhrase
          -> m TxId
          -- ^ Return type should be IO (Either SendTxException TxId), but current
          -- implementation just throws an IO exception.
    }
