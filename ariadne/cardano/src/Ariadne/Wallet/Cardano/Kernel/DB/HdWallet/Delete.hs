{-# LANGUAGE AllowAmbiguousTypes #-}

-- | DELETE operatiosn on HD wallets
module Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Delete (
    DeleteHdAccountError(..)
  , DeleteHdRootError(..)
  , deleteHdRoot
  , deleteHdAccount
  ) where

import Control.Lens ((%=))
import Data.SafeCopy (base, deriveSafeCopySimple)

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet

{-------------------------------------------------------------------------------
  DELETE
-------------------------------------------------------------------------------}

-- | Delete a wallet with the whole subtree (addresses and accounts).
deleteHdRoot :: HdRootId -> Update' HdWallets DeleteHdRootError ()
deleteHdRoot rootId = do
  zoomHdRootId DeleteUnknownHdRoot rootId $ pass
  hdWalletsAddresses %= deleteIxAll rootId
  hdWalletsAccounts  %= deleteIxAll rootId
  hdWalletsRoots     %= deleteIxAll rootId

-- | Delete an account with its addresses.
deleteHdAccount :: HdAccountId -> Update' HdWallets DeleteHdAccountError ()
deleteHdAccount accId = do
  zoomHdAccountId DeleteUnknownHdAccount accId $ pass
  hdWalletsAddresses %= deleteIxAll accId
  hdWalletsAccounts  %= deleteIxAll accId


{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Errors thrown by 'deleteHdWallet'
data DeleteHdRootError =
    -- | We don't have a wallet with the specified ID
    DeleteUnknownHdRoot UnknownHdRoot
    deriving (Eq, Show)

-- | Errors thrown by 'deleteHdAccount'
data DeleteHdAccountError =
    -- | The specified account could not be found
    DeleteUnknownHdAccount UnknownHdAccount
    deriving (Eq, Show)

instance Exception DeleteHdRootError where
  displayException (DeleteUnknownHdRoot (UnknownHdRoot rootId)) =
    toString $ "The wallet '" <> pretty rootId <> "' does not exist."

instance Exception DeleteHdAccountError where
  displayException (DeleteUnknownHdAccount (UnknownHdAccount accountId)) =
    toString $ "The account '" <> pretty accountId <> "' does not exist."
  displayException (DeleteUnknownHdAccount (UnknownHdAccountRoot rootId)) =
    toString $ "The corresponding wallet '" <> pretty rootId <> "' does not exist."

deriveSafeCopySimple 1 'base ''DeleteHdRootError
deriveSafeCopySimple 1 'base ''DeleteHdAccountError
