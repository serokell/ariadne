-- | This module contains functions that are needed
-- to check consistency of wallets and key storage.

module Ariadne.Wallet.Cardano.Kernel.Consistency
       ( getUnknownKeys
       , getUnknownWallets
       ) where

import Data.List ((\\))
import qualified Data.Map as Map
import Fmt (blockListF, (+|), (|+))

import Ariadne.Wallet.Cardano.Kernel (PassiveWallet, getWalletSnapshot)
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (dbHdWallets)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (HdRoot, HdRootId, WalletName, hdRootId, hdRootName)
import Ariadne.Wallet.Cardano.Kernel.Keystore (Keystore)
import Ariadne.Wallet.Cardano.Kernel.Types (WalletId(..))

import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read as HDRead
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet as IxSet
import qualified Ariadne.Wallet.Cardano.Kernel.Keystore as Keystore

-- | Checks keystore for unknown keys and return list of them.
getUnknownKeys :: MonadIO m => PassiveWallet -> Keystore -> m [HdRootId]
getUnknownKeys pwallet keystore = do
  (keystoreRootIds,walletRootIds,_) <- getRootIdsFromWalletAndKeyfile pwallet keystore
  return $ (keystoreRootIds \\ walletRootIds)

-- | Returns list of wallets without corresponding key in keystore.
getUnknownWallets
  :: MonadIO m => PassiveWallet -> Keystore -> m ([WalletName], [HdRootId])
getUnknownWallets pwallet keystore = do
  (keystoreRootIds,walletRootIds, wallets) <- getRootIdsFromWalletAndKeyfile pwallet keystore
  let rootIDsWithMissedSecretKeys = walletRootIds \\ keystoreRootIds
  let eitherWalletsWithMissedSecretKeys =
        mapM (flip IxSet.getTheOnly wallets) rootIDsWithMissedSecretKeys
  let walletNamesWithMissedSecretKeys = case eitherWalletsWithMissedSecretKeys of
        Right wallet -> view hdRootName <$> wallet
        Left  (brokenRootId, brokenWallets) ->
                 error $ "There are more (or less) than one wallet with the same HdRootId: \n \
                         \Broken RootId: " +|brokenRootId|+ "\n" <>
                         "Names of wallets with this id: " <>
                         blockListF (view hdRootName <$> IxSet.toList brokenWallets)
  return (walletNamesWithMissedSecretKeys,rootIDsWithMissedSecretKeys)

-- | helper function. It returns list of HdRootIds from keystore and from wallets database.
-- And also ixset of all HdRoots
getRootIdsFromWalletAndKeyfile
  :: MonadIO m
  => PassiveWallet
  -> Keystore
  ->  m ([HdRootId], [HdRootId], IxSet.IxSet HdRoot)
getRootIdsFromWalletAndKeyfile pwallet keystore = do
  snapshot <- liftIO $ getWalletSnapshot pwallet
  let wallets = HDRead.readAllHdRoots (snapshot ^. dbHdWallets)
  let walletRootIds = view hdRootId <$> IxSet.toList wallets
  (keystoreRootIds) <- liftIO $ Map.keys <$> Keystore.toMap keystore
  return (getHdRootId <$> keystoreRootIds, walletRootIds, wallets)
  where
    getHdRootId :: WalletId -> HdRootId
    getHdRootId (WalletIdHdSeq rootId) = rootId
