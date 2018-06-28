module Ariadne.Wallet.UiAdapter
  ( UiWalletData(..)
  , UiAccountData(..)
  , UiWalletSelection(..)
  , toUiWalletDatas
  , toUiWalletSelection
  ) where

import Universum

import qualified Data.Vector as V
import Pos.Core

import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet
import Ariadne.Wallet.Face

import Data.List.Index (indexed)

-- 'WalletData' like data type, used only in UI glue
data UiWalletData = UiWalletData
  { _uwdName     :: !Text
  , _uwdAccounts :: !(Vector UiAccountData)
  } deriving (Show, Generic)

data UiAccountData = UiAccountData
  { _uadName      :: !Text
  , _uadPath      :: !Word32
  -- Because of ChainType layer Now it should be ((HdAddressChain, Word32), Address) I guess, but
  -- AFAIU we don't want a new layer, so addresses of both types wiil be in one list -- External first.
  , _uadAddresses :: !(Vector (Word32, Address))
  } deriving (Eq, Show, Generic)

indexed_ :: (Integral i) => [a] -> [(i, a)]
indexed_ x = (mapFst fromIntegral) <$> (indexed x)
  where
    mapFst f (a, b) = (f a, b)

toUiWalletDatas :: DB -> [UiWalletData]
toUiWalletDatas db = toUiWalletData <$> walletList
  where
    -- Helpers
    wallets = (db ^. dbHdWallets)

    walletList :: [HdRoot]
    walletList = toList (readAllHdRoots wallets)

    accList :: HdRootId -> [HdAccount]
    accList rootId = toList $ getAccounts rootId

    getAccounts :: HdRootId -> IxSet HdAccount
    getAccounts rootId = fromRight
      (error "Bug: UnknownHdRoot")
      (readAccountsByRootId rootId wallets)

    -- External chain listed first
    addrList :: HdAccountId -> [HdAddress]
    addrList accId = toList $ getAddresses accId

    getAddresses :: HdAccountId -> IxSet HdAddress
    getAddresses accountId = fromRight
      (error "Bug: UnknownHdAccount")
      (readAddressesByAccountId accountId wallets)
    ---

    toUiWalletData :: HdRoot -> UiWalletData
    toUiWalletData HdRoot {..} = UiWalletData
      { _uwdName = _unWalletName _hdRootName
      , _uwdAccounts =
        let
          indexedAccounts :: Vector (Word32, HdAccount)
          indexedAccounts = V.fromList $ indexed_ $ accList _hdRootId
        in toUiAccountData <$> indexedAccounts
      }

    toUiAccountData :: (Word32, HdAccount) -> UiAccountData
    toUiAccountData (accIdx, HdAccount {..}) = UiAccountData
      { _uadName = _unAccountName _hdAccountName
      -- path indexation should be the same as in selection
      , _uadPath =  accIdx

      , _uadAddresses = map toUiAddresses (V.fromList $ indexed_ $ addrList _hdAccountId)
      }

    toUiAddresses :: (Word32, HdAddress) -> (Word32, Address)
    toUiAddresses (addrIx, HdAddress {..}) = (addrIx, _fromDb _hdAddressAddress)

data UiWalletSelection = UiWalletSelection
  { uwsWalletIdx :: Word
  , uwsPath :: [Word]
  }

toUiWalletSelection :: DB -> WalletSelection -> UiWalletSelection
toUiWalletSelection db WalletSelection{..} = case wsPath of
  RootPath rootId ->
    UiWalletSelection (getHdRootIdx rootId) []
  AccountPath accountId ->
    let
      parentRootId = accountId ^. hdAccountIdParent
    in
      UiWalletSelection (getHdRootIdx parentRootId) [(getAccountIdx accountId parentRootId)]
  AddressPath addressId ->
    let
      parentAccountId = addressId ^. hdAddressIdParent
      parentRootId = parentAccountId ^. hdAccountIdParent
    in
      UiWalletSelection
        (getHdRootIdx parentRootId)
        [(getAccountIdx parentAccountId parentRootId), (getAddressIdx addressId parentAccountId)]
  where
    wallets = (db ^. dbHdWallets)

    walletList :: [(Word, HdRoot)]
    walletList = indexed_ $ toList (readAllHdRoots wallets)

    -- Selection always exist
    getAccountList parentRootId = indexed_ . toList $
      fromRight
        (error "Bug: parentRootId does not exist")
        (readAccountsByRootId parentRootId (db ^. dbHdWallets))

    getAddressList parentAccountId = indexed_ . toList $
      fromRight
        (error "Bug: parentAccountId does not exist")
        (readAddressesByAccountId parentAccountId (db ^. dbHdWallets))

    getHdRootIdx rootId = fst $ fromMaybe
      (error "Bug: selected Wallet does not exist.")
      (mbHead $ filter (\(_, wal) -> wal ^. hdRootId == rootId) walletList)

    getAccountIdx accountId parentRootId = fst $ fromMaybe
      (error "Bug: selected Account does not exist.")
      (mbHead $ filter (\(_, acc) -> acc ^. hdAccountId == accountId) (getAccountList parentRootId))

    getAddressIdx addressId parentAccountId = fst $ fromMaybe
      (error "Bug: selected Address does not exist.")
      (mbHead $ filter (\(_, addr) -> addr ^. hdAddressId == addressId) (getAddressList parentAccountId))

    mbHead :: [a] -> Maybe a
    mbHead (x:_) = Just x
    mbHead [] = Nothing
