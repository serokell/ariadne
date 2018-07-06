module Ariadne.Wallet.UiAdapter
  ( UiWalletData(..)
  , UiAccountData(..)
  , UiWalletSelection(..)
  -- * Lens
  , uwdAccounts
  , uadAddresses
  --
  , toUiWalletDatas
  , toUiWalletSelection
  ) where

import Universum

import qualified Data.Vector as V

import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Read
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet
import Ariadne.Wallet.Face

import Control.Lens (makeLenses)
import Serokell.Util (enumerate)

-- 'WalletData' like data type, used only in UI glue
data UiWalletData = UiWalletData
  { _uwdName     :: !Text
  , _uwdId       :: !HdRootId
  , _uwdAccounts :: !(Vector UiAccountData)
  } deriving (Show, Generic)

data UiAccountData = UiAccountData
  { _uadName      :: !Text
  , _uadPath      :: !Word32
  -- Because of ChainType layer Now it should be ((HdAddressChain, Word32), Address) I guess, but
  -- AFAIU we don't want a new layer, so addresses of both types wiil be in one list -- External first.
  , _uadAddresses :: !(Vector (Word32, Address))
  } deriving (Eq, Show, Generic)

makeLenses 'UiWalletData
makeLenses 'UiAccountData

toUiWalletDatas :: DB -> [UiWalletData]
toUiWalletDatas db = toUiWalletData <$> walletList
  where
    -- Helpers
    wallets = db ^. dbHdWallets

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
      { _uwdName = unWalletName _hdRootName
      , _uwdId = _hdRootId
      , _uwdAccounts =
        let
          indexedAccounts :: Vector (Word32, HdAccount)
          indexedAccounts = V.fromList $ enumerate  $ accList _hdRootId
        in toUiAccountData <$> indexedAccounts
      }

    toUiAccountData :: (Word32, HdAccount) -> UiAccountData
    toUiAccountData (accIdx, HdAccount {..}) = UiAccountData
      { _uadName = unAccountName _hdAccountName
      -- path indexation should be the same as in selection
      , _uadPath =  accIdx

      , _uadAddresses = map toUiAddresses (V.fromList $ enumerate  $ addrList _hdAccountId)
      }

    toUiAddresses :: (Word32, HdAddress) -> (Word32, Address)
    toUiAddresses (addrIx, HdAddress {..}) = (addrIx, _fromDb _hdAddressAddress)

data UiWalletSelection = UiWalletSelection
  { uwsWalletIdx :: Word
  , uwsPath :: [Word]
  }

toUiWalletSelection :: DB -> WalletSelection -> UiWalletSelection
toUiWalletSelection db selection = case selection of
  WSRoot rootId ->
    UiWalletSelection (getHdRootIdx rootId) []
  WSAccount accountId ->
    let
      parentRootId = accountId ^. hdAccountIdParent
    in
      UiWalletSelection (getHdRootIdx parentRootId) [(getAccountIdx accountId parentRootId)]
  where
    wallets = db ^. dbHdWallets

    walletList :: [(Word, HdRoot)]
    walletList = enumerate  $ toList (readAllHdRoots wallets)

    -- Selection always exist
    getAccountList parentRootId = enumerate  . toList $
      fromRight
        (error "Bug: parentRootId does not exist")
        (readAccountsByRootId parentRootId (db ^. dbHdWallets))

    getHdRootIdx rootId = fst $ fromMaybe
      (error "Bug: selected Wallet does not exist.")
      (find (\(_, wal) -> wal ^. hdRootId == rootId) walletList)

    getAccountIdx accountId parentRootId = fst $ fromMaybe
      (error "Bug: selected Account does not exist.")
      (find (\(_, acc) -> acc ^. hdAccountId == accountId) (getAccountList parentRootId))
