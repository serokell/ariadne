module Ariadne.Wallet.UiAdapter
  ( UiWalletData(..)
  , UiAccountData(..)
  , UiWalletSelection(..)
  , UiAddressData(..)
  -- * Lens
  , uwdName
  , uwdId
  , uwdAccounts
  , uwdBalance

  , uadName
  , uadAccountIdx
  , uadAddresses
  , uadBalance

  , uiadAddress
  , uiadAddressIdx
  , uiadBalance
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

import Pos.Core (unsafeIntegerToCoin)
import Pos.Crypto.Hashing (AbstractHash(..))
import Serokell.Util (enumerate)

import Control.Lens (makeLenses)

-- 'WalletData' like data type, used only in UI glue
data UiWalletData = UiWalletData
  { _uwdName     :: !Text
  , _uwdId       :: !HdRootId
  , _uwdAccounts :: !(Vector UiAccountData)
  , _uwdBalance  :: !Coin
  } deriving (Show, Generic)

data UiAccountData = UiAccountData
  { _uadName :: !Text
  -- It is the UI indexation path, with sequential indexation without gaps.
  -- Removing an account (except the last) will change all accounts indexes.
  , _uadAccountIdx :: !Word32
  -- AFAIU we don't want a new ChainType layer, so addresses of both types wiil be in one list -- External first.
  , _uadAddresses :: !(Vector UiAddressData)
  , _uadBalance  :: !Coin
  } deriving (Eq, Show, Generic)

data UiAddressData = UiAddressData
  { _uiadAddress :: !Address
  -- It is the UI indexation path, with sequential indexation without gaps.
  , _uiadAddressIdx :: !Word32
  , _uiadBalance :: !Coin
  } deriving (Eq, Show)

makeLenses 'UiWalletData
makeLenses 'UiAccountData
makeLenses 'UiAddressData

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
    toUiWalletData HdRoot{..} = UiWalletData
      { _uwdName = unWalletName _hdRootName
      , _uwdId = _hdRootId
      , _uwdAccounts =
        let
          indexedAccounts :: Vector (Word32, HdAccount)
          indexedAccounts = V.fromList $ enumerate  $ accList _hdRootId
        in toUiAccountData <$> indexedAccounts
      , _uwdBalance =
          unsafeIntegerToCoin $ hdRootBalance _hdRootId wallets
      }

    toUiAccountData :: (Word32, HdAccount) -> UiAccountData
    toUiAccountData (accIdx, acc@HdAccount{..}) = UiAccountData
      { _uadName = unAccountName _hdAccountName
      , _uadAccountIdx =  accIdx
      , _uadAddresses = map toUiAddresses (V.fromList $ enumerate $ addrList _hdAccountId)
      , _uadBalance = hdAccountBalance acc
      }

    toUiAddresses :: (Word32, HdAddress) -> UiAddressData
    toUiAddresses (addrIx, addr@HdAddress{..}) = addressData
      where
        addressData = UiAddressData
          { _uiadAddress = _fromDb _hdAddressAddress
          , _uiadAddressIdx = addrIx
          , _uiadBalance = hdAddressBalance addr}

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
