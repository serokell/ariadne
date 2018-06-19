-- | HD wallets
module Ariadne.Wallet.Cardano.Kernel.DB.HdWallet (
    -- * Supporting types
    WalletName(..)
  , AccountName(..)
  , HdAccountIx(..)
  , HdAddressIx(..)
  , AssuranceLevel(..)
  , HasSpendingPassword(..)
    -- * HD wallet types proper
  , HdWallets(..)
  , HdRootId(..)
  , HdAccountId(..)
  , HdAddressChain(..)
  , HdAddressId(..)
  , HdRoot(..)
  , HdAccount(..)
  , HdAddress(..)
    -- ** Initialiser
  , initHdWallets
    -- ** Lenses
  , hdWalletsRoots
  , hdWalletsAccounts
  , hdWalletsAddresses
  , hdAccountIdParent
  , hdAccountIdIx
  , hdAddressIdParent
  , hdAddressIdIx
  , hdRootId
  , hdRootName
  , hdRootHasPassword
  , hdRootAssurance
  , hdRootCreatedAt
  , hdAccountId
  , hdAccountName
  , hdAccountCurrentCheckpoint
  , hdAccountCheckpoints
  , hdAddressId
  , hdAddressAddress
  , hdAddressIsUsed
  , hdAddressChain
  , hdAddressCheckpoints
    -- ** Composite lenses
  , hdAccountRootId
  , hdAddressRootId
  , hdAddressAccountId
    -- * Unknown identifiers
  , UnknownHdRoot(..)
  , UnknownHdAccount(..)
  , UnknownHdAddress(..)
  , embedUnknownHdRoot
  , embedUnknownHdAccount
    -- * Zoom to parts of the HD wallet
  , zoomHdRootId
  , zoomHdAccountId
  , zoomHdAddressId
    -- * Zoom variations that create on request
  , zoomOrCreateHdRoot
  , zoomOrCreateHdAccount
  , zoomOrCreateHdAddress
  , assumeHdRootExists
  , assumeHdAccountExists
  ) where

import Universum

import Control.Lens (at)
import Control.Lens.TH (makeLenses)
import qualified Data.IxSet.Typed as IxSet
import Data.SafeCopy (base, deriveSafeCopySimple)

import qualified Data.Text.Buildable
import Formatting (bprint, build, (%))

import qualified Pos.Core as Core
import qualified Pos.Crypto as Core

import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Spec
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet

{-------------------------------------------------------------------------------
  Supporting types
-------------------------------------------------------------------------------}

-- | Wallet name
newtype WalletName = WalletName Text

-- | Account name
newtype AccountName = AccountName Text

-- | Account index
newtype HdAccountIx = HdAccountIx Word32
  deriving (Eq, Ord)

-- | Whether the chain is an external or an internal one
data HdAddressChain = HdChainExternal | HdChainInternal
  deriving (Eq, Ord)

-- | Address index
newtype HdAddressIx = HdAddressIx Word32
  deriving (Eq, Ord)

-- | Wallet assurance level
--
-- TODO: document what these levels mean (in particular, how it does translate
-- to the depth required before a transaction is marked as Persisted?)
data AssuranceLevel =
    AssuranceLevelNormal
  | AssuranceLevelStrict

-- | Does this wallet have a spending password
data HasSpendingPassword =
    -- | No spending password set
    NoSpendingPassword

    -- | If there is a spending password, we record when it was last updated.
  | HasSpendingPassword (InDb Core.Timestamp)

deriveSafeCopySimple 1 'base ''WalletName
deriveSafeCopySimple 1 'base ''AccountName
deriveSafeCopySimple 1 'base ''HdAccountIx
deriveSafeCopySimple 1 'base ''HdAddressChain
deriveSafeCopySimple 1 'base ''HdAddressIx
deriveSafeCopySimple 1 'base ''AssuranceLevel
deriveSafeCopySimple 1 'base ''HasSpendingPassword

{-------------------------------------------------------------------------------
  HD wallets
-------------------------------------------------------------------------------}

-- | HD wallet root ID
data HdRootId = HdRootId (InDb (Core.AddressHash Core.PublicKey))
  deriving (Eq, Ord)

-- | HD wallet account ID
data HdAccountId = HdAccountId {
      _hdAccountIdParent :: HdRootId
    , _hdAccountIdIx     :: HdAccountIx
    }
  deriving (Eq, Ord)

-- | HD wallet address ID
data HdAddressId = HdAddressId {
      _hdAddressIdParent :: HdAccountId
    , _hdAddressIdIx     :: HdAddressIx
    }
  deriving (Eq, Ord)

-- | Root of a HD wallet
--
-- The wallet has sequentially assigned account indices and randomly assigned
-- address indices.
--
-- NOTE: We do not store the encrypted key of the wallet.
--
-- TODO: synchronization state
data HdRoot = HdRoot {
      -- | Wallet ID
      _hdRootId          :: HdRootId

      -- | Wallet name
    , _hdRootName        :: WalletName

      -- | Does this wallet have a spending password?
      --
      -- NOTE: We do not store the spending password itself, but merely record
      -- whether there is one. Updates to the spending password affect only the
      -- external key storage, not the wallet DB proper.
    , _hdRootHasPassword :: HasSpendingPassword

      -- | Assurance level
    , _hdRootAssurance   :: AssuranceLevel

      -- | When was this wallet created?
    , _hdRootCreatedAt   :: InDb Core.Timestamp
    }

-- | Account in a HD wallet
--
-- Key derivation is cheap
data HdAccount = HdAccount {
      -- | Account index
      _hdAccountId          :: HdAccountId

      -- | Account name
    , _hdAccountName        :: AccountName

      -- | Part of the wallet state pertaining to this account,
      -- as stipulated by the wallet specification
    , _hdAccountCheckpoints :: NonEmpty AccCheckpoint
    }

-- | Address in an account of a HD wallet
data HdAddress = HdAddress {
      -- | Address ID
      _hdAddressId      :: HdAddressId

      -- | The actual address
    , _hdAddressAddress :: InDb Core.Address

      -- | Has this address been involved in a transaction?
      --
      -- TODO: How is this determined? What is the definition? How is it set?
      -- TODO: This will likely move to the 'BlockMeta' instead.
    , _hdAddressIsUsed  :: Bool

      -- | Whether this address is derived on the external or internal chain.
      -- Invariant: this must match the actual derivation scheme which
      -- yields _hdAddressAddress.
    , _hdAddressChain   :: HdAddressChain

      -- | Part of the wallet state pertaining to this address,
      -- as stipulated by the wallet specification
    , _hdAddressCheckpoints :: NonEmpty AddrCheckpoint
    }

makeLenses ''HdAccountId
makeLenses ''HdAddressId

makeLenses ''HdRoot
makeLenses ''HdAccount
makeLenses ''HdAddress

deriveSafeCopySimple 1 'base ''HdRootId
deriveSafeCopySimple 1 'base ''HdAccountId
deriveSafeCopySimple 1 'base ''HdAddressId

deriveSafeCopySimple 1 'base ''HdRoot
deriveSafeCopySimple 1 'base ''HdAccount
deriveSafeCopySimple 1 'base ''HdAddress

{-------------------------------------------------------------------------------
  Derived lenses
-------------------------------------------------------------------------------}

hdAccountRootId :: Lens' HdAccount HdRootId
hdAccountRootId = hdAccountId . hdAccountIdParent

hdAddressAccountId :: Lens' HdAddress HdAccountId
hdAddressAccountId = hdAddressId . hdAddressIdParent

hdAddressRootId :: Lens' HdAddress HdRootId
hdAddressRootId = hdAddressAccountId . hdAccountIdParent

hdAccountCurrentCheckpoint :: Lens' HdAccount AccCheckpoint
hdAccountCurrentCheckpoint = hdAccountCheckpoints . currentAccCheckpoint

{-------------------------------------------------------------------------------
  Unknown identifiers
-------------------------------------------------------------------------------}

-- | Unknown root
data UnknownHdRoot =
    -- | Unknown root ID
    UnknownHdRoot HdRootId

-- | Unknown account
data UnknownHdAccount =
    -- | Unknown root ID
    UnknownHdAccountRoot HdRootId

    -- | Unknown account (implies the root is known)
  | UnknownHdAccount HdAccountId

-- | Unknown address
data UnknownHdAddress =
    -- | Unknown root ID
    UnknownHdAddressRoot HdRootId

    -- | Unknown account (implies the root is known)
  | UnknownHdAddressAccount HdAccountId

    -- | Unknown address (implies the account is known)
  | UnknownHdAddress HdAddressId

embedUnknownHdRoot :: UnknownHdRoot -> UnknownHdAccount
embedUnknownHdRoot = go
  where
    go (UnknownHdRoot rootId) = UnknownHdAccountRoot rootId

embedUnknownHdAccount :: UnknownHdAccount -> UnknownHdAddress
embedUnknownHdAccount = go
  where
    go (UnknownHdAccountRoot rootId) = UnknownHdAddressRoot rootId
    go (UnknownHdAccount accountId)  = UnknownHdAddressAccount accountId

deriveSafeCopySimple 1 'base ''UnknownHdRoot
deriveSafeCopySimple 1 'base ''UnknownHdAddress
deriveSafeCopySimple 1 'base ''UnknownHdAccount

{-------------------------------------------------------------------------------
  IxSet instantiations
-------------------------------------------------------------------------------}

instance HasPrimKey HdRoot where
    type PrimKey HdRoot = HdRootId
    primKey = _hdRootId

instance HasPrimKey HdAccount where
    type PrimKey HdAccount = HdAccountId
    primKey = _hdAccountId

instance HasPrimKey HdAddress where
    type PrimKey HdAddress = HdAddressId
    primKey = _hdAddressId

type HdRootIxs    = '[]
type HdAccountIxs = '[HdRootId]
type HdAddressIxs = '[HdRootId, HdAccountId, Core.Address]

type instance IndicesOf HdRoot    = HdRootIxs
type instance IndicesOf HdAccount = HdAccountIxs
type instance IndicesOf HdAddress = HdAddressIxs

instance IxSet.Indexable (HdRootId ': HdRootIxs)
                         (OrdByPrimKey HdRoot) where
    indices = ixList

instance IxSet.Indexable (HdAccountId ': HdAccountIxs)
                         (OrdByPrimKey HdAccount) where
    indices = ixList
                (ixFun ((:[]) . view hdAccountRootId))

instance IxSet.Indexable (HdAddressId ': HdAddressIxs)
                         (OrdByPrimKey HdAddress) where
    indices = ixList
                (ixFun ((:[]) . view hdAddressRootId))
                (ixFun ((:[]) . view hdAddressAccountId))
                (ixFun ((:[]) . view (hdAddressAddress . fromDb)))

{-------------------------------------------------------------------------------
  Top-level HD wallet structure
-------------------------------------------------------------------------------}

-- | All wallets, accounts and addresses in the HD wallets
--
-- We use a flat "relational" structure rather than nested maps so that we can
-- go from address to wallet just as easily as the other way around.
data HdWallets = HdWallets {
    _hdWalletsRoots     :: IxSet HdRoot
  , _hdWalletsAccounts  :: IxSet HdAccount
  , _hdWalletsAddresses :: IxSet HdAddress
  }

deriveSafeCopySimple 1 'base ''HdWallets
makeLenses ''HdWallets

initHdWallets :: HdWallets
initHdWallets = HdWallets emptyIxSet emptyIxSet emptyIxSet

{-------------------------------------------------------------------------------
  Zoom to existing parts of a HD wallet
-------------------------------------------------------------------------------}

zoomHdRootId :: forall e a.
                (UnknownHdRoot -> e)
             -> HdRootId
             -> Update' HdRoot e a -> Update' HdWallets e a
zoomHdRootId embedErr rootId =
    zoomDef err (hdWalletsRoots . at rootId)
  where
    err :: Update' HdWallets e a
    err = throwError $ embedErr (UnknownHdRoot rootId)

zoomHdAccountId :: forall e a.
                   (UnknownHdAccount -> e)
                -> HdAccountId
                -> Update' HdAccount e a -> Update' HdWallets e a
zoomHdAccountId embedErr accId =
    zoomDef err (hdWalletsAccounts . at accId)
  where
    err :: Update' HdWallets e a
    err = zoomHdRootId embedErr' (accId ^. hdAccountIdParent) $
            throwError $ embedErr (UnknownHdAccount accId)

    embedErr' :: UnknownHdRoot -> e
    embedErr' = embedErr . embedUnknownHdRoot

zoomHdAddressId :: forall e a.
                   (UnknownHdAddress -> e)
                -> HdAddressId
                -> Update' HdAddress e a -> Update' HdWallets e a
zoomHdAddressId embedErr addrId =
    zoomDef err (hdWalletsAddresses . at addrId)
  where
    err :: Update' HdWallets e a
    err = zoomHdAccountId embedErr' (addrId ^. hdAddressIdParent) $
            throwError $ embedErr (UnknownHdAddress addrId)

    embedErr' :: UnknownHdAccount -> e
    embedErr' = embedErr . embedUnknownHdAccount

{-------------------------------------------------------------------------------
  Zoom to parts of the wallet, creating them if they don't exist
-------------------------------------------------------------------------------}

-- | Variation on 'zoomHdRootId' that creates the 'HdRoot' if it doesn't exist
--
-- Precondition: @newRoot ^. hdRootId == rootId@
zoomOrCreateHdRoot :: HdRoot
                   -> HdRootId
                   -> Update' HdRoot    e a
                   -> Update' HdWallets e a
zoomOrCreateHdRoot newRoot rootId upd =
    zoomCreate newRoot (hdWalletsRoots . at rootId) $ upd

-- | Variation on 'zoomHdAccountId' that creates the 'HdAccount' if it doesn't exist
--
-- Precondition: @newAccount ^. hdAccountId == accountId@
zoomOrCreateHdAccount :: (HdRootId -> Update' HdWallets e ())
                      -> HdAccount
                      -> HdAccountId
                      -> Update' HdAccount e a
                      -> Update' HdWallets e a
zoomOrCreateHdAccount checkRootExists newAccount accId upd = do
    checkRootExists $ accId ^. hdAccountIdParent
    zoomCreate newAccount (hdWalletsAccounts . at accId) $ upd

-- | Variation on 'zoomHdAddressId' that creates the 'HdAddress' if it doesn't exist
--
-- Precondition: @newAddress ^. hdAddressId == AddressId@
zoomOrCreateHdAddress :: (HdAccountId -> Update' HdWallets e ())
                      -> HdAddress
                      -> HdAddressId
                      -> Update' HdAddress e a
                      -> Update' HdWallets e a
zoomOrCreateHdAddress checkAccountExists newAddress addrId upd = do
    checkAccountExists $ addrId ^. hdAddressIdParent
    zoomCreate newAddress (hdWalletsAddresses . at addrId) $ upd

-- | Assume that the given HdRoot exists
--
-- Helper function which can be used as an argument to 'zoomOrCreateHdAccount'
assumeHdRootExists :: HdRootId -> Update' HdWallets e ()
assumeHdRootExists _id = return ()

-- | Assume that the given HdAccount exists
--
-- Helper function which can be used as an argument to 'zoomOrCreateHdAddress'
assumeHdAccountExists :: HdAccountId -> Update' HdWallets e ()
assumeHdAccountExists _id = return ()

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Buildable HdRootId where
    build (HdRootId keyInDb)
        = bprint ("HdRootId: "%build) (_fromDb keyInDb)

instance Buildable HdAccountIx where
    build (HdAccountIx ix)
        = bprint ("HdAccountIx: "%build) ix

instance Buildable HdAccountId where
    build (HdAccountId parentId accountIx)
        = bprint ("HdAccountId: "%build%", "%build) parentId accountIx

instance Buildable HdAddressIx where
    build (HdAddressIx ix)
        = bprint ("HdAddressIx: "%build) ix

instance Buildable HdAddressId where
    build (HdAddressId parentId addressIx)
        = bprint ("HdAddressId: "%build%", "%build) parentId addressIx

instance Buildable UnknownHdAccount where
    build (UnknownHdAccountRoot rootId)
        = bprint ("UnknownHdAccountRoot: "%build) rootId
    build (UnknownHdAccount accountId)
        = bprint ("UnknownHdAccount accountId: "%build) accountId
