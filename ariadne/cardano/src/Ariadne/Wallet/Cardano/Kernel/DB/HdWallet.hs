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
  , hdAddressIdChain
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
  , hdAddressCheckpoints
    -- ** Composite lenses
  , hdAccountRootId
  , hdAddressRootId
  , hdAddressChain
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
    -- * General-utility functions
  , eskToHdRootId
  ) where

import Control.Lens (at)
import Control.Lens.TH (makeLenses)
import qualified Data.ByteString as BS
import qualified Data.IxSet.Typed as IxSet
import Data.SafeCopy (base, deriveSafeCopySimple)

import Test.QuickCheck (Arbitrary(..), elements, oneof, vectorOf)

import qualified Data.Text.Buildable
import Formatting (bprint, build, (%))

import qualified Pos.Core as Core
import qualified Pos.Crypto as Core

import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Spec
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet
import Ariadne.Wallet.Cardano.Kernel.Word31 (Word31)

{-------------------------------------------------------------------------------
  Supporting types
-------------------------------------------------------------------------------}

-- | Wallet name
newtype WalletName = WalletName
    { unWalletName :: Text
    } deriving (Show, Eq, Ord, IsString, ToString)
      deriving newtype (Buildable)

instance Arbitrary WalletName where
    arbitrary = pure "New wallet"

-- | Account name
newtype AccountName = AccountName
    { unAccountName :: Text
    } deriving (Show, Eq, Ord, IsString, ToString)
      deriving newtype (Buildable)

instance Arbitrary AccountName where
    arbitrary = pure "New account"

-- | Account index
newtype HdAccountIx = HdAccountIx
    { getHdAccountIx :: Word31
    } deriving (Eq, Show, Ord, Bounded)

-- NOTE(adn) if we need to generate only @hardened@ account indexes, we
-- need to extend this arbitrary instance accordingly.
instance Arbitrary HdAccountIx where
    arbitrary = HdAccountIx <$> arbitrary

-- | Whether the chain is an external or an internal one
data HdAddressChain = HdChainExternal | HdChainInternal
  deriving (Eq, Ord, Show)

instance Arbitrary HdAddressChain where
    arbitrary = elements [HdChainExternal, HdChainInternal]

-- | Address index
newtype HdAddressIx = HdAddressIx
    { getHdAddressIx :: Word31
    } deriving (Eq, Show, Ord, Bounded)

instance Arbitrary HdAddressIx where
    arbitrary = HdAddressIx <$> arbitrary

-- | Wallet assurance level
--
-- TODO: document what these levels mean (in particular, how it does translate
-- to the depth required before a transaction is marked as Persisted?)
data AssuranceLevel =
     AssuranceLevelNormal
   | AssuranceLevelStrict
   deriving (Eq, Show, Ord, Enum, Bounded)

instance Buildable AssuranceLevel where
    build AssuranceLevelNormal = "normal"
    build AssuranceLevelStrict = "strict"

instance Arbitrary AssuranceLevel where
    arbitrary = elements [minBound..maxBound]

-- | Does this wallet have a spending password
data HasSpendingPassword =
    -- | No spending password set
    NoSpendingPassword

    -- | If there is a spending password, we record when it was last updated.
  | HasSpendingPassword (InDb Core.Timestamp)

instance Buildable HasSpendingPassword where
    build NoSpendingPassword = "no"
    build (HasSpendingPassword (InDb lastUpdate)) =
        bprint ("updated " % build) lastUpdate

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

data HdRootId = HdRootId { getHdRootId :: InDb (Core.AddressHash Core.PublicKey) }
  deriving (Eq, Show, Ord)

instance Arbitrary HdRootId where
  arbitrary = do
      (_, esk) <- Core.safeDeterministicKeyGen <$> (BS.pack <$> vectorOf 12 arbitrary)
                                               <*> pure mempty
      pure (eskToHdRootId esk)

-- | HD wallet account ID
data HdAccountId = HdAccountId {
      _hdAccountIdParent :: HdRootId
    , _hdAccountIdIx     :: HdAccountIx
    }
  deriving (Eq, Show, Ord)

instance Arbitrary HdAccountId where
  arbitrary = HdAccountId <$> arbitrary <*> arbitrary

-- | HD wallet address ID
data HdAddressId = HdAddressId {
      _hdAddressIdParent :: HdAccountId
    , _hdAddressIdChain  :: HdAddressChain
    , _hdAddressIdIx     :: HdAddressIx
    }
  deriving (Eq, Show, Ord)

instance Arbitrary HdAddressId where
  arbitrary = HdAddressId <$> arbitrary <*> arbitrary <*> arbitrary

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

instance Buildable HdRoot where
    build HdRoot{..} =
        bprint (
            "HdRoot { id = " % build %
                 ", name = " % build %
          ", hasPassword = " % build %
       ", assuranceLevel = " % build %
            ", createdAt = " % build
        ) (_fromDb . getHdRootId $ _hdRootId)
          _hdRootName
          _hdRootHasPassword
          _hdRootAssurance
          (_fromDb _hdRootCreatedAt)

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

instance Buildable HdAccount where
    build HdAccount{..} =
        bprint ("HdAccount { id = "   % build
                         % " name = " % build
                         % " checkpoints = <checkpoints> "
                         % " }"
               ) _hdAccountId _hdAccountName

-- | Address in an account of a HD wallet
data HdAddress = HdAddress {
      -- | Address ID
      _hdAddressId       :: HdAddressId

      -- | The actual address
    , _hdAddressAddress  :: InDb Core.Address

      -- | Has this address been involved in a transaction?
      --
      -- TODO: How is this determined? What is the definition? How is it set?
      -- TODO: This will likely move to the 'BlockMeta' instead.
    , _hdAddressIsUsed   :: Bool

      -- | Part of the wallet state pertaining to this address,
      -- as stipulated by the wallet specification
    , _hdAddressCheckpoints :: NonEmpty AddrCheckpoint
    }

{-------------------------------------------------------------------------------
  General-utility functions
-------------------------------------------------------------------------------}

-- | Computes the 'HdRootId' from the given 'EncryptedSecretKey'.
eskToHdRootId :: Core.EncryptedSecretKey -> HdRootId
eskToHdRootId = HdRootId . InDb . Core.addressHash . Core.encToPublic

{-------------------------------------------------------------------------------
  Template Haskell splices
-------------------------------------------------------------------------------}

makeLenses ''HdAccountId
makeLenses ''HdAddressId

makeLenses ''HdRoot
makeLenses ''HdAccount
makeLenses ''HdAddress
makeLenses ''WalletName
makeLenses ''AccountName
makeLenses ''HdAccountIx
makeLenses ''HdAddressIx

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

hdAddressChain :: Lens' HdAddress HdAddressChain
hdAddressChain = hdAddressId . hdAddressIdChain

hdAccountCurrentCheckpoint :: Lens' HdAccount AccCheckpoint
hdAccountCurrentCheckpoint = hdAccountCheckpoints . currentAccCheckpoint

{-------------------------------------------------------------------------------
  Unknown identifiers
-------------------------------------------------------------------------------}

-- | Unknown root
data UnknownHdRoot =
    -- | Unknown root ID
    UnknownHdRoot HdRootId
    deriving (Eq, Show)

instance Arbitrary UnknownHdRoot where
    arbitrary = oneof [ UnknownHdRoot <$> arbitrary
                      ]

-- | Unknown account
data UnknownHdAccount =
    -- | Unknown root ID
    UnknownHdAccountRoot HdRootId

    -- | Unknown account (implies the root is known)
  | UnknownHdAccount HdAccountId
  deriving (Eq, Show)

instance Arbitrary UnknownHdAccount where
    arbitrary = oneof [ UnknownHdAccountRoot <$> arbitrary
                      , UnknownHdAccount <$> arbitrary
                      ]

-- | Unknown address
data UnknownHdAddress =
    -- | Unknown root ID
    UnknownHdAddressRoot HdRootId

    -- | Unknown account (implies the root is known)
  | UnknownHdAddressAccount HdAccountId

    -- | Unknown address (implies the account is known)
  | UnknownHdAddress HdAddressId

    -- | Unknown address (implies it was not derived from the given Address)
  | UnknownHdCardanoAddress Core.Address
  deriving (Eq, Show)

instance Exception UnknownHdRoot where
  displayException (UnknownHdRoot rootId) =
    toString $ "The wallet " <> pretty rootId <> " does not exist."

instance Exception UnknownHdAccount where
  displayException (UnknownHdAccountRoot rootId) =
    toString $ "The wallet '" <> pretty rootId <> "' does not exist."
  displayException (UnknownHdAccount accId) =
    toString $ "The account '" <> pretty accId <> "' does not exist."

instance Exception UnknownHdAddress where
  displayException (UnknownHdAddressRoot rootId) =
    toString $ "The wallet '" <> pretty rootId <> "' does not exist."
  displayException (UnknownHdAddressAccount accId) =
    toString $ "The account '" <> pretty accId <> "' does not exist."
  displayException (UnknownHdAddress addrId) =
    toString $ "The address '" <> pretty addrId <> "' does not exist."
  displayException (UnknownHdCardanoAddress cardanoAddress) =
    toString $ "Cardano address '" <> pretty cardanoAddress <> "' is not stored in the DB."

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

type SecondaryHdRootIxs    = '[]
type SecondaryHdAccountIxs = '[HdRootId]
type SecondaryHdAddressIxs = '[HdRootId, HdAccountId, Core.Address]

type instance IndicesOf HdRoot    = SecondaryHdRootIxs
type instance IndicesOf HdAccount = SecondaryHdAccountIxs
type instance IndicesOf HdAddress = SecondaryHdAddressIxs

instance IxSet.Indexable (HdRootId ': SecondaryHdRootIxs)
                         (OrdByPrimKey HdRoot) where
    indices = ixList

instance IxSet.Indexable (HdAccountId ': SecondaryHdAccountIxs)
                         (OrdByPrimKey HdAccount) where
    indices = ixList
                (ixFun ((:[]) . view hdAccountRootId))

instance IxSet.Indexable (HdAddressId ': SecondaryHdAddressIxs)
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

instance Buildable HdAddressChain where
    build HdChainInternal = "internal"
    build HdChainExternal = "external"

instance Buildable HdAddressIx where
    build (HdAddressIx ix)
        = bprint ("HdAddressIx: "%build) ix

instance Buildable HdAddressId where
    build (HdAddressId parentId chain addressIx)
        = bprint ("HdAddressId: "%build%", "%build%", "%build) parentId chain addressIx

instance Buildable UnknownHdRoot where
    build (UnknownHdRoot rootId)
        = bprint ("UnknownHdRoot: "%build) rootId

instance Buildable UnknownHdAccount where
    build (UnknownHdAccountRoot rootId)
        = bprint ("UnknownHdAccountRoot: "%build) rootId
    build (UnknownHdAccount accountId)
        = bprint ("UnknownHdAccount accountId: "%build) accountId
