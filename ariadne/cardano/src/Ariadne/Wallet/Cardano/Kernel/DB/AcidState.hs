{-# LANGUAGE RankNTypes, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-deriving-typeable #-}

-- | Acid-state database for the wallet kernel
module Ariadne.Wallet.Cardano.Kernel.DB.AcidState
       ( -- * Top-level database
         DB(..)
       , dbHdWallets
       , defDB
         -- * Acid-state operations
         -- ** Snapshot
       , Snapshot(..)
         -- ** Spec mandated updates
       , NewPending(..)
       , CancelPending(..)
       , ApplyBlock(..)
       , SwitchToFork(..)
         -- ** Updates on HD wallets
         -- *** CREATE
       , CreateHdWallet(..)
       , CreateHdAccount(..)
       , CreateHdAddress(..)
         -- *** UPDATE
       , UpdateHdWalletName(..)
       , UpdateHdWalletAssurance(..)
       , UpdateHdAccountName(..)
         -- *** DELETE
       , DeleteHdRoot(..)
       , DeleteHdAccount(..)
         -- * Errors
       , NewPendingError
         -- * Testing
       , ObservableRollbackUseInTestsOnly(..)
       ) where

import Control.Lens (at, non, to)
import Control.Lens.TH (makeLenses)
import Control.Monad.Except (MonadError, catchError, runExcept)

import Test.QuickCheck (Arbitrary(..), oneof)

import Data.Acid (Query, Update, makeAcidic)
import qualified Data.Map.Merge.Strict as Map.Merge
import qualified Data.Map.Strict as Map
import Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text.Buildable
import Formatting (bprint, build, sformat, (%))

import qualified Pos.Core as Core
import Pos.Core.Chrono (OldestFirst(..))
import qualified Pos.Core.Txp as Txp
import Pos.Txp (Utxo)

import qualified Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet as IxSet
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx
  (AddrWithId, PrefilteredBlock(..), PrefilteredUtxo, UtxoByAccount,
  emptyPrefilteredBlock)

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Create as HD
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Delete as HD
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Update as HD
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Spec
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Spec.Update as Spec
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Spec.Util as Spec
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState

{-------------------------------------------------------------------------------
  Top-level database
-------------------------------------------------------------------------------}

-- | Full state of the wallet, with the exception of transaction metadata
--
-- We store the different kinds of wallets in different maps for increased
-- type safety. Moreover, since we currently only have a single type of wallet,
-- trying to factor our common parts would be premature at this point.
--
--  References:
--
--  * The acid-state DB for the legacy wallet is defined in module
--    "Pos.Wallet.Web.State.Storage".
--  * V1 API defined in "Ariadne.Wallet.Cardano.API.V1.*" (in @src/@)
data DB = DB {
      _dbHdWallets :: HdWallets
    }

makeLenses ''DB
deriveSafeCopySimple 1 'base ''DB

-- | Default DB
defDB :: DB
defDB = DB initHdWallets

{-------------------------------------------------------------------------------
  Wrap wallet spec
-------------------------------------------------------------------------------}

-- | Errors thrown by 'newPending'
data NewPendingError =
    -- | Unknown account
    NewPendingUnknown UnknownHdAccount

    -- | Some inputs are not in the wallet utxo
  | NewPendingFailed Spec.NewPendingFailed

deriveSafeCopySimple 1 'base ''NewPendingError

instance Arbitrary NewPendingError where
    arbitrary = oneof [ NewPendingUnknown <$> arbitrary
                      , NewPendingFailed  <$> arbitrary
                      ]

instance Buildable NewPendingError where
    build (NewPendingUnknown unknownAccount) =
        bprint ("NewPendingUnknown " % build) unknownAccount
    build (NewPendingFailed npf) =
        bprint ("NewPendingFailed " % build) npf

newPending :: HdAccountId
           -> InDb Txp.TxAux
           -> Update DB (Either NewPendingError ())
newPending accountId tx = runUpdate' . zoom dbHdWallets $ do
    zoomHdAccountId NewPendingUnknown accountId $
      zoom hdAccountCheckpoints $
        mapUpdateErrors NewPendingFailed $ Spec.newPending tx
    -- TODO: newPending for address checkpoints is actually a no-op.
    -- Perhaps we should simplify code by removing this call, even though
    -- it distances the implementation from the spec?
    zoom hdWalletsAddresses $ do
      allAddresses <- get
      newAddresses <- flip (IxSet.updateIxManyM accountId) allAddresses $ \addr ->
        coerceAction addr $ mapUpdateErrors NewPendingFailed $ Spec.newPending tx
      put newAddresses
  where
    coerceAction
        :: HdAddress
        -> Update' AddrCheckpoints NewPendingError ()
        -> Update' a NewPendingError HdAddress
    coerceAction addr action =
        let oldCheckpoints = view hdAddressCheckpoints addr
            checkpointsOrErr = runExcept $ execStateT action oldCheckpoints
        in case checkpointsOrErr of
          Left err -> throwError err
          Right newCheckpoints -> pure $ set hdAddressCheckpoints newCheckpoints addr

-- | Cancels the input transactions from the 'Checkpoints' of each of
-- the accounts cointained in the 'Cancelled' map.
--
-- The reason why this function doesn't take a 'HdRootId' as an argument
-- is because the submission layer doesn't have the notion of \"which HdWallet
-- is this transaction associated with?\", but it merely dispatch and cancels
-- transactions for all the wallets managed by this edge node.
cancelPending :: Map HdAccountId (InDb (Set Txp.TxId)) -> Update DB ()
cancelPending cancelled = void . runUpdate' . zoom dbHdWallets $
    forM_ (toPairs cancelled) $ \(accountId, InDb txids) ->
        -- Here we are deliberately swallowing the possible exception
        -- returned by the wrapped 'zoom' as the only reason why this update
        -- might fail is if, in the meantime, the target account was cancelled,
        -- in which case we do want the entire computation to abort, but simply
        -- skip cancelling the transactions for the account that has been removed.
        handleError (\(_e :: UnknownHdAccount) -> pass) $
            zoomHdAccountId identity accountId $ do
              modify' (over hdAccountCheckpoints (Spec.cancelPending txids))
    where
        handleError :: MonadError e m => (e -> m a) -> m a -> m a
        handleError = flip catchError

-- | Apply prefiltered block (indexed by HdAccountId) to the matching accounts.
--
-- The prefiltered block should be indexed by AccountId, with each prefiltered block
-- containing only inputs and outputs relevant to the account. Since HdAccountId embeds HdRootId,
-- it unambiguously places an Account in the Wallet/Account hierarchy. The AccountIds here could
-- therefor refer to an Account in /any/ Wallet (not only sibling accounts in a single wallet).

-- NOTE:
-- * Calls to 'applyBlock' must be sequentialized by the caller
-- (although concurrent calls to 'applyBlock' cannot interfere with each
-- other, 'applyBlock' must be called in the right order.)
--
-- * Since a block may reference wallet accounts that do not exist yet locally,
-- we need to create such 'missing' accounts. (An Account might not exist locally
-- if it was created on another node instance of this wallet).
--
-- * For every address encountered in the block outputs, create an HdAddress if it
-- does not already exist.
applyBlock :: Map HdAccountId PrefilteredBlock -> Update DB ()
applyBlock blocksByAccount = runUpdateNoErrors $ zoom dbHdWallets $
    createPrefiltered
        mkPrefilteredUtxo
        (\prefBlock -> zoom hdAccountCheckpoints $
                           modify $ Spec.applyBlock prefBlock)
        prefilterBlockToAddress
        (\prefBlock -> zoom hdAddressCheckpoints $
                           modify $ Spec.applyBlock prefBlock)
        blocksByAccount
        Map.empty -- we are not providing custom names to accounts discovered in applyBlock
  where
    -- Initial UTxO and addresses for a new account
    --
    -- NOTE: When we initialize the kernel, we look at the genesis UTxO and create
    -- an initial balance for all accounts that we recognize as ours. This means
    -- that when we later discover a new account that is also ours, it cannot appear
    -- in the genesis UTxO, because if it did, we would already have seen it (the
    -- genesis UTxO is static, after all). As PrefilteredUtxo is by default empty,
    -- the initial UTxO for accounts discovered during 'applyBlock' (and 'switchToFork')
    -- will be empty too.
    mkPrefilteredUtxo :: PrefilteredBlock -> PrefilteredUtxo
    mkPrefilteredUtxo = pfbPrefilteredUtxo

-- | Switch to a fork
--
-- See comments about prefiltering for 'applyBlock'.
--
-- TODO: We use a plain list here rather than 'OldestFirst' since the latter
-- does not have a 'SafeCopy' instance.
switchToFork :: Int
             -> [Map HdAccountId PrefilteredBlock]
             -> Update DB ()
switchToFork n blocks = runUpdateNoErrors $ zoom dbHdWallets $ do
    blocks' <- mapM fillInEmptyBlock blocks
    createPrefiltered
        mkPrefilteredUtxo
        (\prefBlocks -> zoom hdAccountCheckpoints $
                            modify $ Spec.switchToFork n (OldestFirst prefBlocks))
        prefilterToAddress
        (\prefBlocks -> zoom hdAddressCheckpoints $
                            modify $ Spec.switchToFork n (OldestFirst prefBlocks))
        (distribute blocks')
        Map.empty
  where
    -- The natural result of prefiltering each block is a list of maps, but
    -- in order to apply them to each account, we want a map of lists
    --
    -- NOTE: We have to be careful to /first/ use 'fillInEmptyBlock' to make
    -- sure that if, say, the first and third slot both contain a block for
    -- account A, but the second does not, we end up with an empty block
    -- inserted for slot 2.
    distribute :: [Map HdAccountId PrefilteredBlock]
               -> Map HdAccountId [PrefilteredBlock]
    distribute = Map.unionsWith (++) . map (Map.map (:[]))

    -- See comments in 'applyBlock'
    mkPrefilteredUtxo :: [PrefilteredBlock] -> PrefilteredUtxo
    mkPrefilteredUtxo = Map.unionsWith Map.union . map pfbPrefilteredUtxo

    prefilterToAddress :: AddrWithId -> [PrefilteredBlock] -> [PrefilteredBlock]
    prefilterToAddress addrWithId = map (prefilterBlockToAddress addrWithId)

-- | Observable rollback, used for tests only
--
-- See 'switchToFork' for use in real code.
observableRollbackUseInTestsOnly :: Update DB ()
observableRollbackUseInTestsOnly = runUpdateNoErrors $ do
    zoomAll (dbHdWallets . hdWalletsAccounts) $
      hdAccountCheckpoints %~ Spec.observableRollbackUseInTestsOnly
    zoomAll (dbHdWallets . hdWalletsAddresses) $
      hdAddressCheckpoints %~ Spec.observableRollbackUseInTestsOnly

{-------------------------------------------------------------------------------
  Wallet creation
-------------------------------------------------------------------------------}

-- | Create an HdWallet with HdRoot, possibly with HdAccounts and HdAddresses.
--
--  Given prefiltered utxo's, by account, create an HdAccount for each account,
--  along with HdAddresses for all utxo outputs.
--
-- NOTE: since the genesis Utxo does not come into being through regular transactions,
--       there is no block metadata to record when we create a wallet
createHdWallet :: HdRoot
               -> UtxoByAccount
               -> Map HdAccountId AccountName
               -> Update DB (Either HD.CreateHdRootError ())
createHdWallet newRoot utxoByAccount accountNames = runUpdate' . zoom dbHdWallets $ do
      HD.createHdRoot newRoot
      createPrefiltered
        identity
        doNothing -- we just want to create the accounts
        mkSingleton
        doNothing
        utxoByAccount
        accountNames

createHdAccount :: HdAccountId
                -> PrefilteredUtxo
                -> AccountName
                -> Update DB (Either HD.CreateHdAccountError HdAccount)
createHdAccount accId prefilteredUtxo accountName = runUpdate' . zoom dbHdWallets $ do
    -- Make sure root exists. An alternative is to check this within
    -- @createAccPrefiltered@ by passing a handler there (instead of
    -- @assumeHdRootExists@), but it has too many parameters already.
    zoomHdRootId HD.CreateHdAccountUnknownRoot rootId $
        pass

    createAccPrefiltered
        identity
        doNothing
        mkSingleton
        doNothing
        accId
        prefilteredUtxo
        (Just accountName)
  where
    rootId :: HdRootId
    rootId = accId ^. hdAccountIdParent

doNothing :: forall p a e. p -> Update' a e ()
doNothing _ = pass

{-------------------------------------------------------------------------------
  Internal auxiliary: apply a function to a prefiltered block/utxo
-------------------------------------------------------------------------------}

-- | Given a map from account IDs, add default values for all accounts in
-- the wallet that aren't given a value in the map
fillInDefaults :: forall p e.
                  (HdAccount -> p)   -- ^ Default value
               -> Map HdAccountId p  -- ^ Map with values per account
               -> Update' HdWallets e (Map HdAccountId p)
fillInDefaults def accs =
    aux . IxSet.toMap <$> use hdWalletsAccounts
  where
    aux :: Map HdAccountId HdAccount -> Map HdAccountId p
    aux = Map.Merge.merge newAccount needsDefault valueForExistingAcc accs

    newAccount :: Map.Merge.SimpleWhenMissing HdAccountId p p
    newAccount = Map.Merge.preserveMissing

    needsDefault :: Map.Merge.SimpleWhenMissing HdAccountId HdAccount p
    needsDefault = Map.Merge.mapMissing (const def)

    valueForExistingAcc :: Map.Merge.SimpleWhenMatched HdAccountId p HdAccount p
    valueForExistingAcc = Map.Merge.zipWithMatched $ \_accId p _acc -> p

-- | Specialization of 'fillInDefaults' for prefiltered blocks
fillInEmptyBlock :: Map HdAccountId PrefilteredBlock
                 -> Update' HdWallets e (Map HdAccountId PrefilteredBlock)
fillInEmptyBlock = fillInDefaults (const emptyPrefilteredBlock)

-- | For each of the specified accounts, create them if they do not exist,
-- and apply the specified function.
createPrefiltered :: forall p e.
                     (p -> PrefilteredUtxo)
                      -- ^ Initial UTxO for each of the addresses in the
                      -- newly created account, as well as the addresses
                      -- themselves
                  -> (p -> Update' HdAccount e ())
                      -- ^ Function to apply to the account
                  -> (AddrWithId -> p -> p)
                      -- ^ Function that prefilters p further, leaving only
                      -- stuff that is relevant to the provided address.
                  -> (p -> Update' HdAddress e ())
                      -- ^ Function to apply to the address
                  -> Map HdAccountId p
                  -> Map HdAccountId AccountName
                  -> Update' HdWallets e ()
createPrefiltered mkPrefilteredUtxo accApplyP narrowP addrApplyP pByAccount accountNames =
    forM_ (toPairs pByAccount) $ \(accId, p) ->
        createAccPrefiltered mkPrefilteredUtxo accApplyP narrowP addrApplyP accId p
            (accountNames ^. at accId)

-- | See @createPrefiltered@ for comments on parameters.
createAccPrefiltered :: forall p e.
                        (p -> PrefilteredUtxo)
                     -> (p -> Update' HdAccount e ())
                     -> (AddrWithId -> p -> p)
                     -> (p -> Update' HdAddress e ())
                     -> HdAccountId
                     -> p
                     -> Maybe AccountName
                     -> Update' HdWallets e HdAccount
createAccPrefiltered mkPrefilteredUtxo accApplyP narrowP addrApplyP accId p mbAccountName = do
    let prefilteredUtxo :: PrefilteredUtxo
        prefilteredUtxo = mkPrefilteredUtxo p
        accUtxo :: Utxo
        accUtxo = Map.unions $ elems prefilteredUtxo

    -- apply the update to the account
    let newAccount = HD.initHdAccount accId accountName (firstAccCheckpoint accUtxo)
    zoomOrCreateHdAccount
        assumeHdRootExists
        newAccount
        accId
        (accApplyP p)

    -- create addresses (if they don't exist)
    forM_ (toPairs prefilteredUtxo) $ \(addrWithId@(addressId, address), addrUtxo) ->
        zoomOrCreateHdAddress
            assumeHdAccountExists -- we created it above
            (newAddress addressId address addrUtxo)
            addressId
            (addrApplyP $ narrowP addrWithId p)

    pure newAccount
    where
        accountName :: AccountName
        accountName = fromMaybe defName mbAccountName
          where
            defName = AccountName $ sformat ("Discovered account " % build)
                                            (accId ^. hdAccountIdIx . to getHdAccountIx)

        firstAccCheckpoint :: Utxo -> AccCheckpoint
        firstAccCheckpoint utxo' = AccCheckpoint {
              _accCheckpointUtxo        = InDb utxo'
            , _accCheckpointUtxoBalance = InDb $ Spec.balance utxo'
            , _accCheckpointPending     = Pending . InDb $ Map.empty
            -- Since this is the first checkpoint before we have applied
            -- any blocks, the block metadata is empty
            , _accCheckpointBlockMeta   = mempty
            }

        newAddress :: HdAddressId -> Core.Address -> Utxo -> HdAddress
        newAddress addressId address addrUtxo =
            HD.initHdAddress addressId (InDb address) (firstAddrCheckpoint addrUtxo)

        firstAddrCheckpoint :: Utxo -> AddrCheckpoint
        firstAddrCheckpoint addrUtxo = AddrCheckpoint {
              _addrCheckpointUtxo        = InDb addrUtxo
            , _addrCheckpointUtxoBalance = InDb $ Spec.balance addrUtxo
            }

{-------------------------------------------------------------------------------
  Wrap HD C(R)UD operations
-------------------------------------------------------------------------------}

createHdAddress :: HdAddress -> Update DB (Either HD.CreateHdAddressError ())
createHdAddress hdAddress = runUpdate' . zoom dbHdWallets $
    HD.createHdAddress hdAddress

updateHdWalletName :: HdRootId
                   -> WalletName
                   -> Update DB (Either UnknownHdRoot HdRoot)
updateHdWalletName rootId name = runUpdate' . zoom dbHdWallets $
    HD.updateHdRootName rootId name

updateHdWalletAssurance :: HdRootId
                        -> AssuranceLevel
                        -> Update DB (Either UnknownHdRoot HdRoot)
updateHdWalletAssurance rootId assurance = runUpdate' . zoom dbHdWallets $
    HD.updateHdRootAssurance rootId assurance

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update DB (Either UnknownHdAccount (DB, HdAccount))
updateHdAccountName accId name = do
    a <- runUpdate' . zoom dbHdWallets $ HD.updateHdAccountName accId name
    get >>= \st' -> return $ bimap identity (st',) a

deleteHdRoot :: HdRootId -> Update DB (Either HD.DeleteHdRootError ())
deleteHdRoot rootId = runUpdate' . zoom dbHdWallets $
    HD.deleteHdRoot rootId

deleteHdAccount :: HdAccountId -> Update DB (Either HD.DeleteHdAccountError ())
deleteHdAccount accId = runUpdate' . zoom dbHdWallets $
    HD.deleteHdAccount accId

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

mkSingleton :: (Ord k, Eq m, Monoid m) => k -> Map k m -> Map k m
mkSingleton addrWithId prefilteredUtxo =
    let utxo = prefilteredUtxo ^. at addrWithId . non mempty in
    one (addrWithId, utxo)

prefilterBlockToAddress :: AddrWithId -> PrefilteredBlock -> PrefilteredBlock
prefilterBlockToAddress addrWithId PrefilteredBlock {..} =
    PrefilteredBlock
        { pfbPrefilteredInputs = mkSingleton addrWithId pfbPrefilteredInputs
        , pfbPrefilteredUtxo   = mkSingleton addrWithId pfbPrefilteredUtxo
        , pfbMeta = mempty -- AddrCheckpoints know nothing about BlockMeta
        }

{-------------------------------------------------------------------------------
  Acid-state magic
-------------------------------------------------------------------------------}

-- | Reads the full DB. This is and @must@ be the only 'Query' ever exported
-- by this module. All the getters exposed for the kernel @must@ take a 'DB'
-- as input and be completely pure.
snapshot :: Query DB DB
snapshot = ask

makeAcidic ''DB [
      -- Database snapshot
      'snapshot
      -- Updates on the "spec state"
    , 'newPending
    , 'cancelPending
    , 'applyBlock
    , 'switchToFork
      -- Updates on HD wallets
    , 'createHdAddress
    , 'createHdAccount
    , 'createHdWallet
    , 'updateHdWalletName
    , 'updateHdWalletAssurance
    , 'updateHdAccountName
    , 'deleteHdRoot
    , 'deleteHdAccount
      -- Testing
    , 'observableRollbackUseInTestsOnly
    ]
