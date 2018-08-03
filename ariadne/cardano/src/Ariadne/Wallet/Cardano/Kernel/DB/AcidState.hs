{-# OPTIONS_GHC -fno-warn-orphans #-} -- to enable... deriveSafeCopySimple 1 'base ''EncryptedSecretKey
{-# LANGUAGE RankNTypes, TemplateHaskell #-}

-- | Acid-state database for the wallet kernel
module Ariadne.Wallet.Cardano.Kernel.DB.AcidState (
    -- * Top-level database
    DB(..)
  , dbHdWallets
  , defDB
    -- * Acid-state operations
    -- ** Snapshot
  , Snapshot(..)
    -- ** Spec mandated updates
  , NewPending(..)
  , ApplyBlock(..)
  , SwitchToFork(..)
    -- ** Updates on HD wallets
    -- *** CREATE
  , CreateHdWallet(..)
  , CreateHdAddress(..)
  , CreateHdAccount(..)
    -- *** UPDATE
  , UpdateHdRootAssurance
  , UpdateHdRootName(..)
  , UpdateHdAccountName(..)
    -- *** DELETE
  , DeleteHdRoot(..)
  , DeleteHdAccount(..)
    -- * errors
  , NewPendingError
  ) where

import Universum

import Control.Lens (at, non)
import Control.Lens.TH (makeLenses)
import Control.Monad.Trans.Except (runExcept)
import Data.Acid (Query, Update, makeAcidic)
import qualified Data.Map.Strict as Map
import Data.SafeCopy (base, deriveSafeCopySimple)

import qualified Pos.Core as Core
import Pos.Core.Chrono (OldestFirst(..))
import Pos.Txp (Utxo)

import Ariadne.Wallet.Cardano.Kernel.PrefilterTx
  (AddrWithId, PrefilteredBlock(..), PrefilteredUtxo)

import Ariadne.Wallet.Cardano.Kernel.DB.BlockMeta
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Create as HD
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Delete as HD
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Update as HD
import Ariadne.Wallet.Cardano.Kernel.DB.InDb
import Ariadne.Wallet.Cardano.Kernel.DB.Spec
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Spec.Update as Spec
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Spec.Util as Spec
import Ariadne.Wallet.Cardano.Kernel.DB.Util.AcidState
import qualified Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet as IxSet

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

newPending :: HdAccountId
           -> InDb Core.TxAux
           -> Update DB (Either NewPendingError ())
newPending accountId tx = runUpdate' . zoom dbHdWallets $ do
    zoomHdAccountId NewPendingUnknown accountId $
      zoom hdAccountCheckpoints $
        mapUpdateErrors NewPendingFailed $ Spec.newPending tx
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
--
-- TODO(@uroboros/ryan) Move BlockMeta inside PrefilteredBlock (as part of CBR-239: Support history tracking and queries)
applyBlock :: (Map HdAccountId PrefilteredBlock, BlockMeta) -> Update DB ()
applyBlock (blocksByAccount,meta) = runUpdateNoErrors $ zoom dbHdWallets $
    createPrefiltered
        mkPrefilteredUtxo
        (\prefBlock -> zoom hdAccountCheckpoints $
                           modify $ Spec.applyBlock (prefBlock,meta))
        prefilterBlockToAddress
        (\prefBlock -> zoom hdAddressCheckpoints $
                           modify $ Spec.applyBlock (prefBlock,meta))
        blocksByAccount
        Map.empty -- we are not providing custom names to accounts discovered in applyBlock
  where
    -- Accounts are discovered during wallet creation (if the account was given
    -- a balance in the genesis block) or otherwise, during ApplyBlock. For
    -- accounts discovered during ApplyBlock, we can assume that there was no
    -- genesis utxo, hence we use empty initial utxo for such new accounts.
    mkPrefilteredUtxo :: PrefilteredBlock -> PrefilteredUtxo
    mkPrefilteredUtxo = pfbPrefilteredUtxo

    prefilterBlockToAddress :: AddrWithId -> PrefilteredBlock -> PrefilteredBlock
    prefilterBlockToAddress addrWithId PrefilteredBlock {..} =
        PrefilteredBlock
            { pfbPrefilteredInputs = mkSingleton addrWithId pfbPrefilteredInputs
            , pfbPrefilteredUtxo   = mkSingleton addrWithId pfbPrefilteredUtxo
            }

-- | Switch to a fork
--
-- See comments about prefiltering for 'applyBlock'.
--
-- TODO: We use a plain list here rather than 'OldestFirst' since the latter
-- does not have a 'SafeCopy' instance.
switchToFork :: Int
             -> [(PrefilteredBlock, BlockMeta)]
             -> Update DB ()
switchToFork n blocks = runUpdateNoErrors $ do
    zoomAll (dbHdWallets . hdWalletsAccounts) $
      hdAccountCheckpoints %~ Spec.switchToFork n (OldestFirst blocks)
    zoomAll (dbHdWallets . hdWalletsAddresses) $
      hdAddressCheckpoints %~ Spec.switchToFork n (OldestFirst blocks)

{-------------------------------------------------------------------------------
  Wallet/account creation
-------------------------------------------------------------------------------}

-- | Create an HdWallet with HdRoot, possibly with HdAccounts and HdAddresses.
--
--  Given prefiltered utxo's, by account, create an HdAccount for each account,
--  along with HdAddresses for all utxo outputs.
createHdWallet :: HdRoot
               -> Map HdAccountId PrefilteredUtxo
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
                -> Maybe AccountName
                -> Update DB (Either HD.CreateHdAccountError ())
createHdAccount accId prefilteredUtxo mbAccountName = runUpdate' . zoom dbHdWallets $ do
    -- Make sure root exists. An alternative is to check this within
    -- @createAccPrefiltered@ by passing a handler there (instead of
    -- @assumeHdRootExists@), but it has too many parameters already.
    zoomHdRootId HD.CreateHdAccountUnknownRoot rootId $
        return ()

    createAccPrefiltered
        identity
        doNothing
        mkSingleton
        doNothing
        accId
        prefilteredUtxo
        mbAccountName
  where
    rootId :: HdRootId
    rootId = accId ^. hdAccountIdParent

doNothing :: forall p a e. p -> Update' a e ()
doNothing _ = pass

{-------------------------------------------------------------------------------
  Internal auxiliary: apply a function to a prefiltered block/utxo
-------------------------------------------------------------------------------}

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
    forM_ (Map.toList pByAccount) $ \(accId, p) ->
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
                     -> Update' HdWallets e ()
createAccPrefiltered mkPrefilteredUtxo accApplyP narrowP addrApplyP accId p mbAccountName = do
    let prefilteredUtxo :: PrefilteredUtxo
        prefilteredUtxo = mkPrefilteredUtxo p
        accUtxo :: Utxo
        accUtxo = Map.unions $ Map.elems prefilteredUtxo

    -- apply the update to the account
    zoomOrCreateHdAccount
        assumeHdRootExists
        (newAccount accId mbAccountName accUtxo)
        accId
        (accApplyP p)

    -- create addresses (if they don't exist)
    forM_ (Map.toList prefilteredUtxo) $ \(addrWithId@(addressId, address), addrUtxo) ->
        zoomOrCreateHdAddress
            assumeHdAccountExists -- we created it above
            (newAddress addressId address addrUtxo)
            addressId
            (addrApplyP $ narrowP addrWithId p)

    where
        newAccount :: HdAccountId -> Maybe AccountName -> Utxo -> HdAccount
        newAccount accId' mbAccountName' utxo' =
            HD.initHdAccount accId' mbAccountName' (firstAccCheckpoint utxo')

        firstAccCheckpoint :: Utxo -> AccCheckpoint
        firstAccCheckpoint utxo' = AccCheckpoint {
              _accCheckpointUtxo        = InDb utxo'
            , _accCheckpointUtxoBalance = InDb $ Spec.balance utxo'
            , _accCheckpointExpected    = InDb Map.empty
            , _accCheckpointPending     = Pending . InDb $ Map.empty
            -- TODO(@uroboros/ryan) proper BlockMeta initialisation (as part of CBR-239: Support history tracking and queries)
            , _accCheckpointBlockMeta   = BlockMeta . InDb $ Map.empty
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

createHdRoot :: HdRoot -> Update DB (Either HD.CreateHdRootError ())
createHdRoot hdRoot = runUpdate' . zoom dbHdWallets $
    HD.createHdRoot hdRoot

createHdAddress :: HdAddress -> Update DB (Either HD.CreateHdAddressError ())
createHdAddress hdAddress = runUpdate' . zoom dbHdWallets $
    HD.createHdAddress hdAddress

updateHdRootAssurance :: HdRootId
                      -> AssuranceLevel
                      -> Update DB (Either UnknownHdRoot ())
updateHdRootAssurance rootId assurance = runUpdate' . zoom dbHdWallets $
    HD.updateHdRootAssurance rootId assurance

updateHdRootName :: HdRootId
                 -> WalletName
                 -> Update DB (Either UnknownHdRoot ())
updateHdRootName rootId name = runUpdate' . zoom dbHdWallets $
    HD.updateHdRootName rootId name

updateHdAccountName :: HdAccountId
                    -> AccountName
                    -> Update DB (Either UnknownHdAccount ())
updateHdAccountName accId name = runUpdate' . zoom dbHdWallets $
    HD.updateHdAccountName accId name

deleteHdRoot :: HdRootId -> Update DB ()
deleteHdRoot rootId = runUpdateNoErrors . zoom dbHdWallets $
    HD.deleteHdRoot rootId

deleteHdAccount :: HdAccountId -> Update DB (Either UnknownHdRoot ())
deleteHdAccount accId = runUpdate' . zoom dbHdWallets $
    HD.deleteHdAccount accId

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

mkSingleton :: (Ord k, Eq m, Monoid m) => k -> Map k m -> Map k m
mkSingleton addrWithId prefilteredUtxo =
    let utxo = prefilteredUtxo ^. at addrWithId . non mempty in
    one (addrWithId, utxo)

{-------------------------------------------------------------------------------
  Acid-state magic
-------------------------------------------------------------------------------}

snapshot :: Query DB DB
snapshot = ask

makeAcidic ''DB [
      -- Database snapshot
      'snapshot
      -- Updates on the "spec state"
    , 'newPending
    , 'applyBlock
    , 'switchToFork
      -- Updates on HD wallets
    , 'createHdWallet
    , 'createHdAccount
    , 'createHdRoot
    , 'createHdAddress
    , 'updateHdRootAssurance
    , 'updateHdRootName
    , 'updateHdAccountName
    , 'deleteHdRoot
    , 'deleteHdAccount
    ]