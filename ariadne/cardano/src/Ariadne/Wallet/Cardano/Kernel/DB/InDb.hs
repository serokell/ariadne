{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ariadne.Wallet.Cardano.Kernel.DB.InDb (
    InDb(..)
  , fromDb
  ) where

import Universum

import Control.Lens.TH (makeLenses)
import Data.Coerce (coerce)
import Data.SafeCopy
  (SafeCopy(..), base, contain, deriveSafeCopySimple, safeGet, safePut)
import qualified Data.Set as Set

import qualified Pos.Core as Core
import qualified Pos.Core.Txp as Txp
import qualified Pos.Crypto as Core
import Pos.SafeCopy ()
import qualified Pos.Txp as Core (Utxo)

{-------------------------------------------------------------------------------
  Wrap core types so that we can make independent serialization decisions
-------------------------------------------------------------------------------}

-- | Wrapped type (with potentially different 'SafeCopy' instance)
newtype InDb a = InDb { _fromDb :: a }
  deriving (Eq, Show, Ord)

instance Functor InDb where
  fmap f = InDb . f . _fromDb

instance Applicative InDb where
  pure = InDb
  InDb f <*> InDb x = InDb (f x)

makeLenses ''InDb

{-------------------------------------------------------------------------------
 Orphans
-------------------------------------------------------------------------------}

deriveSafeCopySimple 1 'base ''Core.TxAux
deriveSafeCopySimple 1 'base ''Core.Timestamp

{-------------------------------------------------------------------------------
  Specific SafeCopy instances
-------------------------------------------------------------------------------}

instance SafeCopy (InDb Core.Utxo) where
  getCopy = contain $ InDb <$> safeGet
  putCopy (InDb a) = contain . safePut $ a

-- TODO: This is really a UTxO again..
instance SafeCopy (InDb (NonEmpty (Txp.TxIn, Core.TxOutAux))) where
  getCopy = contain $ InDb <$> safeGet
  putCopy (InDb a) = contain . safePut $ a

instance SafeCopy (InDb Core.Address) where
  getCopy = contain $ InDb <$> safeGet
  putCopy (InDb a) = contain . safePut $ a

instance SafeCopy (InDb (Core.AddressHash Core.PublicKey)) where
  getCopy = contain $ InDb <$> safeGet
  putCopy (InDb a) = contain . safePut $ a

instance SafeCopy (InDb Core.Coin) where
  getCopy = contain $ InDb <$> safeGet
  putCopy (InDb a) = contain . safePut $ a

instance SafeCopy (InDb Core.SlotId) where
  getCopy = contain $ InDb <$> safeGet
  putCopy (InDb a) = contain . safePut $ a

instance SafeCopy (InDb Core.Timestamp) where
  getCopy = contain $ InDb <$> safeGet
  putCopy (InDb a) = contain . safePut $ a

instance SafeCopy (InDb Txp.TxAux) where
  getCopy = contain $ InDb <$> safeGet
  putCopy (InDb a) = contain . safePut $ a

instance SafeCopy (InDb (Map Txp.TxId Txp.TxAux)) where
  getCopy = contain $ InDb <$> safeGet
  putCopy (InDb a) = contain . safePut $ a

instance SafeCopy (InDb Txp.TxId) where
  getCopy = contain $ InDb <$> safeGet
  putCopy (InDb a) = contain . safePut $ a

instance SafeCopy (InDb Txp.TxIn) where
  getCopy = contain $ InDb <$> safeGet
  putCopy (InDb a) = contain . safePut $ a

instance (Ord a, SafeCopy (InDb a)) => SafeCopy (InDb (Set a)) where
  getCopy = contain $ (InDb . Set.map _fromDb) <$> safeGet
  putCopy (InDb a) = contain . safePut $ Set.map InDb $ coerce a

instance SafeCopy (InDb (Map Txp.TxId Core.SlotId)) where
  getCopy = contain $ InDb <$> safeGet
  putCopy (InDb a) = contain . safePut $ a
