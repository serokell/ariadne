{-# LANGUAGE ConstraintKinds, RankNTypes #-}

-- | Infrastructure for working with indexed sets
module Ariadne.Wallet.Cardano.Kernel.DB.Util.IxSet
       ( -- * Primary keys
         HasPrimKey(..)
       , OrdByPrimKey -- opaque
         -- * Wrapper around IxSet
       , IndicesOf
       , IxSet
       , Indexable
         -- * Building 'Indexable' instances
       , deleteIxAll
       , ixFun
       , ixList
         -- * Queries
       , getEQ
       , member
       , size
       , getOne
       , getTheOnly
       , toMap
         -- * Modification
       , updateIxManyM
         -- * Construction
       , fromList
       , singleton
       , omap
       , otraverse
       , emptyIxSet
         -- * Destruction
       , toAscList
       , toList
         -- * Indexing
       , (@+)
       ) where

import Prelude hiding (toList)

import qualified Control.Lens as Lens
import Data.Coerce (coerce)
import qualified Data.Foldable
import qualified Data.IxSet.Typed as IxSet
import qualified Data.Map.Strict as Map
import Data.SafeCopy (SafeCopy(..), contain, safeGet, safePut)
import qualified Data.Set as Set
import qualified Data.Traversable

-- Imports needed for the various instances
import qualified Data.Text.Buildable
import Formatting (bprint, build)
import Pos.Infra.Util.LogSafe
  (BuildableSafe, SecureLog, buildSafeList, getSecureLog, secure)
import Serokell.Util (listJsonIndent)
import Test.QuickCheck (Arbitrary(..))
import qualified Text.Show

{-# ANN module ("HLint: ignore Unnecessary hiding" :: Text) #-}

{-------------------------------------------------------------------------------
  Primary keys
-------------------------------------------------------------------------------}

-- | Type equipped with a primary key
--
-- The key assumption is that such types can be compared for equality and
-- sorted using their primary key only.
class Ord (PrimKey a) => HasPrimKey a where
  type PrimKey a :: *
  primKey :: a -> PrimKey a

-- | Newtype wrapper around a type that uses the type's primary key for
-- equality and ordering comparisons.
--
-- Unfortunately we cannot keep this type entirely internally, since we need
-- it in 'Indexable' instances. TODO: Is that fixable?
newtype OrdByPrimKey a = WrapOrdByPrimKey { unwrapOrdByPrimKey :: a }

instance HasPrimKey a => Eq (OrdByPrimKey a) where
  (==) = (==) `on` (primKey . unwrapOrdByPrimKey)

instance HasPrimKey a => Ord (OrdByPrimKey a) where
  compare = compare `on` (primKey . unwrapOrdByPrimKey)

instance Buildable a => Buildable (OrdByPrimKey a) where
    build (WrapOrdByPrimKey o) = bprint build o

instance (SafeCopy a) => SafeCopy (OrdByPrimKey a) where
  getCopy = contain $ WrapOrdByPrimKey <$> safeGet
  putCopy (WrapOrdByPrimKey a) = contain . safePut $ a

{-------------------------------------------------------------------------------
  Wrap IxSet
-------------------------------------------------------------------------------}

-- | Static set of indices per type
type family IndicesOf (a :: *) :: [*]

-- | Wrapper around IxSet
--
-- This is an 'IxSet' with a fixed set of indices ('IndicesOf') as well as
-- a primary key.
--
-- NOTE: This module is intended as a replacement for an import of "Data.IxSet",
-- so we use the same names as "Data.IxSet" uses.
newtype IxSet a = WrapIxSet {
      unwrapIxSet :: IxSet.IxSet (PrimKey a ': IndicesOf a) (OrdByPrimKey a)
    }
deriving instance Indexable a => Eq (IxSet a)

instance Show a => Show (IxSet a) where
    show = show . map unwrapOrdByPrimKey . IxSet.toList . unwrapIxSet

-- | Evidence that the specified indices are in fact available
type Indexable a = IxSet.Indexable (PrimKey a ': IndicesOf a) (OrdByPrimKey a)

-- | Evidence that something is an index
type IsIndexOf ix a = IxSet.IsIndexOf ix (PrimKey a ': IndicesOf a)

{-------------------------------------------------------------------------------
  Safecopy
-------------------------------------------------------------------------------}

instance (Indexable a, SafeCopy a) => SafeCopy (IxSet a) where
  getCopy =  contain $ WrapIxSet <$> safeGet
  putCopy (WrapIxSet ixset) = contain . safePut $ ixset

{-------------------------------------------------------------------------------
  Building 'Indexable' instances
-------------------------------------------------------------------------------}

ixFun :: Ord ix => (a -> [ix]) -> IxSet.Ix ix (OrdByPrimKey a)
ixFun f = IxSet.ixFun (f . unwrapOrdByPrimKey)

ixList :: ( HasPrimKey a
          , IxSet.MkIxList ixs (PrimKey a : ixs) (OrdByPrimKey a) r
          )
       => r
ixList = IxSet.ixList (ixFun ((:[]) . primKey))

{-------------------------------------------------------------------------------
  Lens instances for the primary key
-------------------------------------------------------------------------------}

type instance Lens.Index   (IxSet a) = PrimKey a
type instance Lens.IxValue (IxSet a) = a

instance (HasPrimKey a, Indexable a) => Lens.Ixed (IxSet a) where
  ix pk f (WrapIxSet s) =
      case IxSet.getOne (IxSet.getEQ pk s) of
        Nothing -> pure $ WrapIxSet s
        Just a  -> upd <$> f (unwrapOrdByPrimKey a)
    where
      upd :: a -> IxSet a
      upd a = WrapIxSet $ IxSet.updateIx pk (WrapOrdByPrimKey a) s

instance (HasPrimKey a, Indexable a) => Lens.At (IxSet a) where
  at pk f (WrapIxSet s) =
      upd <$> f (unwrapOrdByPrimKey <$> IxSet.getOne (IxSet.getEQ pk s))
    where
      upd :: Maybe a -> IxSet a
      upd Nothing  = WrapIxSet $ IxSet.deleteIx pk                      s
      upd (Just a) = WrapIxSet $ IxSet.updateIx pk (WrapOrdByPrimKey a) s

{-------------------------------------------------------------------------------
  Standard and @universum@ type class instances
-------------------------------------------------------------------------------}

instance Container (IxSet a)

instance Foldable IxSet where
    null = IxSet.null . unwrapIxSet
    toList = coerce . IxSet.toList . unwrapIxSet
    foldr f e = Data.Foldable.foldr (f . unwrapOrdByPrimKey) e . unwrapIxSet

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

getEQ :: (Indexable a, IsIndexOf ix a) => ix -> IxSet a -> IxSet a
getEQ ix = WrapIxSet . IxSet.getEQ ix . unwrapIxSet

-- | Returns either element by its index or
-- index to which several (or zero) elements correspond + those elements
getTheOnly
    :: (Indexable a, HasPrimKey a, IsIndexOf ix a) => ix -> IxSet a -> Either (ix, IxSet a) a
getTheOnly ix ixset =
    let eqElems = getEQ ix ixset
    in case getOne eqElems of
         Just a -> Right a
         Nothing -> Left (ix, eqElems)

member :: (HasPrimKey a, Indexable a) => PrimKey a -> IxSet a -> Bool
member pk = isJust . view (Lens.at pk)

size :: IxSet a -> Int
size = IxSet.size . unwrapIxSet

-- | Safely returns the 'head' of this 'IxSet', but only if it is a singleton
-- one, i.e. only if it has @exactly@ one element in it. Usually this is
-- used in tandem with 'getEQ' to witness the existence of exactly one element
-- in the set indexed by a particular index.
getOne :: HasPrimKey a => IxSet a -> Maybe a
getOne = fmap coerce . IxSet.getOne . unwrapIxSet

-- | Project out the underlying set
--
-- Use with caution! This loses all the indices, potentially losing all the
-- benefits that 'IxSet' provides.
toMap :: HasPrimKey a => IxSet a -> Map (PrimKey a) a
toMap = Map.mapKeysMonotonic (primKey . unwrapOrdByPrimKey)
      . Map.fromSet unwrapOrdByPrimKey
      . IxSet.toSet
      . unwrapIxSet

{-------------------------------------------------------------------------------
  Modification
-------------------------------------------------------------------------------}

updateIxManyM
    :: forall a ix f. (Indexable a, IsIndexOf ix a, Applicative f)
    => ix -> (a -> f a) -> IxSet a -> f (IxSet a)
updateIxManyM pk f ws = do
    let originalValues = toList $ getEQ pk ws
    mappedValues <- traverse f originalValues
    pure $ coerce $
        foldr insert (foldr delete ws originalValues) mappedValues
  where
    insert :: a -> IxSet a -> IxSet a
    insert a = coerce . (IxSet.insert $ WrapOrdByPrimKey a) . coerce
    delete :: a -> IxSet a -> IxSet a
    delete a = coerce . (IxSet.delete $ WrapOrdByPrimKey a) . coerce

-- | Delete all values with the given key.

deleteIxAll :: (Indexable a, IsIndexOf ix a) => ix -> IxSet a -> IxSet a
deleteIxAll ix (WrapIxSet ixSet) =
    WrapIxSet (IxSet.getGT ix ixSet `IxSet.union` IxSet.getLT ix ixSet)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Construct 'IxSet' from a list
fromList :: Indexable a => [a] -> IxSet a
fromList = WrapIxSet . IxSet.fromList . coerce

-- | Construct 'IxSet' from a single element
singleton :: Indexable a => a -> IxSet a
singleton = fromList . (:[])

-- | Monomorphic map over an 'IxSet'
--
-- Since we assume that the primary keys never change, we do not need to
-- build the set itself. However, we do need to rebuild the indices.
omap :: forall a. Indexable a => (a -> a) -> IxSet a -> IxSet a
omap f =
      WrapIxSet
    . IxSet.fromSet
    . Set.mapMonotonic (coerce f)
    . IxSet.toSet
    . unwrapIxSet

-- | Monomorphic traversal over an 'IxSet'
--
-- NOTE: This rebuilds the entire 'IxSet'. Potentially expensive.
otraverse :: (Applicative f, Indexable a)
          => (a -> f a) -> IxSet a -> f (IxSet a)
otraverse f = fmap fromList . Data.Traversable.traverse f . toList

emptyIxSet :: forall a.
              Indexable a
           => IxSet a
emptyIxSet = WrapIxSet IxSet.empty

{-------------------------------------------------------------------------------
  Destruction
-------------------------------------------------------------------------------}

-- | Converts the 'IxSet' back into a plain list. You need to use this function
-- with care as unwrapping the 'IxSet' means losing the performance advantages
-- of using it in the first place.
-- You probably want to use this function only at application boundaries, i.e.
-- before the data gets consumed by the web handlers.
toList :: IxSet a -> [a]
toList = coerce . IxSet.toList . unwrapIxSet

-- | Converts an 'IxSet' to its list of elements.
--
-- List will be sorted in ascending order by the primary key.
--
-- The list may contain duplicate entries if a single value produces
-- multiple keys.
toAscList ::
       forall a. Ord (PrimKey a)
    => IxSet a
    -> [a]
toAscList = coerce . IxSet.toAscList (Proxy @(PrimKey a)) . unwrapIxSet

{-------------------------------------------------------------------------------
  Indexing
-------------------------------------------------------------------------------}

-- | Creates the subset that has an index in the provided list.
(@+) :: Indexable a => IxSet a -> [PrimKey a] -> IxSet a
(WrapIxSet ixSet) @+ indices = WrapIxSet (ixSet IxSet.@+ indices)

{-------------------------------------------------------------------------------
  Other miscellanea instances for IxSet
-------------------------------------------------------------------------------}

instance (Indexable a, Arbitrary a) => Arbitrary (IxSet a) where
    arbitrary = fromList <$> arbitrary

instance Buildable a => Buildable (IxSet a) where
    build = bprint (listJsonIndent 4) . map unwrapOrdByPrimKey
                                      . IxSet.toList
                                      . unwrapIxSet

instance BuildableSafe a => Buildable (SecureLog (IxSet a)) where
    build = bprint (buildSafeList secure) . map unwrapOrdByPrimKey
                                          . IxSet.toList
                                          . unwrapIxSet
                                          . getSecureLog

