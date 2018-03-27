{-# LANGUAGE AllowAmbiguousTypes #-}
module Knit.Utils where

import Data.Union
import Data.Vinyl.TypeLevel
import Data.Vinyl.Core hiding (Dict)
import Data.Type.Equality
import Data.Proxy
import GHC.Exts (Constraint)

umapConstrained
  :: forall c f g as.
     AllConstrained c as
  => (forall a . c a => f a -> g a)
  -> Union f as
  -> Union g as
umapConstrained f = \case
  This a -> This (f a)
  That u -> That (umapConstrained @c f u)

class RIndex a xs ~ n => Elem' n a xs where
  elemEv' :: Union ((:~:) a) xs

instance (xs ~ (a : xs')) => Elem' 'Z a xs where
  elemEv' = This Refl

instance
    ( RIndex a (x : xs') ~ 'S n
    , xs ~ (x : xs')
    , Elem' n a xs'
    ) => Elem' ('S n) a xs
  where
    elemEv' = That elemEv'

class Elem' (RIndex x xs) x xs => Elem xs x
instance Elem' (RIndex x xs) x xs => Elem xs x

elemEv :: forall xs a. Elem xs a => Union ((:~:) a) xs
elemEv = elemEv'

rgetElem :: forall f xs a. Elem xs a => Rec f xs -> f a
rgetElem = go (elemEv @xs @a)
  where
    go
      :: forall xs'.
         Union ((:~:) a) xs'
      -> Rec f xs'
      -> f a
    go (This Refl) (fa :& _) = fa
    go (That i) (_ :& fxs) = go i fxs

data Some c f where
  Some :: c x => f x -> Some c f

type Spine = Rec Proxy

class KnownSpine xs where
  knownSpine :: Spine xs

instance KnownSpine '[] where
  knownSpine = RNil

instance KnownSpine xs => KnownSpine (x:xs) where
  knownSpine = Proxy :& knownSpine

type family AllConstrainedF c f xs :: Constraint where
  AllConstrainedF _ _ '[] = ()
  AllConstrainedF c f (x : xs) =
    ( c (f x)
    , AllConstrainedF c f xs
    )

data Dict c where
  Dict :: c => Dict c

type UnionBaseCase c f = Dict (c (Union f '[]))
type UnionInductionCase c f =
  forall x xs'.
     Dict (c (f x))
  -> Dict (c (Union f xs'))
  -> Dict (c (Union f (x:xs')))

deduceUnion
  :: forall c f xs.
     (AllConstrainedF c f xs, KnownSpine xs)
  => UnionBaseCase c f
  -> UnionInductionCase c f
  -> Dict (c (Union f xs))
deduceUnion baseCase inductionCase = go (knownSpine @xs)
  where
    go
      :: forall xs'.
         AllConstrainedF c f xs'
      => Spine xs'
      -> Dict (c (Union f xs'))
    go RNil = baseCase
    go (Proxy :& xs) = inductionCase Dict (go xs)

deduceEqUnion :: forall f xs.
  (AllConstrainedF Eq f xs, KnownSpine xs) => Dict (Eq (Union f xs))
deduceEqUnion = deduceUnion @Eq Dict (\Dict Dict -> Dict)

deduceOrdUnion :: forall f xs.
  (AllConstrainedF Ord f xs, KnownSpine xs) => Dict (Ord (Union f xs))
deduceOrdUnion = deduceUnion @Ord Dict (\Dict Dict -> Dict)

deduceShowUnion :: forall f xs.
  (AllConstrainedF Show f xs, KnownSpine xs) => Dict (Show (Union f xs))
deduceShowUnion = deduceUnion @Show Dict (\Dict Dict -> Dict)
