{-# LANGUAGE AllowAmbiguousTypes #-}

module Knit.Utils where

import Data.Union
import Data.Vinyl.TypeLevel
import Data.Vinyl.Core hiding (Dict)
import Data.Type.Equality
import Data.Proxy
import Control.Lens

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

uliftElem
  :: forall f xs a.
     Elem xs a
  => f a
  -> Union f xs
uliftElem v = umap (\Refl -> v) (elemEv @xs @a)

umatchElem
  :: forall f xs a.
     Elem xs a
  => Union f xs
  -> Maybe (f a)
umatchElem = go (elemEv @xs @a)
  where
    go
      :: forall xs'.
         Union ((:~:) a) xs'
      -> Union f xs'
      -> Maybe (f a)
    go (This Refl) (This v) = Just v
    go (That i) (That v) = go i v
    go _ _ = Nothing

uprismElem :: forall f xs a. Elem xs a => Prism' (Union f xs) (f a)
uprismElem = prism' uliftElem umatchElem

data Some c f where
  Some :: c x => f x -> Some c f

type Spine = Rec Proxy

class KnownSpine xs where
  knownSpine :: Spine xs

instance KnownSpine '[] where
  knownSpine = RNil

instance KnownSpine xs => KnownSpine (x:xs) where
  knownSpine = Proxy :& knownSpine
