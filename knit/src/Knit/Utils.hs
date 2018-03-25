{-# LANGUAGE AllowAmbiguousTypes #-}
module Knit.Utils where

import Data.Union
import Data.Vinyl.TypeLevel
import Data.Type.Equality

umapConstrained :: forall c f g as. AllConstrained c as => (forall a . c a => f a -> g a) -> Union f as -> Union g as
umapConstrained f = \case
  This a -> This (f a)
  That u -> That (umapConstrained @c f u)

class RIndex a xs ~ n => Elem' n a xs where
  elemEv' :: Union ((:~:) a) xs

instance (a ~ x, xs ~ (x : xs')) => Elem' 'Z a xs where
  elemEv' = This Refl

instance (RIndex a (x : xs') ~ 'S n, Elem' n a xs', xs ~ (x : xs')) => Elem' ('S n) a xs where
  elemEv' = That elemEv'

class Elem' (RIndex x xs) x xs => Elem x xs
instance Elem' (RIndex x xs) x xs => Elem x xs

elemEv :: forall a xs. Elem a xs => Union ((:~:) a) xs
elemEv = elemEv'
