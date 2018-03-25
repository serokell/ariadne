{-# LANGUAGE AllowAmbiguousTypes #-}
module Knit.Utils where

import Data.Union
import Data.Vinyl.TypeLevel

umapConstrained :: forall c f g as. AllConstrained c as => (forall a . c a => f a -> g a) -> Union f as -> Union g as
umapConstrained f = \case
  This a -> This (f a)
  That u -> That (umapConstrained @c f u)
