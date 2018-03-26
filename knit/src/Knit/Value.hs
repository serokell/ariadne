module Knit.Value where

import Data.Union
import Data.Type.Equality

import Knit.Utils

data family ComponentValue (components :: [*]) component

type ValueUnion components = Union (ComponentValue components)

newtype Value components =
  Value { getValueUnion :: ValueUnion components components }

instance
    ( AllConstrainedF Eq (ComponentValue components) components
    , KnownSpine components
    ) => Eq (Value components)
  where
    a == b =
      case deduceEqUnion @(ComponentValue components) @components of
        Dict -> getValueUnion a == getValueUnion b

instance
    ( AllConstrainedF Eq (ComponentValue components) components
    , AllConstrainedF Ord (ComponentValue components) components
    , KnownSpine components
    ) => Ord (Value components)
  where
    compare a b =
      case deduceOrdUnion @(ComponentValue components) @components of
        Dict -> getValueUnion a `compare` getValueUnion b

instance
    ( AllConstrainedF Show (ComponentValue components) components
    , KnownSpine components
    ) => Show (Value components)
  where
    showsPrec n a =
      case deduceShowUnion @(ComponentValue components) @components of
        Dict -> showsPrec n (getValueUnion a)

toValue
  :: forall components component.
     Elem components component
  => ComponentValue components component
  -> Value components
toValue v = Value $ umap (\Refl -> v) (elemEv @components @component)

fromValue
  :: forall components component.
     Elem components component
  => Value components
  -> Maybe (ComponentValue components component)
fromValue = go (elemEv @components @component) . getValueUnion
  where
    go
      :: forall components'.
         Union ((:~:) component) components'
      -> Union (ComponentValue components) components'
      -> Maybe (ComponentValue components component)
    go (This Refl) (This v) = Just v
    go (That i) (That v) = go i v
    go _ _ = Nothing
