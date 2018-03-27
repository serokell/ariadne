module Knit.Value where

import Data.Union

import Knit.Utils

data family ComponentValue (components :: [*]) component

type ValueUnion components = Union (ComponentValue components)

newtype Value components =
  Value { getValueUnion :: ValueUnion components components }

deriving instance Eq (ValueUnion components components) => Eq (Value components)
deriving instance Ord (ValueUnion components components) => Ord (Value components)
deriving instance Show (ValueUnion components components) => Show (Value components)

toValue
  :: forall components component.
     Elem components component
  => ComponentValue components component
  -> Value components
toValue = Value . uliftElem

fromValue
  :: forall components component.
     Elem components component
  => Value components
  -> Maybe (ComponentValue components component)
fromValue = umatchElem . getValueUnion
