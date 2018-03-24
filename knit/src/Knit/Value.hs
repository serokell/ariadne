module Knit.Value where

import Data.Union

data family ComponentValue v component

type ValueUnion v = Union (ComponentValue v)

newtype Value components =
  Value { getValueUnion :: ValueUnion (Value components) components }
