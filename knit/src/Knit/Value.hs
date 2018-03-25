module Knit.Value where

import Data.Union

data family ComponentValue (components :: [*]) component

type ValueUnion components = Union (ComponentValue components)

newtype Value components =
  Value { getValueUnion :: ValueUnion components components }
