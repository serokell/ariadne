{- |

Values manipulated by Knit programs during evaluation/execution.

-}
module Knit.Value where

import Knit.Prelude

-- | The 'ComponentValue' data family defines the value type of a particular
-- component. The first parameter, 'components', provides the ability to
-- reference the overall list of components in the application, and data
-- instances should not match on it. The second parameter, 'component', is the
-- component for which we are defining the data instance.
--
-- @
-- data instance ComponentValue components Core
--   = ValueBool Bool
--   | ValueNumber Scientific
--   | ValueList [Value components]
-- @
--
data family ComponentValue (components :: [*]) component

-- | The 'ValueUnion' type synonym is the sum (union) of component value types.
type ValueUnion components = Union (ComponentValue components) components

-- | 'Value' type represents what commands take as input and return as output.
-- One can think of it as an extensible data type, where each component adds
-- constructors to it. For instance, let's say we have the following data
-- instances:
--
-- @
-- data instance ComponentValue components A
--   = ValueX X
--   | ValueY Y
--
-- data instance ComponentValue components B
--   = ValueM M
--   | ValueN N
-- @
--
-- Then @Value '[A, B]@ is roughly equivalent to:
--
-- data ValueAB
--   = ValueX X
--   | ValueY Y
--   | ValueM M
--   | ValueN N
--
newtype Value components =
  Value { getValueUnion :: ValueUnion components }

deriving instance Eq (ValueUnion components) => Eq (Value components)
deriving instance Ord (ValueUnion components) => Ord (Value components)
deriving instance Show (ValueUnion components) => Show (Value components)

-- | Convert a value of some particular component to a 'Value'.
--
-- @
--            ValueBool True  :: ComponentValue components Core@
--   toValue (ValueBool True) :: Elem components Core => Value components
-- @
toValue
  :: forall components component.
     Elem components component
  => ComponentValue components component
  -> Value components
toValue = Value . ulift

-- | Match on a 'Value', expecting it to belong to a particular component.
-- Returns 'Nothing' if the value belongs to a different component.
--
-- prop> fromValue \@cs \@c . toValue \@cs \@c == Just
fromValue
  :: forall components component.
     Elem components component
  => Value components
  -> Maybe (ComponentValue components component)
fromValue = umatch . getValueUnion
