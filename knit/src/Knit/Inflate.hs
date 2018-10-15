module Knit.Inflate
       ( ComponentInflate(..)
       , inflate
       ) where

import Knit.Prelude
import Knit.Syntax
import Knit.Value

class ComponentInflate components component where
  componentInflate
    :: ComponentValue components component
    -> Expr NoExt CommandId components

inflate
  :: forall components.
     AllConstrained (ComponentInflate components) components
  => Value components
  -> Expr NoExt CommandId components
inflate = ufoldConstrained @(ComponentInflate components) componentInflate . getValueUnion
