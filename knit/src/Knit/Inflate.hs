module Knit.Inflate where

import Knit.Syntax
import Knit.Value
import Knit.Prelude

class ComponentInflate components component where
  componentInflate
    :: ComponentValue components component
    -> Expr CommandName components

inflate
  :: forall components.
     AllConstrained (ComponentInflate components) components
  => Value components
  -> Expr CommandName components
inflate = ufoldConstrained @(ComponentInflate components) componentInflate . getValueUnion
