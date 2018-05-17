module Knit.Inflate where

import IiExtras

import Knit.Syntax
import Knit.Value

class ComponentInflate components component where
  componentInflate
    :: ComponentValue components component
    -> Expr CommandId components

inflate
  :: forall components.
     AllConstrained (ComponentInflate components) components
  => Value components
  -> Expr CommandId components
inflate = ufold @(ComponentInflate components) componentInflate . getValueUnion
