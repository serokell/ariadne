module Knit.Inflate where

import IiExtras

import Knit.Value
import Knit.Syntax

class ComponentInflate components component where
  componentInflate
    :: ComponentValue components component
    -> Expr CommandName components

inflate
  :: forall components.
     AllConstrained (ComponentInflate components) components
  => Value components
  -> Expr CommandName components
inflate = ufold @(ComponentInflate components) componentInflate . getValueUnion
