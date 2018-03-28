module Knit.Inflate where

import Data.Vinyl.TypeLevel

import Knit.Value
import Knit.Syntax
import Knit.Utils

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
