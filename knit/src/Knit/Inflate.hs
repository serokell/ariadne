module Knit.Inflate where

import Data.Union
import Data.Vinyl.TypeLevel

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
inflate = go . getValueUnion
  where
    go
      :: forall components'.
         AllConstrained (ComponentInflate components) components'
      => Union (ComponentValue components) components'
      -> Expr CommandName components
    go (This v) = componentInflate v
    go (That v) = go v
