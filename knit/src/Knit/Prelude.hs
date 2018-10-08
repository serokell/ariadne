module Knit.Prelude
       ( module X
       , uprism
       ) where

import Prelude as X

import Control.Lens as X hiding (op, rmap)
import Data.Foldable as X
import Data.Function as X (on)
import Data.Traversable as X
import Data.Void as X
import NType as X

uprism :: forall f xs a. Elem xs a => Prism' (Union f xs) (f a)
uprism = prism' ulift umatch
