module Ariadne.CommandId where

import Data.Unique
import Prelude

-- A unique identifier assigned to each command, needed to associate it with
-- the result of its execution.
newtype CommandId = CommandId Unique
  deriving (Eq)

newCommandId :: IO CommandId
newCommandId = CommandId <$> newUnique
