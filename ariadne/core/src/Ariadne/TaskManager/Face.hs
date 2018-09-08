module Ariadne.TaskManager.Face where

import Control.Concurrent.Async
import Control.Exception (Exception, SomeException)

-- | This type reperesents a unique task identifier.
-- The uniqueness is handled by the backend: each time a command is spawned
-- it is assigned a new unique TaskId
data TaskId = TaskId Natural
  deriving (Eq, Ord, Show)

-- | The process manager context is a map from CommandId to an async object that
-- returns the result of that command.
data TaskManagerFace v
  = TaskManagerFace
  { spawnTask :: (TaskId -> IO v) -> IO TaskId
  , lookupTask :: TaskId -> IO (Maybe (Async v))
  , lookupCache :: TaskId -> IO (Maybe (Either SomeException v))
  }

newtype EvalErrorException = EvalErrorException Text
  deriving Show
instance Exception EvalErrorException

data NoTaskException = NoTaskException
  deriving Show
instance Exception NoTaskException
