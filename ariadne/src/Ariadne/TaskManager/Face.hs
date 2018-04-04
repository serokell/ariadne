module Ariadne.TaskManager.Face where

import Universum

import Control.Concurrent.Async
import Control.Monad.Trans.Reader (ReaderT)
import Control.Exception (Exception, SomeException)

-- | This type reperesents a unique command identifier.
-- The uniqueness is handled by the backend: each time a command is spawned
-- it is assigned a new unique CommandId
data TaskId = TaskId Int
  deriving (Eq, Ord, Show)

-- | Possible outcomes of running a command. It either returns a value to us
-- or fails with an exception
data CommandResult v
  = CommandSuccess v
  | CommandFailure SomeException

-- | The process manager context is a map from CommandId to an async object that
-- returns the result of that command.
data TaskManagerContext v
  = TaskManagerContext
  { spawnTask :: (TaskId -> IO v) -> IO TaskId
  , lookupTask :: TaskId -> IO (Maybe (Async v))
  , lookupCache :: TaskId -> IO (Maybe (Either SomeException v))
  }

newtype TaskManagerM v a = TaskManagerM (ReaderT (TaskManagerContext v) IO a)

newtype EvalErrorException = EvalErrorException Text
  deriving Show
instance Exception EvalErrorException

data NoTaskException = NoTaskException
  deriving Show
instance Exception NoTaskException
