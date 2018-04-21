module Ariadne.Util (atomicRunStateIORef') where

import Control.Monad.Trans.State (State, runState)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Tuple (swap)
import Prelude (IO, (.))

-- | Atomically modifies the contents of an 'IORef' using the provided 'State'
-- action. Forces both the value stored in the 'IORef' as well as the value
-- returned.
atomicRunStateIORef' :: IORef s -> State s a -> IO a
atomicRunStateIORef' ref st = atomicModifyIORef' ref (swap . runState st)
