module Ariadne.Util
       ( atomicRunStateIORef'
       , postfixLFields
       ) where

import Control.Lens (LensRules, lensField, lensRules, mappingNamer)

atomicRunStateIORef' :: IORef s -> State s a -> IO a
atomicRunStateIORef' ref st = atomicModifyIORef' ref (swap . runState st)

postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ mappingNamer (\s -> [s++"L"])
