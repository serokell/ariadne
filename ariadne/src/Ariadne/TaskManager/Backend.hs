module Ariadne.TaskManager.Backend (createTaskManagerFace) where

import Universum

import Control.Lens as L
import Data.Map as Map
import Control.Concurrent.Async

import Ariadne.TaskManager.Face

newTaskIdGenerator :: IO (IO TaskId)
newTaskIdGenerator = do
  taskIdVar <- newIORef 0
  let
    newTaskId = atomicModifyIORef' taskIdVar (\tid -> (succ tid, TaskId tid))
  return newTaskId

data TaskMap v = TaskMap
  { _resultCacheMap :: Map TaskId (Either SomeException v)
  , _taskAsyncMap :: Map TaskId (Async v)
  }
makeLenses 'TaskMap

createTaskManagerFace :: forall v. IO (TaskManagerFace v)
createTaskManagerFace = do
  idGen <- newTaskIdGenerator
  taskMapVar <- newIORef (TaskMap Map.empty Map.empty)
  let
    spawnTask :: (TaskId -> IO v) -> IO TaskId
    spawnTask action = do
      taskId <- idGen
      a <- async $ action taskId
      runStateIORef taskMapVar $ taskAsyncMap . at taskId .= Just a
      -- Wait for the task to finish and clean it up
      void . async $ do
        r <- waitCatch a
        removeTask taskId r
      return taskId

    lookupTask :: TaskId -> IO (Maybe (Async v))
    lookupTask taskId = (Map.lookup taskId . _taskAsyncMap) <$> readIORef taskMapVar

    removeTask :: TaskId -> (Either SomeException v) -> IO ()
    removeTask taskId r = runStateIORef taskMapVar $ do
      taskMap <- taskAsyncMap <%= delete taskId
      if Map.null taskMap
      then resultCacheMap .= Map.empty
      else resultCacheMap . at taskId .= Just r

    lookupCache :: TaskId -> IO (Maybe (Either SomeException v))
    lookupCache taskId = (Map.lookup taskId . _resultCacheMap) <$> readIORef taskMapVar

  return TaskManagerFace{..}
  where
    runStateIORef :: IORef s -> State s a -> IO a
    runStateIORef ref m = atomicModifyIORef' ref (swapTuple . runState m)
    swapTuple (x, y) = (y, x)
