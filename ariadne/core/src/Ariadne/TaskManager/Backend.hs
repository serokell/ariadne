module Ariadne.TaskManager.Backend (createTaskManagerFace) where

import Control.Concurrent.Async
import Control.Lens as L
import Control.Monad.Component (ComponentM, buildComponent_)
import Data.Map as Map

import Ariadne.TaskManager.Face
import Ariadne.Util

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

createTaskManagerFace :: forall v. ComponentM (TaskManagerFace v)
createTaskManagerFace = buildComponent_ "TaskManager" $ do
  idGen <- newTaskIdGenerator
  taskMapVar <- newIORef (TaskMap Map.empty Map.empty)
  let
    spawnTask :: (TaskId -> IO v) -> IO TaskId
    spawnTask action = do
      taskId <- idGen
      a <- async $ action taskId
      atomicRunStateIORef' taskMapVar $ taskAsyncMap . at taskId .= Just a
      -- Wait for the task to finish and clean it up
      void . async $ do
        r <- waitCatch a
        removeTask taskId r
      return taskId

    lookupTask :: TaskId -> IO (Maybe (Async v))
    lookupTask taskId = (Map.lookup taskId . _taskAsyncMap) <$> readIORef taskMapVar

    removeTask :: TaskId -> (Either SomeException v) -> IO ()
    removeTask taskId r = atomicRunStateIORef' taskMapVar $ do
      taskMap <- taskAsyncMap <%= delete taskId
      if Map.null taskMap
      then resultCacheMap .= Map.empty
      else resultCacheMap . at taskId .= Just r

    lookupCache :: TaskId -> IO (Maybe (Either SomeException v))
    lookupCache taskId = (Map.lookup taskId . _resultCacheMap) <$> readIORef taskMapVar

  return TaskManagerFace{..}
