module Ariadne.TaskManager.Backend (createTaskManagerBackend) where

import Universum

import Data.Map as Map
import Control.Concurrent.Async

import IiExtras
import Ariadne.TaskManager.Face

newTaskIdGenerator :: IO (IO TaskId)
newTaskIdGenerator = do
  taskIdVar <- newIORef 0
  let
    newTaskId = TaskId <$> atomicModifyIORef' taskIdVar (\tid -> (succ tid, tid))
  return newTaskId

createTaskManagerContext :: forall v. IO (TaskManagerContext v)
createTaskManagerContext = do
  idGen <- newTaskIdGenerator
  taskMapVar <- newIORef Map.empty
  cacheMapVar <- newIORef Map.empty
  let
    spawnTask action = do
      taskId <- idGen
      a <- async $ action taskId
      atomicModifyIORef' taskMapVar (\taskMap -> (insert taskId a taskMap, ()))
      -- Wait for the task to finish and clean it up
      void . async $ do
        r <- waitCatch a
        removeTask taskId r
      return taskId

    lookupTask taskId = Map.lookup taskId <$> readIORef taskMapVar

    removeTask taskId r = do
      atomicModifyIORef' taskMapVar (\taskMap -> (delete taskId taskMap, ()))
      taskMap <- readIORef taskMapVar
      if Map.null taskMap
      then clearCache
      else insertCache taskId r

    insertCache taskId val =
      atomicModifyIORef' cacheMapVar (\cacheMap -> (insert taskId val cacheMap, ()))

    lookupCache taskId = Map.lookup taskId <$> readIORef cacheMapVar

    clearCache =
      atomicModifyIORef' cacheMapVar (\_ -> (Map.empty, ()))
  return TaskManagerContext{..}

createTaskManagerBackend :: IO (TaskManagerM v :~> IO, TaskManagerContext v)
createTaskManagerBackend = do
  ctx <- createTaskManagerContext
  return (Nat $ \(TaskManagerM m) -> flip runReaderT ctx m, ctx)
