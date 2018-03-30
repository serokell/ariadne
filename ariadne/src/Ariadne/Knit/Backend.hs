module Ariadne.Knit.Backend
  ( KnitFace(..)
  , createKnitBackend
  ) where

import Control.Concurrent.STM
import Control.Exception (handle)
import Data.Unique
import IiExtras
import Universum hiding (atomically)

import Ariadne.CommandId
import Ariadne.Knit.Face
import qualified Knit

type CommandQueue components =
  TBQueue (Knit.Expr Knit.CommandName components, CommandId)

type Components components =
  ( KnownSpine components
  , AllConstrained (Knit.ComponentCommandProcs components) components
  , AllConstrained (Knit.ComponentCommandExec IO components) components
  , AllConstrained (Knit.ComponentLitToValue components) components
  , Ord (Knit.Value components)
  )

type KnitAction components =
     Knit.ExecContext components
  -> (KnitEvent components -> IO ())
  -> IO ()

createKnitBackend
  :: forall components.
     Components components
  => IO (KnitFace components, KnitAction components)
createKnitBackend = do
  commandQueue <- newTBQueueIO 100
  let
    putCommand expr = do
      uid <- CommandId <$> newUnique
      atomically $ writeTBQueue commandQueue (expr, uid)
      return uid
  return (KnitFace putCommand, runKnit commandQueue)

runKnit
  :: forall components.
     (Components components)
  => CommandQueue components
  -> KnitAction components
runKnit commandQueue execCtxs putKnitEvent = do
  forever $ do
    (expr, commandId) <- liftIO . atomically $ readTBQueue commandQueue
    -- TODO @Hithroc: process management
    res <-
      -- We catch asynchronous exceptions intentionally here, as we don't want
      -- a single command to crash the entire app.
      handle (\e -> return $ KnitCommandException e) $
        case resolveProcNames expr of
          Left e -> return $ KnitCommandProcError e
          Right expr' ->
            either KnitCommandEvalError KnitCommandSuccess <$>
              Knit.evaluate execCtxs expr'
    putKnitEvent $ KnitCommandResultEvent commandId res
  where
    commandProcs = Knit.commandProcs @components
    resolveProcNames =
      Knit.resolveProcNames (\(Some cp) -> Knit.cpName cp) commandProcs
