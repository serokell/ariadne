module Ariadne.Knit.Backend (createKnitBackend) where

import Universum hiding (atomically)
import Control.Concurrent.STM
import Data.Unique
import Data.Vinyl.TypeLevel
import Control.Exception (handle)

import Ariadne.Face
import qualified Knit

type CommandQueue components =
  TBQueue (Knit.Expr Knit.CommandName components, CommandId)

type Components components =
  ( Knit.KnownSpine components
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
      handle (\e -> return $ CommandException e) $
        case resolveProcNames expr of
          Left e -> return $ CommandProcError e
          Right expr' ->
            either CommandEvalError CommandSuccess <$>
              Knit.evaluate execCtxs expr'
    putKnitEvent $ KnitResultEvent commandId res
  where
    commandProcs = Knit.commandProcs @components
    resolveProcNames =
      Knit.resolveProcNames (\(Knit.Some cp) -> Knit.cpName cp) commandProcs
