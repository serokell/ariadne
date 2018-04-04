module Ariadne.Knit.Backend
  ( KnitFace(..)
  , createKnitBackend
  ) where

import Universum hiding (atomically)

import Control.Exception
import Data.Vinyl.TypeLevel
import Control.Exception (handle)
import IiExtras

import Ariadne.Knit.Face
import Ariadne.TaskManager.Face
import qualified Knit

type Components components =
  ( KnownSpine components
  , AllConstrained (Knit.ComponentCommandProcs components) components
  , AllConstrained (Knit.ComponentCommandExec IO components) components
  , AllConstrained (Knit.ComponentLitToValue components) components
  , AllConstrained (Knit.ComponentInflate components) components
  , AllConstrained Knit.ComponentPrinter components
  , Ord (Knit.Value components)
  )

createKnitBackend
  :: forall components.
     Components components
  => Knit.ExecContext components
  -> (KnitEvent components -> IO ())
  -> TaskManagerContext (Knit.Value components)
  -> IO (KnitFace components)
createKnitBackend execCtxs putKnitEvent TaskManagerContext{..} = do
  let
    putCommand expr = spawnTask $ \taskId -> do
      -- We catch asynchronous exceptions intentionally here, as we don't want
      -- a single command to crash the entire app.
      res <- handle (\e -> return $ KnitCommandException e) $
        case resolveProcNames expr of
          Left e -> return $ KnitCommandProcError e
          Right expr' ->
            either KnitCommandEvalError KnitCommandSuccess <$> do
              Knit.evaluate execCtxs expr'
      putKnitEvent $ KnitCommandResultEvent taskId res
      case res of
        KnitCommandProcError _ -> throwIO $ EvalErrorException "meh"
        KnitCommandEvalError e -> throwIO $ EvalErrorException (show $ Knit.ppEvalError e)
        KnitCommandSuccess v -> return v
        KnitCommandException e -> throwIO $ e
  return $ KnitFace putCommand
  where
    commandProcs = Knit.commandProcs @components
    resolveProcNames =
      Knit.resolveProcNames (\(Some cp) -> Knit.cpName cp) commandProcs
