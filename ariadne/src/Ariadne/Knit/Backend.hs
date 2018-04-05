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
  :: forall components commandid.
     Components components
  => Knit.ExecContext components
  -> (KnitEvent components commandid -> IO ())
  -> TaskManagerContext (Knit.Value components)
  -> IO (KnitFace components commandid)
createKnitBackend execCtxs putKnitEvent TaskManagerContext{..} = do
  let
    putCommand commandId expr = case resolveProcNames expr of
      Left e -> do
        putKnitEvent $ KnitCommandResultEvent commandId Nothing $
          KnitCommandProcError e
        return Nothing
      Right expr' -> fmap Just . spawnTask $ \taskId -> do
        -- We catch asynchronous exceptions intentionally here, as we don't want
        -- a single command to crash the entire app.
        res <- handle (\e -> return $ KnitCommandException e) $
          either KnitCommandEvalError KnitCommandSuccess <$> do
            Knit.evaluate execCtxs expr'
        putKnitEvent $ KnitCommandResultEvent commandId (Just taskId) res
        case res of
          KnitCommandEvalError e -> throwIO $ EvalErrorException (show $ Knit.ppEvalError e)
          KnitCommandSuccess v -> return v
          KnitCommandException e -> throwIO e
          -- This case is impossible because procedure names are resolved outside of this code.
          KnitCommandProcError _ -> error "impossible happened"
  return $ KnitFace putCommand
  where
    commandProcs = Knit.commandProcs @components
    resolveProcNames =
      Knit.resolveProcNames (\(Some cp) -> Knit.cpName cp) commandProcs
