module Ariadne.Knit.Backend
  ( KnitFace(..)
  , createKnitBackend
  ) where

import Universum hiding (atomically)

import Control.Exception
import Data.Vinyl.TypeLevel
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
  :: forall commandid components.
     Components components
  => Knit.ExecContext components
  -> (KnitEvent commandid components -> IO ())
  -> TaskManagerFace (Knit.Value components)
  -> KnitFace commandid components
createKnitBackend execCtxs putKnitEvent TaskManagerFace{..} =
  let
    putCommand commandId expr = case resolveProcNames expr of
      Left e -> do
        putKnitEvent $ KnitCommandResultEvent commandId Nothing $
          KnitCommandProcError e
        return Nothing
      Right expr' -> fmap Just . spawnTask $ \taskId -> do
        -- We catch asynchronous exceptions intentionally here to send them to UI and
        -- rethrow them afterwards.
        res <- try $ Knit.evaluate execCtxs expr'
        let event = KnitCommandResultEvent commandId (Just taskId)
        case res of
          Left e -> do
            putKnitEvent . event $ KnitCommandException e
            throwIO e
          Right (Left e) -> do
            putKnitEvent . event $ KnitCommandEvalError e
            throwIO $ EvalErrorException (show $ Knit.ppEvalError e)
          Right (Right v) -> return v
  in KnitFace putCommand
  where
    commandProcs = Knit.commandProcs @components
    resolveProcNames =
      Knit.resolveProcNames (\(Some cp) -> Knit.cpName cp) commandProcs
