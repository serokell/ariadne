module Ariadne.Knit.Backend
  ( KnitFace(..)
  , createKnitBackend
  ) where

import Universum hiding (atomically)

import Control.Exception
import Data.Vinyl.TypeLevel
import IiExtras
import Text.PrettyPrint.ANSI.Leijen (Doc)

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
  => ((Doc -> IO ()) -> Knit.ExecContext IO components)
  -> TaskManagerFace (Knit.Value components)
  -> KnitFace components
createKnitBackend mkExecCtxs TaskManagerFace{..} =
  let
    putCommand KnitCommandHandle{..} expr = case resolveProcNames expr of
      Left e -> do
        putCommandResult Nothing $ KnitCommandProcError e
        return Nothing
      Right expr' -> fmap Just . spawnTask $ \taskId -> do
        let execCtxs = mkExecCtxs (putCommandOutput taskId)
        -- We catch asynchronous exceptions intentionally here to send them to UI and
        -- rethrow them afterwards.
        res <- try $ Knit.evaluate execCtxs expr'
        case res of
          Left e -> do
            putCommandResult (Just taskId) $ KnitCommandException e
            throwIO e
          Right (Left e) -> do
            putCommandResult (Just taskId) $ KnitCommandEvalError e
            throwIO $ EvalErrorException (show $ Knit.ppEvalError e)
          Right (Right v) -> do
            putCommandResult (Just taskId) $ KnitCommandSuccess v
            return v
  in KnitFace putCommand
  where
    commandProcs = Knit.commandProcs @components
    resolveProcNames =
      Knit.resolveProcNames (\(Some cp) -> Knit.cpName cp) commandProcs
