module Main where

import Control.Concurrent.Async (waitCatch)
import Control.Monad.Component (ComponentM, runComponentM)
import NType (N (..))
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Knit.Backend
import Ariadne.Knit.Face
import Ariadne.TaskManager.Backend
import Ariadne.TaskManager.Face
import Ariadne.UI.Cli
import Dscp.Wallet.Backend
import Dscp.Wallet.CLI

import qualified Ariadne.TaskManager.Knit as Knit
import qualified Dscp.Wallet.Knit as Knit
import qualified Knit

import Glue

type UiComponents = '[Knit.Core, Knit.TaskManager, Knit.Wallet]

main :: IO ()
main = do
    params <- getWalletCLIParams
    runComponentM "ariadne" (initializeEverything params) id

initializeEverything :: WalletCLIParams -> ComponentM (IO ())
initializeEverything WalletCLIParams{..} = do
    taskManagerFace <- createTaskManagerFace
    walletFace <- createWalletFace wpWitness (void . return)
    (uiFace, mkUiAction) <- createAriadneUI

    let knitExecContext :: (Doc -> IO ()) -> Knit.ExecContext IO UiComponents
        knitExecContext putCommandOutput =
            Knit.CoreExecCtx (putCommandOutput . Knit.ppValue) &:
            Knit.TaskManagerExecCtx taskManagerFace &:
            Knit.WalletExecCtx walletFace &:
            Base ()
          where
            a &: b = Step (a, b)
            infixr &:

        knitFace = createKnitBackend knitExecContext taskManagerFace

        uiAction :: IO ()
        uiAction = case wpKnitCommand of
            Nothing -> mkUiAction (knitFaceToUI uiFace knitFace)
            Just cmd -> case Knit.parse @UiComponents cmd of
                Right expr -> do
                    whenJustM (putKnitCommand knitFace handle expr) $
                        \tid -> whenJustM (lookupTask taskManagerFace tid) $ void . waitCatch
                Left err -> print $ Knit.ppParseError err
              where
                handle = KnitCommandHandle
                    { putCommandResult = \_ -> handleResult
                    , putCommandOutput = \_ doc -> print doc
                    }
                handleResult = \case
                    KnitCommandSuccess v -> print $ Knit.ppValue v
                    KnitCommandEvalError e -> print $ Knit.ppEvalError e
                    KnitCommandProcError e -> print $ Knit.ppResolveErrors e
                    KnitCommandException e -> print $ displayException e


    return uiAction
