module Main where

import Control.Concurrent.Async
import IiExtras
import Prelude
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Cardano.Backend

import Ariadne.TaskManager.Backend
import Ariadne.Help
import Ariadne.Knit.Backend
import Ariadne.UI.Vty
import Ariadne.UI.Vty.Face
import Ariadne.Wallet.Backend

import qualified Ariadne.Cardano.Knit as Knit
import qualified Ariadne.Wallet.Knit as Knit
import qualified Ariadne.TaskManager.Knit as Knit
import qualified Knit

import Glue

main :: IO ()
main = do
  (uiFace, mkUiAction) <- createAriadneUI
  (runCardanoMode, mkCardanoAction) <- createCardanoBackend
  (runTaskManagerM, taskManagerContext) <- createTaskManagerBackend
  mkWalletFace <- createWalletBackend

  let
    walletFace :: WalletFace
    walletInitAction :: IO ()
    (walletFace, walletInitAction) =
      mkWallet runCardanoMode (putWalletEventToUI uiFace)

    helpData :: [Doc]
    helpData = generateKnitHelp $ relemsproxy knitExecContext

    helpInitAction :: IO ()
    helpInitAction = putUiEvent uiFace $ UiHelpUpdateData helpData

    knitExecContext :: Rec (Knit.ComponentExecContext _) _
    knitExecContext =
      Knit.CoreExecCtx :&
      Knit.CardanoExecCtx (runNat runCardanoMode) :&
      Knit.WalletExecCtx walletFace :&
      Knit.TaskManagerExecCtx (runNat runTaskManagerM) :&
      RNil
  knitFace <- createKnitBackend knitExecContext (putKnitEventToUI uiFace) taskManagerContext

  let
    uiAction, knitAction, cardanoAction :: IO ()
    uiAction = mkUiAction (knitFaceToUI knitFace)
    knitAction = mkKnitAction knitExecContext (putKnitEventToUI uiFace)
    cardanoAction = mkCardanoAction (putCardanoEventToUI uiFace)
    initAction :: IO ()
    initAction = concurrently_ walletInitAction helpInitAction

    serviceAction :: IO ()
    serviceAction = uiAction `race_` knitAction `race_` cardanoAction

  concurrently_ initAction serviceAction
