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
  taskManagerFace <- createTaskManagerFace
  mkWallet <- createWalletBackend

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
      Knit.TaskManagerExecCtx taskManagerFace :&
      RNil

    knitFace = createKnitBackend knitExecContext (putKnitEventToUI uiFace) taskManagerFace

    uiAction, cardanoAction :: IO ()
    uiAction = mkUiAction (knitFaceToUI knitFace)
    cardanoAction = mkCardanoAction (putCardanoEventToUI uiFace)

    initAction :: IO ()
    initAction = concurrently_ walletInitAction helpInitAction

    serviceAction :: IO ()
    serviceAction = uiAction `race_` cardanoAction

  concurrently_ initAction serviceAction
