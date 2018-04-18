module Main where

import Universum

import Control.Concurrent.Async
import IiExtras
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Cardano.Backend
import Ariadne.Cardano.Face (CardanoFace(..))
import Ariadne.Help
import Ariadne.Knit.Backend
import Ariadne.TaskManager.Backend
import Ariadne.UI.Vty
import Ariadne.UI.Vty.Face
import Ariadne.Wallet.Backend

import qualified Ariadne.Cardano.Knit as Knit
import qualified Ariadne.TaskManager.Knit as Knit
import qualified Ariadne.Wallet.Knit as Knit
import qualified Knit

import Glue

main :: IO ()
main = do
  (uiFace, mkUiAction) <- createAriadneUI
  (cardanoFace, mkCardanoAction) <- createCardanoBackend
  let CardanoFace { cardanoRunCardanoMode = runCardanoMode
                  } = cardanoFace
  taskManagerFace <- createTaskManagerFace
  mkWallet <- createWalletBackend

  let
    walletFace :: WalletFace
    walletInitAction :: IO ()
    (walletFace, walletInitAction) =
      mkWallet cardanoFace (putWalletEventToUI uiFace)

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

    knitFace = createKnitBackend knitExecContext taskManagerFace

    uiAction, cardanoAction :: IO ()
    uiAction = mkUiAction (knitFaceToUI uiFace knitFace)
    cardanoAction = mkCardanoAction (putCardanoEventToUI uiFace)

    initAction :: IO ()
    initAction = concurrently_ walletInitAction helpInitAction

    serviceAction :: IO ()
    serviceAction = uiAction `race_` cardanoAction

  concurrently_ initAction serviceAction
