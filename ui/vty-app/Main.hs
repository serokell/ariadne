module Main where

import Universum

import Control.Concurrent.Async
import IiExtras
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Cardano.Backend
import Ariadne.Cardano.Face (CardanoFace(..))
import Ariadne.Config.Ariadne (AriadneConfig(..))
import Ariadne.Config.CLI (getConfig)
import Ariadne.Config.History (HistoryConfig(..))
import Ariadne.Config.TH (getCommitHash)
import Ariadne.Knit.Backend
import Ariadne.TaskManager.Backend
import Ariadne.UI.Vty
import Ariadne.Update.Backend
import Ariadne.UX.CommandHistory
import Ariadne.Wallet.Backend

import qualified Ariadne.Cardano.Knit as Knit
import qualified Ariadne.TaskManager.Knit as Knit
import qualified Ariadne.UI.Vty.Knit as Knit
import qualified Ariadne.Wallet.Knit as Knit
import qualified Knit

import Glue

type Components = '[Knit.Core, Knit.Cardano, Knit.Wallet, Knit.TaskManager, Knit.UI]

main :: IO ()
main = do
  ariadneConfig <- getConfig $(getCommitHash)
  let cardanoConfig = acCardano ariadneConfig
      walletConfig = acWallet ariadneConfig
      updateConfig = acUpdate ariadneConfig
      historyConfig = acHistory ariadneConfig

  history <- openCommandHistory $ hcPath historyConfig
  let historyFace = historyToUI history

  (uiFace, mkUiAction) <- createAriadneUI historyFace
  (bHandle, addUs, mkWallet) <- createWalletBackend walletConfig (putWalletEventToUI uiFace)
  (cardanoFace, mkCardanoAction) <- createCardanoBackend cardanoConfig bHandle addUs
  let CardanoFace { cardanoRunCardanoMode = runCardanoMode
                  } = cardanoFace
  taskManagerFace <- createTaskManagerFace

  let
    mkWalletFace :: (Doc -> IO ()) -> WalletFace
    walletInitAction :: IO ()
    (mkWalletFace, walletInitAction) =
      mkWallet cardanoFace

    knitExecContext :: (Doc -> IO ()) -> Knit.ExecContext IO Components
    knitExecContext putCommandOutput =
      Knit.CoreExecCtx (putCommandOutput . Knit.ppValue) :&
      Knit.CardanoExecCtx (runNat runCardanoMode) :&
      Knit.WalletExecCtx walletFace :&
      Knit.TaskManagerExecCtx taskManagerFace :&
      Knit.UiExecCtx uiFace :&
      RNil
      where
        walletFace = mkWalletFace putCommandOutput

    knitFace = createKnitBackend knitExecContext taskManagerFace

    uiAction, cardanoAction, updateCheckAction :: IO ()
    uiAction = mkUiAction (knitFaceToUI uiFace knitFace)
    cardanoAction = mkCardanoAction (putCardanoEventToUI uiFace)

    updateCheckAction = runUpdateCheck updateConfig (putUpdateEventToUI uiFace)

    initAction :: IO ()
    initAction = walletInitAction

    serviceAction :: IO ()
    serviceAction = uiAction `race_` cardanoAction `race_` updateCheckAction

  withAsync initAction $ \_ -> serviceAction
