module Main where

import Universum

import Control.Concurrent.Async
import IiExtras
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Cardano.Backend
import Ariadne.Cardano.Face (CardanoFace(..))
import Ariadne.Config.Ariadne (AriadneConfig(..))
import Ariadne.Config.CLI (getConfig)
import Ariadne.Knit.Backend
import Ariadne.TaskManager.Backend
import Ariadne.UI.Qt
import Ariadne.UX.CommandHistory
import Ariadne.Wallet.Backend

import qualified Ariadne.Cardano.Knit as Knit
import qualified Ariadne.TaskManager.Knit as Knit
import qualified Ariadne.Wallet.Knit as Knit
import qualified Knit

import Glue

type Components = '[Knit.Core, Knit.Cardano, Knit.Wallet, Knit.TaskManager]

main :: IO ()
main = do
  ariadneConfig <- getConfig
  let cardanoConfig = acCardano ariadneConfig
      walletConfig = acWallet ariadneConfig

  history <- openCommandHistory "ariadne_history"
  let historyFace = historyToUI history

  (uiFace, mkUiAction) <- createAriadneUI historyFace
  (cardanoFace, mkCardanoAction) <- createCardanoBackend cardanoConfig
  let CardanoFace { cardanoRunCardanoMode = runCardanoMode
                  } = cardanoFace
  taskManagerFace <- createTaskManagerFace
  mkWallet <- createWalletBackend walletConfig

  let
    mkWalletFace :: (Doc -> IO ()) -> WalletFace
    walletInitAction :: IO ()
    (mkWalletFace, walletInitAction) =
      mkWallet cardanoFace (putWalletEventToUI uiFace)

    knitExecContext :: (Doc -> IO ()) -> Knit.ExecContext IO Components
    knitExecContext putCommandOutput =
      Knit.CoreExecCtx (putCommandOutput . Knit.ppValue) :&
      Knit.CardanoExecCtx (runNat runCardanoMode) :&
      Knit.WalletExecCtx (mkWalletFace putCommandOutput) :&
      Knit.TaskManagerExecCtx taskManagerFace :&
      RNil

    knitFace = createKnitBackend knitExecContext taskManagerFace

    uiAction, cardanoAction :: IO ()
    uiAction = mkUiAction (knitFaceToUI uiFace knitFace)
    cardanoAction = mkCardanoAction (putCardanoEventToUI uiFace)

    initAction :: IO ()
    initAction = walletInitAction

    serviceAction :: IO ()
    serviceAction = uiAction `race_` cardanoAction

  withAsync initAction $ \_ -> serviceAction