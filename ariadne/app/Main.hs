module Main where

import Control.Concurrent.Async
import IiExtras
import Prelude
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Cardano.Backend
import Ariadne.Help
import Ariadne.Knit.Backend
import Ariadne.UI.Vty
import Ariadne.UI.Vty.Face
import Ariadne.Wallet.Backend

import qualified Ariadne.Cardano.Knit as Knit
import qualified Ariadne.Wallet.Knit as Knit
import qualified Knit

import Glue

main :: IO ()
main = do
  (uiFace, mkUiAction) <- createAriadneUI
  (knitFace, mkKnitAction) <- createKnitBackend
  (runCardanoMode, mkCardanoAction) <- createCardanoBackend
  mkWallet <- createWalletBackend
  let
    uiAction, knitAction, cardanoAction :: IO ()
    uiAction = mkUiAction (knitFaceToUI knitFace)
    knitAction = mkKnitAction knitExecContext (putKnitEventToUI uiFace)
    cardanoAction = mkCardanoAction (putCardanoEventToUI uiFace)

    walletFace :: WalletFace
    walletInitAction :: IO ()
    (walletFace, walletInitAction) =
      mkWallet runCardanoMode (putWalletEventToUI uiFace)

    helpData :: [Doc]
    helpData = generateKnitHelp $ relemsproxy knitExecContext

    helpInitAction :: IO ()
    helpInitAction = putUiEvent uiFace $ UiHelpUpdateData helpData

    knitExecContext :: Rec Knit.ComponentExecContext _
    knitExecContext =
      Knit.CoreExecCtx :&
      Knit.CardanoExecCtx (runNat runCardanoMode) :&
      Knit.WalletExecCtx walletFace :&
      RNil

    initAction :: IO ()
    initAction = concurrently_ walletInitAction helpInitAction

    serviceAction :: IO ()
    serviceAction = uiAction `race_` knitAction `race_` cardanoAction

  concurrently_ initAction serviceAction
