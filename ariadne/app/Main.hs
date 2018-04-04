module Main where

import Control.Concurrent.Async
import IiExtras
import Prelude

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
  mkWalletFace <- createWalletBackend
  let
    uiAction, knitAction, cardanoAction :: IO ()
    uiAction = mkUiAction (knitFaceToUI knitFace)
    knitAction = mkKnitAction knitExecContext (putKnitEventToUI uiFace)
    cardanoAction = mkCardanoAction (putLogMessage uiFace)

    walletFace :: WalletFace
    walletFace = mkWalletFace runCardanoMode (putWalletEventToUI uiFace)

    helpData = generateKnitHelp $ relemsproxy knitExecContext

    knitExecContext :: Rec Knit.ComponentExecContext _
    knitExecContext =
      Knit.CoreExecCtx :&
      Knit.CardanoExecCtx (runNat runCardanoMode) :&
      Knit.WalletExecCtx walletFace :&
      RNil

  putUiEvent uiFace $ UiHelpUpdateData helpData
  uiAction `race_` knitAction `race_` cardanoAction
