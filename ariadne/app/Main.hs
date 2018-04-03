module Main where

import Control.Concurrent.Async
import IiExtras
import Prelude

import Ariadne.Cardano.Backend
import Ariadne.Glue
import Ariadne.Knit.Backend
import Ariadne.UI.Vty
import Ariadne.Wallet.Backend

import qualified Ariadne.Cardano.Knit as Knit
import qualified Ariadne.Wallet.Knit as Knit
import qualified Knit

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

    knitExecContext :: Rec Knit.ComponentExecContext _
    knitExecContext =
      Knit.CoreExecCtx :&
      Knit.CardanoExecCtx (runNat runCardanoMode) :&
      Knit.WalletExecCtx walletFace :&
      RNil

  uiAction `race_` knitAction `race_` cardanoAction
