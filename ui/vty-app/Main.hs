module Main
       ( main
       ) where

import Control.Monad.Component (ComponentM)
import NType (N(..))

import Ariadne.Config.TH (getCommitHash)
import Ariadne.Logging (Logging)
import Ariadne.MainTemplate (MainSettings(..), defaultMain)
import Ariadne.UI.Vty
import Ariadne.UI.Vty.Face
import Ariadne.UX.CommandHistory
import Ariadne.UX.PasswordManager

import qualified Ariadne.UI.Vty.Knit as Knit

import Glue

type UiComponents = '[Knit.UI]

main :: IO ()
main = defaultMain mainSettings
  where
    mainSettings :: MainSettings UiComponents (UiFace Vty) (UiLangFace Vty)
    mainSettings = MainSettings
        { msCommitHash = $(getCommitHash)
        , msCreateUI = createUI
        , msPutWalletEventToUI = putWalletEventToUI vtyToUiCurrency
        , msPutCardanoEventToUI = putCardanoEventToUI
        , msPutUpdateEventToUI = Just putUpdateEventToUI
        , msPutPasswordEventToUI = putPasswordEventToUI
        , msKnitFaceToUI = knitFaceToUI
        , msUiExecContext = \uiFace -> Step (Knit.UiExecCtx uiFace, Base ())
        , msPutBackendErrorToUI = \_ _ -> pass
        }

    createUI
        :: walletUIFace
        -> Logging
        -> CommandHistory
        -> PutPassword
        -> ComponentM (UiFace Vty, UiLangFace Vty -> IO ())
    createUI _walletUIFace logging history putPass =
        let historyFace = historyToUI history
            features = UiFeatures
                { featureStatus = True
                , featureExport = False
                , featureAccounts = True
                , featureTxHistory = False
                , featureSecretKeyName = "Mnemonic"
                }
        in createAriadneUI features logging historyFace putPass
