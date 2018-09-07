module Main where

import Control.Monad.Component (ComponentM)
import NType (N(..))

import Ariadne.Config.TH (getCommitHash)
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
    mainSettings :: MainSettings UiComponents UiFace UiLangFace
    mainSettings = MainSettings
        { msCommitHash = $(getCommitHash)
        , msCreateUI = createUI
        , msPutWalletEventToUI = putWalletEventToUI
        , msPutCardanoEventToUI = putCardanoEventToUI
        , msPutUpdateEventToUI = Just putUpdateEventToUI
        , msPutPasswordEventToUI = putPasswordEventToUI
        , msKnitFaceToUI = knitFaceToUI
        , msUiExecContext = \uiFace -> Step (Knit.UiExecCtx uiFace, Base ())
        }

    createUI
        :: walletUIFace
        -> CommandHistory
        -> PutPassword
        -> ComponentM (UiFace, UiLangFace -> IO ())
    createUI _walletUIFace history putPass =
        let historyFace = historyToUI history
            features = UiFeatures
                { featureStatus = True
                , featureExport = False
                , featureAccounts = True
                , featureTxHistory = False
                , featureFullRestore = True
                , featureSecretKeyName = "Mnemonic"
                }
        in createAriadneUI features historyFace putPass
