module Main where

import Universum

import Control.Monad.Component (ComponentM)
import IiExtras

import Ariadne.Config.TH (getCommitHash)
import Ariadne.MainTemplate (MainSettings(..), defaultMain)
import Ariadne.UI.Vty
import Ariadne.UI.Vty.Face
import Ariadne.UX.CommandHistory

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
        , msKnitFaceToUI = knitFaceToUI
        , msUiExecContext = \uiFace -> Knit.UiExecCtx uiFace :& RNil
        }

    createUI :: CommandHistory -> ComponentM (UiFace, UiLangFace -> IO ())
    createUI history =
        let historyFace = historyToUI history
            features = UiFeatures
                { featureStatus = True
                , featureExport = False
                , featureAccounts = True
                , featureTxHistory = False
                , featureFullRestore = True
                , featureSecretKeyName = "Mnemonic"
                }
        in createAriadneUI features historyFace
