module Main where

import Universum

import Control.Monad.Component (ComponentM)
import NType (N (..))

import Ariadne.Config.TH (getCommitHash)
import Ariadne.MainTemplate (MainSettings(..), defaultMain)
import Ariadne.UI.Qt
import Ariadne.UI.Qt.Face (UiLangFace)
import Ariadne.UX.CommandHistory

import Glue

type UiComponents = '[]

main :: IO ()
main = defaultMain mainSettings
  where
    mainSettings :: MainSettings UiComponents UiFace UiLangFace
    mainSettings = MainSettings
        { msCommitHash = $(getCommitHash)
        , msCreateUI = createUI
        , msPutWalletEventToUI = putWalletEventToUI
        , msPutCardanoEventToUI = putCardanoEventToUI
        , msPutUpdateEventToUI = Nothing
        , msKnitFaceToUI = knitFaceToUI
        , msUiExecContext = const $ Base ()
        }

    createUI :: CommandHistory -> ComponentM (UiFace, UiLangFace -> IO ())
    createUI history =
        let historyFace = historyToUI history
        in createAriadneUI historyFace
