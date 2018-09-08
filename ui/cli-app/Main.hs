module Main where

import NType (N(..))

import Ariadne.Config.TH (getCommitHash)
import Ariadne.MainTemplate (MainSettings(..), defaultMain)
import Ariadne.UI.Cli
import Ariadne.UI.Cli.Face (UiLangFace)

import Glue

type UiComponents = '[]

main :: IO ()
main = defaultMain mainSettings
  where
    mainSettings :: MainSettings UiComponents UiFace UiLangFace
    mainSettings = MainSettings
        { msCommitHash = $(getCommitHash)
        , msCreateUI = const $ const createAriadneUI
        , msPutWalletEventToUI = putWalletEventToUI
        , msPutCardanoEventToUI = putCardanoEventToUI
        , msPutUpdateEventToUI = Just putUpdateEventToUI
        , msKnitFaceToUI = knitFaceToUI
        , msUiExecContext = const $ Base ()
        }
