module Main
       ( main
       ) where

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
        , msPutPasswordEventToUI = putPasswordEventToUI
        , msKnitFaceToUI = \uiFace knitFace _ -> knitFaceToUI uiFace knitFace
        , msUiExecContext = const $ Base ()
        }
