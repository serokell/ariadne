module Main where

import Control.Monad.Component (ComponentM)
import NType (N(..))

import Ariadne.Config.TH (getCommitHash)
import Ariadne.MainTemplate (MainSettings(..), defaultMain)
import Ariadne.UI.Qt
import Ariadne.UI.Qt.Face (UiLangFace, UiWalletFace(..))
import Ariadne.UX.CommandHistory
import Ariadne.UX.PasswordManager
import Ariadne.Wallet.Face (WalletUIFace(..))

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
        , msPutPasswordEventToUI = putPasswordEventToUI
        , msKnitFaceToUI = knitFaceToUI
        , msUiExecContext = const $ Base ()
        }

    createUI
        :: WalletUIFace
        -> CommandHistory
        -> PutPassword
        -> ComponentM (UiFace, UiLangFace -> IO ())
    createUI WalletUIFace{..} history putPass =
        let
          historyFace = historyToUI history
          uiWalletFace = UiWalletFace
            { uiGenerateMnemonic = walletGenerateMnemonic
            , uiDefaultEntropySize = walletDefaultEntropySize
            }
        in createAriadneUI uiWalletFace historyFace putPass
