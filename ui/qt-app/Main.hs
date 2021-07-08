module Main
       ( main
       ) where

import Control.Monad.Component (ComponentM)
import NType (N(..))

import Ariadne.Config.TH (getCommitHash)
import Ariadne.Config.UI (UIConfig(..))
import Ariadne.Logging (Logging)
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
        , msPutBackendErrorToUI = putBackendErrorToUI
        }

    createUI
        :: WalletUIFace
        -> Logging
        -> CommandHistory
        -> PutPassword
        -> UIConfig
        -> ComponentM (UiFace, UiLangFace -> IO ())
    createUI WalletUIFace{..} _logging history putPass UIConfig{..} =
        let
          historyFace = historyToUI history
          uiWalletFace = UiWalletFace
            { uiGenerateMnemonic = walletGenerateMnemonic
            , uiDefaultEntropySize = walletDefaultEntropySize
            , uiValidateAddress = walletValidateAddress
            , uiValidateCoin = walletValidateCoin
            , uiCoinPrecision = walletCoinPrecision
            }
        in createAriadneUI uiWalletFace historyFace putPass ucNoConfirm
