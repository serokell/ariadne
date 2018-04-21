module Ariadne.Wallet.Backend
  ( WalletFace(..)
  , createWalletBackend
  ) where

import Universum

import Control.Natural ((:~>)(..))
import Data.Constraint (withDict)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Wallet.Backend.KeyStorage
import Ariadne.Wallet.Backend.Tx
import Ariadne.Wallet.Face

createWalletBackend :: IO
  (
    CardanoFace ->
    (WalletEvent -> IO ()) ->
    ((Doc -> IO ()) -> WalletFace, IO ())
  )
createWalletBackend = do
  walletSelRef <- newIORef Nothing
  return $ \cf@CardanoFace {..} sendWalletEvent ->
    let
      NT runCardanoMode = cardanoRunCardanoMode
      withDicts = withDict cardanoConfigurations . withDict cardanoCompileInfo
      mkWalletFace putCommandOutput =
         withDicts $ fix $ \this -> WalletFace
          { walletAddAddress = addAddress this walletSelRef runCardanoMode
          , walletAddAccount = addAccount this walletSelRef runCardanoMode
          , walletAddWallet = addWallet this runCardanoMode
          , walletRefreshUserSecret =
              refreshUserSecret walletSelRef runCardanoMode sendWalletEvent
          , walletSelect = select this walletSelRef runCardanoMode
          , walletSend =
              sendTx this cf walletSelRef putCommandOutput
          , walletSelection = readIORef walletSelRef
          }
      initWalletAction =
        refreshUserSecret walletSelRef runCardanoMode sendWalletEvent
    in
      (mkWalletFace, initWalletAction)
