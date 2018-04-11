module Ariadne.Wallet.Backend
  ( WalletFace(..)
  , createWalletBackend
  ) where

import Universum

import Data.Constraint (withDict)
import IiExtras ((:~>)(..))

import Ariadne.Wallet.Backend.KeyStorage
import Ariadne.Wallet.Backend.Tx
import Ariadne.Wallet.Face

createWalletBackend :: IO
  (
    CardanoFace ->
    (WalletEvent -> IO ()) ->
    (WalletFace, IO ())
  )
createWalletBackend = do
  walletSelRef <- newIORef Nothing
  return $ \cf@CardanoFace {..} sendWalletEvent ->
    let
      Nat runCardanoMode = cardanoRunCardanoMode
      withDicts = withDict cardanoConfigurations . withDict cardanoCompileInfo
      walletFace =
         withDicts $ fix $ \this -> WalletFace
          { walletAddAccount = addAccount this walletSelRef runCardanoMode
          , walletAddWallet = addWallet this runCardanoMode
          , walletRefreshUserSecret =
              refreshUserSecret walletSelRef runCardanoMode sendWalletEvent
          , walletSelect = select this walletSelRef runCardanoMode
          , walletSend = sendTx this cf walletSelRef
          }
      initWalletAction =
        walletRefreshUserSecret walletFace
    in
      (walletFace, initWalletAction)
