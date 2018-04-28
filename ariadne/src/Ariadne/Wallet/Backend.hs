module Ariadne.Wallet.Backend
  ( WalletFace(..)
  , createWalletBackend
  ) where

import Universum

import Data.Constraint (withDict)
import IiExtras ((:~>)(..))
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Wallet.Backend.Balance
import Ariadne.Wallet.Backend.KeyStorage
import Ariadne.Wallet.Backend.Restore
import Ariadne.Wallet.Backend.Tx
import Ariadne.Wallet.Face

createWalletBackend :: WalletConfig -> IO
  (
    CardanoFace ->
    (WalletEvent -> IO ()) ->
    ((Doc -> IO ()) -> WalletFace, IO ())
  )
createWalletBackend walletConfig = do
  walletSelRef <- newIORef Nothing
  return $ \cf@CardanoFace {..} sendWalletEvent ->
    let
      Nat runCardanoMode = cardanoRunCardanoMode
      withDicts = withDict cardanoConfigurations . withDict cardanoCompileInfo
      mkWalletFace putCommandOutput =
         withDicts $ fix $ \this -> WalletFace
          { walletAddAddress = addAddress this walletSelRef runCardanoMode
          , walletAddAccount = addAccount this walletSelRef runCardanoMode
          , walletAddWallet = addNewWallet walletConfig this runCardanoMode
          , walletRestore = restoreWallet this runCardanoMode
          , walletRefreshUserSecret =
              refreshUserSecret walletSelRef runCardanoMode sendWalletEvent
          , walletSelect = select this walletSelRef runCardanoMode
          , walletSend =
              sendTx this cf walletSelRef putCommandOutput
          , walletSelection = readIORef walletSelRef
          , walletBalance = do
              addrs <- getSelectedAddresses this walletSelRef runCardanoMode
              runCardanoMode $ getBalance addrs
          }
      initWalletAction =
        refreshUserSecret walletSelRef runCardanoMode sendWalletEvent
    in
      (mkWalletFace, initWalletAction)
