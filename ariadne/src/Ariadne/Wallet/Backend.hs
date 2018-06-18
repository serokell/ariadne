module Ariadne.Wallet.Backend
  ( WalletFace(..)
  , createWalletBackend
  ) where

import Universum

import Data.Constraint (withDict)
import IiExtras ((:~>)(..))
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Pos.Client.KeyStorage (getSecretDefault)

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
      withDicts :: ((HasConfigurations, HasCompileInfo) => r) -> r
      withDicts r =
          withDict cardanoConfigurations $
          withDict cardanoCompileInfo $
          r
      mkWalletFace putCommandOutput =
         withDicts $ fix $ \this -> WalletFace
          { walletNewAddress = newAddress this walletSelRef runCardanoMode
          , walletNewAccount = newAccount this walletSelRef runCardanoMode
          , walletNewWallet = newWallet walletConfig this runCardanoMode
          , walletRestore = restoreWallet this runCardanoMode
          , walletRestoreFromFile = restoreFromKeyFile this runCardanoMode
          , walletRename = renameSelection this walletSelRef runCardanoMode
          , walletRemove = removeSelection this walletSelRef runCardanoMode
          , walletRefreshUserSecret =
              refreshUserSecret walletSelRef runCardanoMode sendWalletEvent
          , walletSelect = select this walletSelRef runCardanoMode
          , walletSend =
              sendTx this cf walletSelRef putCommandOutput
          , walletGetSelection =
              (,) <$> readIORef walletSelRef <*> runCardanoMode getSecretDefault
          , walletBalance = do
              addrs <- getSelectedAddresses this walletSelRef runCardanoMode
              runCardanoMode $ getBalance addrs
          }
      initWalletAction =
        refreshUserSecret walletSelRef runCardanoMode sendWalletEvent
    in
      (mkWalletFace, initWalletAction)
