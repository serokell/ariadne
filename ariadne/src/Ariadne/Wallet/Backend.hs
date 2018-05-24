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
  -- TODO: Do I need to close session on exit?
  acidDB <- openLocalStateFrom walletAcidDbPathPlaceholder (mempty @DB)
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
          { walletNewAddress = newAddress acidDB this walletSelRef chain
          , walletNewAccount = newAccount acidDB this walletSelRef Nothing
          , walletNewWallet = newWallet acidDB walletConfig this runCardanoMode
          , walletRestore = restoreWallet this runCardanoMode
          , walletRestoreFromFile = restoreFromKeyFile this runCardanoMode
          , walletRename = renameSelection this walletSelRef runCardanoMode
          , walletRemove = removeSelection acidDB this walletSelRef runCardanoMode
          , walletRefreshState =
              refreshState acidDB walletSelRef sendWalletEvent
          , walletSelect = select this walletSelRef
          , walletSend =
              sendTx this cf walletSelRef putCommandOutput
          , walletGetSelection =
              (,) <$> readIORef walletSelRef <*> runCardanoMode getSecretDefault
          , walletBalance = do
              mWalletSel <- readIORef walletSelRef
              addrs <- getSelectedAddresses this walletSelRef
              runCardanoMode $ getBalance addrs
          }
      initWalletAction =
        refreshUserSecret walletSelRef runCardanoMode sendWalletEvent
    in
      (mkWalletFace, initWalletAction)

-- TODO: Make it configurable
walletAcidDbPathPlaceholder :: FilePath
walletAcidDbPathPlaceholder = ".wallet-db"

-- TODO: make 'append' and 'rewrite' modes for wallet acid-state database.
-- If running append mode (append wallets to existing database) it should be
-- prevalidated at first (no name and key duplicates).
