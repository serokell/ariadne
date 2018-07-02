module Ariadne.Wallet.Backend
  ( WalletFace(..)
  , createWalletBackend
  ) where

import Universum

import Data.Acid (openLocalStateFrom, query)
import Data.Constraint (withDict)
import IiExtras ((:~>)(..))
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Cardano.Face
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Wallet.Backend.Balance
import Ariadne.Wallet.Backend.KeyStorage
import Ariadne.Wallet.Backend.Restore
import Ariadne.Wallet.Backend.Tx
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (Snapshot(..), defDB)
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
  acidDb <- openLocalStateFrom walletAcidDbPathPlaceholder defDB
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
          { walletNewAddress = newAddress acidDb this walletSelRef runCardanoMode
          , walletNewAccount = newAccount acidDb this walletSelRef
          , walletNewWallet = newWallet acidDb walletConfig this runCardanoMode
          , walletRestore = restoreWallet acidDb this runCardanoMode
          , walletRestoreFromFile = restoreFromKeyFile acidDb this runCardanoMode
          , walletRename = renameSelection acidDb this walletSelRef
          , walletRemove = removeSelection acidDb this walletSelRef runCardanoMode
          , walletRefreshState =
              refreshState acidDb walletSelRef sendWalletEvent
          , walletSelect = select acidDb this walletSelRef
          , walletSend =
              sendTx acidDb this cf walletSelRef putCommandOutput
          , walletGetSelection =
              (,) <$> readIORef walletSelRef <*> query acidDb Snapshot
          , walletBalance = do
            -- TODO: get balance from acidDb
              addrs <- getSelectedAddresses acidDb this walletSelRef
              runCardanoMode $ getBalance addrs
          }
      initWalletAction =
        refreshState acidDb walletSelRef sendWalletEvent
    in
      (mkWalletFace, initWalletAction)

-- TODO: Make it configurable
walletAcidDbPathPlaceholder :: FilePath
walletAcidDbPathPlaceholder = ".wallet-db"

-- TODO: make 'append' and 'rewrite' modes for wallet acid-state database.
-- If running append mode (append wallets to existing database) it should be
-- prevalidated at first (no name and key duplicates).
