module Ariadne.Wallet.Backend
  ( WalletFace(..)
  , createWalletBackend
  ) where

import Universum

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.Component (ComponentM, buildComponent_)
import Data.Acid (openLocalStateFrom, query)
import Data.Constraint (withDict)
import IiExtras ((:~>)(..))
import Pos.Util.Future (newInitFuture)
import Pos.Util.UserSecret (UserSecret)
import System.Wlog (logMessage, usingLoggerName)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Ariadne.Cardano.Face
import Ariadne.Config.Wallet (WalletConfig(..))
import Ariadne.Wallet.Backend.KeyStorage
import Ariadne.Wallet.Backend.Restore
import Ariadne.Wallet.Backend.Tx
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState (Snapshot(..), defDB)
import Ariadne.Wallet.Cardano.WalletLayer.Kernel (bracketPassiveWallet)
import Ariadne.Wallet.Cardano.WalletLayer.Types (PassiveWalletLayer(..))
import Ariadne.Wallet.Face


createWalletBackend :: WalletConfig -> (WalletEvent -> IO ()) -> ComponentM
  ( BListenerHandle
  , TVar UserSecret -> IO ()
  , CardanoFace ->
    ((Doc -> IO ()) -> WalletFace, IO ())
  )
createWalletBackend walletConfig sendWalletEvent = buildComponent_ "Wallet" $ do
  walletSelRef <- newIORef Nothing
  -- TODO: Do I need to close session on exit?
  acidDb <- openLocalStateFrom walletAcidDbPathPlaceholder defDB

  -- get the block apply handler
  -- We're doing this dirty hack with initPassiveWallet for now because we don't have
  -- a proper bracket architecture anywhere
  mpw <- newEmptyMVar
  (us :: TVar UserSecret, addUs :: TVar UserSecret -> IO ()) <- newInitFuture "UserSecret"
  bracketPassiveWallet
    (\sev -> usingLoggerName "passive-wallet" . logMessage sev)
    us
    acidDb
    (putMVar mpw)
  PassiveWalletLayer{..} <- takeMVar mpw
  let refresh = refreshState acidDb walletSelRef sendWalletEvent
      applyHook = const refresh <=< _pwlApplyBlocks
      rollbackHook = const refresh <=< _pwlRollbackBlocks

  return (BListenerHandle applyHook rollbackHook, addUs, \cf@CardanoFace {..} ->
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
          , walletBalance = getBalance acidDb walletSelRef
          }
      initWalletAction =
        refreshState acidDb walletSelRef sendWalletEvent
    in
      (mkWalletFace, initWalletAction)
    )

-- TODO: Make it configurable
walletAcidDbPathPlaceholder :: FilePath
walletAcidDbPathPlaceholder = ".wallet-db"

-- TODO: make 'append' and 'rewrite' modes for wallet acid-state database.
-- If running append mode (append wallets to existing database) it should be
-- prevalidated at first (no name and key duplicates).
