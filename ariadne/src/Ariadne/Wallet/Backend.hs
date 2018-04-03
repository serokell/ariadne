module Ariadne.Wallet.Backend
  ( WalletFace(..)
  , createWalletBackend
  ) where

import Universum

import Pos.Client.KeyStorage (getSecretDefault, modifySecretDefault)
import Pos.Crypto
import Pos.Util (runGen)
import Pos.Util.UserSecret
import Test.QuickCheck (arbitrary)
import IiExtras

import Ariadne.Wallet.Face

createWalletBackend :: IO ((CardanoMode :~> IO) -> (WalletEvent -> IO ()) -> WalletFace)
createWalletBackend = do
  walletSelRef <- newIORef Nothing
  return $ \(Nat runCardanoMode) sendWalletEvent ->
    fix $ \this -> WalletFace
      { walletAddAccount = addAccount this runCardanoMode
      , walletRefreshUserSecret =
          refreshUserSecret walletSelRef runCardanoMode sendWalletEvent
      , walletSelect = select this walletSelRef
      }

refreshUserSecret
  :: IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> (WalletEvent -> IO ())
  -> IO ()
refreshUserSecret walletSelRef runCardanoMode sendWalletEvent = do
  walletSel <- readIORef walletSelRef
  us <- runCardanoMode getSecretDefault
  sendWalletEvent (WalletUserSecretSetEvent us walletSel)

addAccount :: WalletFace -> (CardanoMode ~> IO) -> IO ()
addAccount WalletFace{..} runCardanoMode = do
    runCardanoMode $ modifySecretDefault $ \us ->
        let (newUS, wus) = ensureWalletExists us
            addAccountPure :: [(Word32, Text)] -> [(Word32, Text)]
            addAccountPure accounts =
                accounts <> [(fromIntegral (length accounts), "account")]
            newWallet :: WalletUserSecret
            newWallet = wus & wusAccounts %~ addAccountPure
         in newUS & usWallet .~ Just newWallet
    walletRefreshUserSecret

select
  :: WalletFace
  -> IORef (Maybe WalletSelection)
  -> WalletSelectAction
  -> IO ()
select WalletFace{..} walletSelRef act = do
  case act of
    WalletDeselect -> atomicWriteIORef walletSelRef Nothing
    WalletSelectByIndex{..} -> atomicWriteIORef walletSelRef $ Just $
      WalletSelection { wsWalletIndex = wsaWalletIndex, wsPath = wsaPath }
  walletRefreshUserSecret

ensureWalletExists :: UserSecret -> (UserSecret, WalletUserSecret)
ensureWalletExists us =
    case us ^. usWallet of
        Just wus -> (us, wus)
        Nothing -> (us & usWallet .~ Just emptyWallet, emptyWallet)
  where
    emptyWallet :: WalletUserSecret
    emptyWallet =
        WalletUserSecret
            { _wusRootKey = noPassEncrypt $ runGen arbitrary
            , _wusWalletName = "patak"
            , _wusAccounts = []
            , _wusAddrs = []
            }
