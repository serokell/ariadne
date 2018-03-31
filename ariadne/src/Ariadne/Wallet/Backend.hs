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

import Ariadne.Wallet.Face

createWalletBackend :: IO ((WalletEvent -> IO ()) -> WalletFace)
createWalletBackend = do
  return $ \sendWalletEvent ->
    WalletFace
      { walletAddAccount = addAccount
      , walletRefreshUserSecret = refreshUserSecret sendWalletEvent
      }

refreshUserSecret :: (WalletEvent -> IO ()) -> CardanoMode ()
refreshUserSecret sendWalletEvent = do
  us <- getSecretDefault
  liftIO $ sendWalletEvent (WalletUserSecretSetEvent us)

addAccount :: CardanoMode ()
addAccount =
    modifySecretDefault $ \us ->
        let (newUS, wus) = ensureWalletExists us
            addAccountPure :: [(Word32, Text)] -> [(Word32, Text)]
            addAccountPure accounts =
                accounts <> [(fromIntegral (length accounts), "account")]
            newWallet :: WalletUserSecret
            newWallet = wus & wusAccounts %~ addAccountPure
         in newUS & usWallet .~ Just newWallet

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
