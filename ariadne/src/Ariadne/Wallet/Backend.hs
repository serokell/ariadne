module Ariadne.Wallet.Backend
  ( WalletFace(..)
  , createWalletBackend
  ) where

import Universum

import Control.Lens ((<>~))
import Pos.Crypto
import Pos.Client.KeyStorage
import Pos.Util.UserSecret
import Test.QuickCheck

import Ariadne.Wallet.Face

createWalletBackend :: IO ((WalletEvent -> IO ()) -> WalletFace)
createWalletBackend = do
  return $ \sendWalletEvent ->
    WalletFace
      { walletAddRandomKey = addRandomKey
      , walletRefreshUserSecret = refreshUserSecret sendWalletEvent
      }

refreshUserSecret :: (WalletEvent -> IO ()) -> CardanoMode ()
refreshUserSecret sendWalletEvent = do
  us <- getSecretDefault
  liftIO $ sendWalletEvent (WalletUserSecretSetEvent us)

addRandomKey :: CardanoMode ()
addRandomKey = do
  key <- liftIO $ generate arbitrary
  let sk = noPassEncrypt key
  modifySecretDefault $ \us ->
    if view usKeys us `containsKey` sk
    then us
    else us & usKeys <>~ [sk]

containsKey :: [EncryptedSecretKey] -> EncryptedSecretKey -> Bool
containsKey ls k = hash k `elem` map hash ls
