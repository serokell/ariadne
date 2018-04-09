module Ariadne.Wallet.Backend
  ( WalletFace(..)
  , createWalletBackend
  ) where

import Control.Exception (Exception(displayException))
import Control.Lens (ix)
import Data.List (findIndex)
import Universum

import IiExtras
import Pos.Client.KeyStorage (getSecretDefault, modifySecretDefault)
import Pos.Crypto
import Pos.Util (runGen)
import Pos.Util.UserSecret
import Test.QuickCheck (arbitrary)

import Ariadne.Wallet.Face

createWalletBackend :: IO
  (
    (CardanoMode :~> IO) ->
    (WalletEvent -> IO ()) ->
    (WalletFace, IO ())
  )
createWalletBackend = do
  walletSelRef <- newIORef Nothing
  return $ \(Nat runCardanoMode) sendWalletEvent ->
    let
      walletFace =
        fix $ \this -> WalletFace
          { walletAddAccount = addAccount this walletSelRef runCardanoMode
          , walletAddWallet = addWallet this runCardanoMode
          , walletRefreshUserSecret =
              refreshUserSecret walletSelRef runCardanoMode sendWalletEvent
          , walletSelect = select this walletSelRef runCardanoMode
          }
      initWalletAction =
        walletRefreshUserSecret walletFace
    in
      (walletFace, initWalletAction)

data NoWalletSelection = NoWalletSelection
  deriving (Eq, Show)

instance Exception NoWalletSelection where
  displayException NoWalletSelection =
    "Select or specify a wallet to perform this operation."

data AtMost1WalletSupported = AtMost1WalletSupported
  deriving (Eq, Show)

instance Exception AtMost1WalletSupported

data WalletDoesNotExist = WalletDoesNotExist Text
  deriving (Eq, Show)

instance Exception WalletDoesNotExist where
  displayException (WalletDoesNotExist t) =
    "The wallet " ++ show t ++ " does not exist."

-- | Get the wallet index by name or using current selection.
resolveWalletRef
  :: IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> WalletReference
  -> IO Word
resolveWalletRef walletSelRef runCardanoMode = \case
  WalletRefSelection -> do
    mWalletSelection <- readIORef walletSelRef
    case mWalletSelection of
      Nothing -> throwM NoWalletSelection
      Just WalletSelection{..} -> return wsWalletIndex
  WalletRefByName name -> do
    us <- runCardanoMode getSecretDefault
    case findIndex (\w -> w ^. wusWalletName == name) (toList (us ^. usWallet)) of
      Just i -> return (fromIntegral i)
      Nothing -> throwM $ WalletDoesNotExist name
  WalletRefByIndex i -> return i

refreshUserSecret
  :: IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> (WalletEvent -> IO ())
  -> IO ()
refreshUserSecret walletSelRef runCardanoMode sendWalletEvent = do
  walletSel <- readIORef walletSelRef
  us <- runCardanoMode getSecretDefault
  sendWalletEvent (WalletUserSecretSetEvent us walletSel)

addAccount
  :: WalletFace
  -> IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> WalletReference
  -> Text
  -> IO ()
addAccount WalletFace{..} walletSelRef runCardanoMode walletRef accountName = do
  wsWalletIndex <- resolveWalletRef walletSelRef runCardanoMode walletRef
  runCardanoMode $ do
    us <- getSecretDefault
    case toList (us ^. usWallet) ^? ix (fromIntegral wsWalletIndex) of
      Nothing -> throwM $ WalletDoesNotExist (show wsWalletIndex)
      Just wus -> do
        let
          addAccountPure :: [(Word32, Text)] -> [(Word32, Text)]
          addAccountPure accounts =
            accounts <> [(fromIntegral (length accounts), accountName)]
          wus' = wus & wusAccounts %~ addAccountPure
        modifySecretDefault $ usWallet .~ Just wus'
  walletRefreshUserSecret

addWallet :: WalletFace -> (CardanoMode ~> IO) -> Text -> IO ()
addWallet WalletFace{..} runCardanoMode walletName = do
  runCardanoMode $ do
    us <- getSecretDefault
    let (newUS, added) = ensureWalletExists walletName us
    if added
      then modifySecretDefault (const newUS)
      else throwM AtMost1WalletSupported
  walletRefreshUserSecret

select
  :: WalletFace
  -> IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> Maybe WalletReference
  -> [Word]
  -> IO ()
select WalletFace{..} walletSelRef runCardanoMode mWalletRef wsPath = do
  case mWalletRef of
    Nothing -> atomicWriteIORef walletSelRef Nothing
    Just walletRef -> do
      wsWalletIndex <- resolveWalletRef walletSelRef runCardanoMode walletRef
      atomicWriteIORef walletSelRef $ Just $
        WalletSelection { wsPath, wsWalletIndex }
  walletRefreshUserSecret

ensureWalletExists :: Text -> UserSecret -> (UserSecret, Bool)
ensureWalletExists walletName us =
    case us ^. usWallet of
        Just _ -> (us, False)
        Nothing -> (us & usWallet .~ Just emptyWallet, True)
  where
    emptyWallet :: WalletUserSecret
    emptyWallet =
        WalletUserSecret
            { _wusRootKey = noPassEncrypt $ runGen arbitrary
            , _wusWalletName = walletName
            , _wusAccounts = []
            , _wusAddrs = []
            }
