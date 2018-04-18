-- | Part of backend which manages keys, wallets, accounts, addresses.

module Ariadne.Wallet.Backend.KeyStorage
       (
         -- * Commands/other functions
         resolveWalletRef
       , refreshUserSecret
       , addAddress
       , addAccount
       , addWallet
       , select

         -- * Exceptions
       , NoWalletSelection (..)
       , NoAccountSelection (..)
       ) where

import Universum

import Control.Exception (Exception(displayException))
import Control.Lens (ix, (.=), (<>=))
import Control.Monad.Catch.Pure (CatchT, runCatchT)
import Data.List (findIndex)
import qualified Data.Vector as V (findIndex)

import IiExtras
import Pos.Client.KeyStorage (getSecretDefault, modifySecretDefault)
import Pos.Core.Common (IsBootstrapEraAddr(..), deriveLvl2KeyPair)
import Pos.Crypto
import Pos.Util (eitherToThrow, maybeThrow)
import Pos.Util.UserSecret

import Ariadne.Wallet.Face

data NoWalletSelection = NoWalletSelection
  deriving (Eq, Show)

instance Exception NoWalletSelection where
  displayException NoWalletSelection =
    "Select or specify a wallet to perform this operation."

data NoAccountSelection = NoAccountSelection
  deriving (Eq, Show)

instance Exception NoAccountSelection where
  displayException NoAccountSelection =
    "Select or specify an account to perform this operation."

data WalletDoesNotExist = WalletDoesNotExist Text
  deriving (Eq, Show)

instance Exception WalletDoesNotExist where
  displayException (WalletDoesNotExist t) =
    "The wallet " ++ show t ++ " does not exist."

data AccountDoesNotExist = AccountDoesNotExist Text
  deriving (Eq, Show)

instance Exception AccountDoesNotExist where
  displayException (AccountDoesNotExist t) =
    "The account " ++ show t ++ " does not exist."

data AddressDoesNotExist = AddressDoesNotExist Text
  deriving (Eq, Show)

instance Exception AddressDoesNotExist where
  displayException (AddressDoesNotExist t) =
    "The address " ++ show t ++ " does not exist."

data SelectIsTooDeep = SelectIsTooDeep
  deriving (Eq, Show)

instance Exception SelectIsTooDeep where
  displayException SelectIsTooDeep =
    "The selection path is too deep"

data AddressGenerationFailed = AGFailedIncorrectPassPhrase
  deriving (Eq, Show)

instance Exception AddressGenerationFailed where
    displayException =
        \case
            AGFailedIncorrectPassPhrase ->
                "Address generation failed due to incorrect passphrase"

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
    case findIndex (\w -> w ^. wdName == name) (us ^. usWallets) of
      Just i -> return (fromIntegral i)
      Nothing -> throwM $ WalletDoesNotExist name
  WalletRefByIndex i -> return i

-- | Like 'resolveWalletRef', but for accounts.
resolveAccountRef
  :: IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> AccountReference
  -> IO (Word, Word32)
resolveAccountRef walletSelRef runCardanoMode accountRef = do
    walletIdx <- resolveWalletRef walletSelRef runCardanoMode walletRef
    (walletIdx, ) <$> resolveAccountIdx walletIdx
  where
    walletRef =
        case accountRef of
            AccountRefSelection -> WalletRefSelection
            AccountRefByIndex _ wr -> wr
            AccountRefByName _ wr -> wr
    resolveAccountIdx walletIdx =
        case accountRef of
            AccountRefSelection -> do
                mWalletSelection <- readIORef walletSelRef
                case mWalletSelection of
                    Nothing -> throwM NoWalletSelection
                    Just WalletSelection {..} ->
                        case headMay wsPath of
                            Nothing -> throwM NoAccountSelection
                            Just accIdx -> return (fromIntegral accIdx)
            AccountRefByIndex accIdx _ -> return accIdx
            AccountRefByName accName _ -> do
                us <- runCardanoMode getSecretDefault
                walletData <-
                    maybeThrow
                        (WalletDoesNotExist (pretty walletIdx))
                        (us ^? usWallets . ix (fromIntegral walletIdx))
                case V.findIndex
                         (\ad -> ad ^. adName == accName)
                         (walletData ^. wdAccounts) of
                    Just i -> return (fromIntegral i)
                    Nothing -> throwM $ AccountDoesNotExist accName

refreshUserSecret
  :: IORef (Maybe WalletSelection)
  -> (CardanoMode ~> IO)
  -> (WalletEvent -> IO ())
  -> IO ()
refreshUserSecret walletSelRef runCardanoMode sendWalletEvent = do
  walletSel <- readIORef walletSelRef
  us <- runCardanoMode getSecretDefault
  sendWalletEvent (WalletUserSecretSetEvent us walletSel)

addAddress ::
       WalletFace
    -> IORef (Maybe WalletSelection)
    -> (CardanoMode ~> IO)
    -> AccountReference
    -> PassPhrase
    -> IO ()
addAddress WalletFace {..} walletSelRef runCardanoMode accRef pp = do
    (walletIdx, accountIdx) <-
        resolveAccountRef walletSelRef runCardanoMode accRef
    let wIdx, accIdx :: Int
        wIdx = fromIntegral walletIdx
        accIdx = fromIntegral accountIdx
    let addAddressPure :: CatchT (State UserSecret) ()
        addAddressPure = do
            walletData <-
                maybeThrow (WalletDoesNotExist (pretty walletIdx)) =<<
                preuse (usWallets . ix wIdx)
            accountData <-
                maybeThrow (AccountDoesNotExist $ show (walletIdx, accountIdx)) $
                walletData ^?
                wdAccounts .
                ix accIdx
            let addrIdx = fromIntegral (length (accountData ^. adAddresses))
            -- FIXME: support not only bootstrap era
            addr <-
                case deriveLvl2KeyPair
                         (IsBootstrapEraAddr True)
                         (ShouldCheckPassphrase True)
                         pp
                         (walletData ^. wdRootKey)
                         accountIdx
                         addrIdx of
                    Nothing -> throwM AGFailedIncorrectPassPhrase
                    Just (a, _) -> pure a
            usWallets . ix wIdx . wdAccounts . ix accIdx . adAddresses <>=
                one (addrIdx, addr)
    runCardanoMode (modifySecretDefault (runCatchT addAddressPure)) >>=
        eitherToThrow
    walletRefreshUserSecret

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
    case us ^. usWallets ^? ix (fromIntegral wsWalletIndex) of
      Nothing -> throwM $ WalletDoesNotExist (show wsWalletIndex)
      Just wd -> do
        let addAccountPure :: Vector AccountData -> Vector AccountData
            addAccountPure accounts =
                accounts <>
                one
                    (AccountData
                            { _adName = accountName
                            , _adPath = fromIntegral (length accounts)
                            , _adAddresses = mempty
                            })
            wd' = wd & wdAccounts %~ addAccountPure
        modifySecretDefault $
            usWallets . ix (fromIntegral wsWalletIndex) .= wd'
  walletRefreshUserSecret

addWallet :: WalletFace -> (CardanoMode ~> IO) -> PassPhrase -> Text -> IO [Text]
addWallet WalletFace{..} runCardanoMode pp walletName = do
    let mnemonic = ["patak"]
    seed <- secureRandomBS 32  -- TODO: use mnemonic!
    let (_, esk) = safeDeterministicKeyGen seed pp
    let emptyWallet :: WalletData
        emptyWallet =
            WalletData
                { _wdRootKey = esk
                , _wdName = walletName
                , _wdAccounts = mempty
                }
    runCardanoMode $ modifySecretDefault (usWallets <>= one emptyWallet)
    mnemonic <$ walletRefreshUserSecret

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
      us <- runCardanoMode getSecretDefault
      -- validate wallet
      wallet <- maybeThrow
        (WalletDoesNotExist $ pretty wsWalletIndex)
        (us ^? usWallets . ix (fromIntegral wsWalletIndex))
      case nonEmpty wsPath of
        Nothing -> return ()
        Just (accIdx :| acPath) -> do

          -- validate account
          account <- maybeThrow
            (AccountDoesNotExist $ pretty accIdx)
            (wallet ^? wdAccounts . ix (fromIntegral accIdx))

          case nonEmpty acPath of
            Nothing -> return ()
            Just (addrIdx :| []) -> do
              -- validate address
              void $ maybeThrow
                (AddressDoesNotExist $ pretty addrIdx)
                (account ^? adAddresses . ix (fromIntegral addrIdx))
            Just (_ :| _) -> throwM SelectIsTooDeep

      atomicWriteIORef walletSelRef $ Just $
        WalletSelection { wsPath, wsWalletIndex }
  walletRefreshUserSecret
