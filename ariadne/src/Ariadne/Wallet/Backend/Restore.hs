-- | Wallet restoration logic.

module Ariadne.Wallet.Backend.Restore
       ( restoreWallet
       , restoreFromKeyFile
       ) where

import Universum hiding (init)

import Control.Exception (Exception(displayException))
import Control.Lens (at, non)
import Data.Acid (AcidState)
import qualified Data.ByteString as BS
import Data.List (init)
import qualified Data.List.NonEmpty as NE
-- import qualified Data.Map.Strict as Map

import Ariadne.Cardano.Face
import Ariadne.Wallet.Cardano.Kernel.Bip44
  (Bip44DerivationPath(..), decodeBip44DerivationPath)
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import IiExtras
import Loot.Crypto.Bip39 (mnemonicToSeed)
import Pos.Binary.Class (decodeFull')
import Pos.Core.Configuration (HasConfiguration)
import Pos.Crypto.HD (deriveHDPassphrase)
import Pos.Crypto.HDDiscovery (discoverHDAddress)
import Pos.Crypto.Signing
  (EncryptedSecretKey, encToPublic, safeDeterministicKeyGen)
import Pos.Util.BackupPhrase (BackupPhrase(..), safeKeysFromPhrase)
import Pos.Util.UserSecret (usKeys0)

import Ariadne.Wallet.Backend.KeyStorage (addWallet)
import Ariadne.Wallet.Face


data WrongMnemonic = WrongMnemonic Text
 deriving (Eq, Show)

instance Exception WrongMnemonic where
  displayException (WrongMnemonic txt) =
    "Wrong mnemonic: " <> show txt

data SecretsDecodingError = SecretsDecodingError FilePath Text
  deriving (Eq, Show)

instance Exception SecretsDecodingError where
  displayException (SecretsDecodingError path txt) =
    "Failed to decode " <> path <> ": " <> show txt

restoreWallet ::
       HasConfiguration
    => AcidState DB
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> PassPhrase
    -> Maybe WalletName
    -> Mnemonic
    -> WalletRestoreType
    -> IO ()
restoreWallet acidDb face runCardanoMode pp mbWalletName (Mnemonic mnemonic) rType = do
    let mnemonicWords = words mnemonic
        isAriadneMnemonic = fromMaybe False $ do
          lastWord <- NE.last <$> nonEmpty mnemonicWords
          pure (lastWord == "ariadne-v0") -- TODO AD-124: version parsing?
    esk <- if
      | isAriadneMnemonic ->
          let seed = mnemonicToSeed (unwords $ init mnemonicWords) ""
          in pure . snd $ safeDeterministicKeyGen seed pp
      | length mnemonicWords == 12 ->
          case safeKeysFromPhrase pp (BackupPhrase mnemonicWords) of
              Left e -> throwM $ WrongMnemonic e
              Right (sk, _) -> pure sk
      | otherwise -> throwM $ WrongMnemonic "Unknown mnemonic type"
    restoreFromSecretKey acidDb face runCardanoMode mbWalletName esk rType

restoreFromKeyFile ::
       HasConfiguration
    => AcidState DB
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> Maybe WalletName
    -> FilePath
    -> WalletRestoreType
    -> IO ()
restoreFromKeyFile acidDb face runCardanoMode mbWalletName path rType = do
    keyFile <- BS.readFile path
    us <- case decodeFull' keyFile of
      Left e -> throwM $ SecretsDecodingError path e
      Right us -> pure us
    let templateName i (WalletName n) = WalletName $ n <> " " <> pretty i
    traverse_
        (\(i,esk) -> restoreFromSecretKey acidDb face runCardanoMode (templateName i <$> mbWalletName) esk rType)
        (zip [(0 :: Int)..] $ us ^. usKeys0)

restoreFromSecretKey ::
       HasConfiguration
    => AcidState DB
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> Maybe WalletName
    -> EncryptedSecretKey
    -> WalletRestoreType
    -> IO ()
restoreFromSecretKey acidDb face runCardanoMode mbWalletName esk rType = do
    accounts <- case rType of
        WalletRestoreQuick -> pure mempty
        WalletRestoreFull -> runCardanoMode $ findAccounts esk
    addWallet acidDb face runCardanoMode esk mbWalletName accounts

findAccounts ::
       HasConfiguration
    => EncryptedSecretKey
    -> CardanoMode (Vector HdAccount)
findAccounts esk =
    convertRes <$> discoverHDAddress (deriveHDPassphrase (encToPublic esk))
  where
    convertRes :: [(Address, [Word32])] -> Vector HdAccount
    convertRes = convertGroups . groupAddresses . filterAddresses

    -- TODO: simply ignoring addresses which are not at the 2nd level
    -- is not perfect (though normal users shouldn't have addresses at
    -- different levels). We should probably at least show some
    -- message if we encounter such addresses. Let's do it after
    -- switching to modern wallet data layer.

    filterAddresses :: [(Address, [Word32])] -> [(Address, Bip44DerivationPath)]
    filterAddresses =
        mapMaybe (\(addr, indices) -> (addr,) <$> decodeBip44DerivationPath indices)

    groupAddresses ::
        [(Address, Bip44DerivationPath)] -> Map HdAccountIx [(Bip44DerivationPath, Address)]
    groupAddresses =
        let step ::
                Map HdAccountIx [(Bip44DerivationPath, Address)] ->
                (Address, Bip44DerivationPath) ->
                Map HdAccountIx [(Bip44DerivationPath, Address)]
            step res (addr, derPath) =
                res & at (bip44AccountIndex derPath) . non mempty %~ ((derPath, addr):)
        in foldl' step mempty

    convertGroups :: Map HdAccountIx [(Bip44DerivationPath, Address)] -> Vector HdAccount
    convertGroups = undefined
    -- TODO:
    -- * Construct AddrCheckpoint
    -- * Add addresses to accounts
    -- * Construct AccCheckpoint
        -- let toAccountData (accIdx, addrs) = AccountData
        --       { _adName = "Restored account " <> pretty accIdx
        --       , _adLastIndex = 0
        --       , _adPath = accIdx
        --       , _adAddresses = V.fromList addrs
        --       }
        -- in V.fromList . map toAccountData . Map.toList
