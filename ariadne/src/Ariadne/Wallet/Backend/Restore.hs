-- | Wallet restoration logic.

module Ariadne.Wallet.Backend.Restore
       ( restoreWallet
       ) where

import Universum

import Control.Exception (Exception(displayException))
import Control.Lens (at, non)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as NE

import IiExtras
import Loot.Crypto.Bip39 (mnemonicToSeed)
import Pos.Core.Configuration (HasConfiguration)
import Pos.Crypto.HD (deriveHDPassphrase)
import Pos.Crypto.HDDiscovery (discoverHDAddress)
import Pos.Crypto.Signing
  (EncryptedSecretKey, encToPublic, safeDeterministicKeyGen)
import Pos.Util.BackupPhrase (BackupPhrase(..), safeKeysFromPhrase)

import Ariadne.Wallet.Backend.KeyStorage (addWallet)
import Ariadne.Wallet.Face


data WrongMnemonic = WrongMnemonic Text
 deriving (Eq, Show)

instance Exception WrongMnemonic where
  displayException (WrongMnemonic txt) =
    "Wrong mnemonic: " <> show txt

restoreWallet ::
       HasConfiguration
    => WalletFace
    -> (CardanoMode ~> IO)
    -> PassPhrase
    -> Maybe WalletName
    -> Mnemonic
    -> WalletRestoreType
    -> IO ()
restoreWallet face runCardanoMode pp mbWalletName (Mnemonic mnemonic) rType = do
    let mnemonicWords = words mnemonic
        isAriadneMnemonic = fromMaybe False $ do
          lastWord <- NE.last <$> nonEmpty mnemonicWords
          pure (lastWord == "ariadne-v0") -- TODO AD-124: version parsing?
    esk <- if
      | isAriadneMnemonic ->
          let seed = mnemonicToSeed mnemonic ""
          in pure . snd $ safeDeterministicKeyGen seed pp
      | length mnemonicWords == 12 ->
          case safeKeysFromPhrase pp (BackupPhrase mnemonicWords) of
              Left e -> throwM $ WrongMnemonic e
              Right (sk, _) -> pure sk
      | otherwise -> throwM $ WrongMnemonic "Unknown mnemonic type"
    accounts <- case rType of
        WalletRestoreQuick -> pure mempty
        WalletRestoreFull -> runCardanoMode $ findAccounts esk
    addWallet face runCardanoMode esk mbWalletName accounts

findAccounts ::
       HasConfiguration
    => EncryptedSecretKey
    -> CardanoMode (Vector AccountData)
findAccounts esk = convertRes <$> discoverHDAddress (deriveHDPassphrase (encToPublic esk))
  where
    convertRes :: [(Address, [Word32])] -> Vector AccountData
    convertRes = convertGroups . groupAddresses . filterAddresses

    -- TODO: simply ignoring addresses which are not at the 2nd level
    -- is not perfect (though normal users shouldn't have addresses at
    -- different levels). We should probably at least show some
    -- message if we encounter such addresses. Let's do it after
    -- switching to modern wallet data layer.
    filterAddresses :: [(Address, [Word32])] -> [(Address, (Word32, Word32))]
    filterAddresses =
        let twoIndices [α, β] = Just (α, β)
            twoIndices _ = Nothing
        in mapMaybe (\(addr, indices) -> (addr,) <$> twoIndices indices)

    groupAddresses ::
        [(Address, (Word32, Word32))] -> Map Word32 [(Word32, Address)]
    groupAddresses =
        let step :: Map Word32 [(Word32, Address)] ->
                    (Address, (Word32, Word32)) ->
                    Map Word32 [(Word32, Address)]
            step res (addr, (accIdx, addrIdx)) =
                res & at accIdx . non mempty %~ ((addrIdx, addr):)
        in foldl' step mempty

    convertGroups :: Map Word32 [(Word32, Address)] -> Vector AccountData
    convertGroups =
        let toAccountData (accIdx, addrs) = AccountData
              { _adName = "Restored account " <> pretty accIdx
              , _adPath = accIdx
              , _adAddresses = V.fromList addrs
              }
        in V.fromList . map toAccountData . Map.toList
