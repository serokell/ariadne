-- | Wallet restoration logic.

module Ariadne.Wallet.Backend.Restore
       ( restoreWallet
       , restoreFromKeyFile
       ) where

import Universum hiding (init)

import Control.Exception (Exception(displayException))
import Control.Lens (at, non, (?~))
import Data.Acid (AcidState)
import qualified Data.ByteString as BS
import Data.List (init)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

import Loot.Crypto.Bip39 (mnemonicToSeed)
import Pos.Binary.Class (decodeFull')
import Pos.Core.Configuration (HasConfiguration)
import Pos.Crypto.HD (deriveHDPassphrase)
import Pos.Crypto.HDDiscovery (discoverHDAddress)
import Pos.Crypto.Signing
  (EncryptedSecretKey, encToPublic, safeDeterministicKeyGen)
import Pos.Txp.Toil.Types (Utxo)
import Pos.Util.BackupPhrase (BackupPhrase(..), safeKeysFromPhrase)
import Pos.Util.UserSecret (usKeys0)

import Ariadne.Cardano.Face
import Ariadne.Wallet.Backend.AddressDiscovery
  (AddressWithPathToUtxoMap, discoverHDAddressWithUtxo)
import Ariadne.Wallet.Backend.KeyStorage (addWallet)
import Ariadne.Wallet.Cardano.Kernel.Bip32 (DerivationPath)
import Ariadne.Wallet.Cardano.Kernel.Bip44
  (Bip44DerivationPath(..), bip44PathToAddressId, decodeBip44DerivationPath)
import Ariadne.Wallet.Cardano.Kernel.DB.AcidState
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.DB.InDb (InDb(..))
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx (PrefilteredUtxo)
import Ariadne.Wallet.Face
import IiExtras

newtype WrongMnemonic = WrongMnemonic Text
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
              Left e        -> throwM $ WrongMnemonic e
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
      Left e   -> throwM $ SecretsDecodingError path e
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
    utxoByAccount <- case rType of
        WalletRestoreQuick -> pure mempty
        WalletRestoreFull  -> runCardanoMode $ collectUtxo esk
    addWallet acidDb face runCardanoMode esk mbWalletName utxoByAccount

collectUtxo ::
       HasConfiguration
    => EncryptedSecretKey
    -> CardanoMode (Map HdAccountId PrefilteredUtxo)
collectUtxo esk = do
    m <- discoverHDAddressWithUtxo $ deriveHDPassphrase $ encToPublic esk
    pure $ groupAddresses $ filterAddresses m
  where
    toHdAddressId :: Bip44DerivationPath -> HdAddressId
    toHdAddressId = bip44PathToAddressId hdRootId
      where
        hdRootId :: HdRootId
        hdRootId = mkHdRootId esk

    -- TODO: simply ignoring addresses which are not BIP-44 compliant
    -- is not perfect (though normal users shouldn't have addresses at
    -- different levels). We should probably at least show some
    -- message if we encounter such addresses. Let's do it after
    -- switching to modern wallet data layer.

    filterAddresses :: AddressWithPathToUtxoMap -> PrefilteredUtxo
    filterAddresses = Map.fromList . mapMaybe f . Map.toList
      where
        f ((derPath, addr), utxo) =
            case decodeBip44DerivationPath derPath of
                Nothing           -> Nothing
                Just bip44DerPath -> Just ((toHdAddressId bip44DerPath, addr), utxo)

    groupAddresses :: PrefilteredUtxo -> Map HdAccountId PrefilteredUtxo
    groupAddresses =
        -- See https://hackage.haskell.org/package/lens-3.10.1/docs/Control-Lens-Iso.html#v:non
        -- or a comment in Ariadne.Wallet.Backend.AddressDiscovery.discoverHDAddressesWithUtxo
        -- for an explanation of how this works.
        let step :: Map HdAccountId PrefilteredUtxo ->
                    (HdAddressId, Address) ->
                    Utxo ->
                    Map HdAccountId PrefilteredUtxo
            step utxoByAccount addrWithId@(hdAddrId, addr) utxo =
                utxoByAccount &
                    at (hdAddrId ^. hdAddressIdParent) .
                    non mempty .
                    at addrWithId ?~ utxo
        in Map.foldlWithKey' step mempty
