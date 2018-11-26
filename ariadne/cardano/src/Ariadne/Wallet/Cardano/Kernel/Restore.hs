-- | Cardano Wallet restoration logic.

module Ariadne.Wallet.Cardano.Kernel.Restore
       ( getKeyFromMnemonic
       , getUtxoByAccount
       , readNonAriadneKeys
       ) where

import qualified Universum.Unsafe as Unsafe (init)

import Control.Exception (Exception(displayException))
import Control.Lens (at, non, (?~))
import Control.Natural (type (~>))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import Pos.Binary.Class (decodeFull')
import Pos.Core.Configuration (HasConfiguration)
import Pos.Crypto (EncryptedSecretKey, PassPhrase)
import qualified Pos.Crypto as Crypto
  (deriveHDPassphrase, encToPublic, safeDeterministicKeyGen)
import Pos.Txp.Toil.Types (Utxo)
import Pos.Util.BackupPhrase (BackupPhrase(..), safeKeysFromPhrase)
import Pos.Util.UserSecret (usKeys0)

import Ariadne.Cardano.Face (Address, CardanoMode)
import Ariadne.Wallet.Backend.AddressDiscovery
  (AddressWithPathToUtxoMap, discoverHDAddressWithUtxo)
import Ariadne.Wallet.Cardano.Kernel.Bip32 (DerivationPath(..))
import Ariadne.Wallet.Cardano.Kernel.Bip39 (mnemonicToSeedNoPassword)
import Ariadne.Wallet.Cardano.Kernel.Bip44
  (Bip44DerivationPath(..), bip44PathToAddressId, decodeBip44DerivationPath)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (HdAddressId, eskToHdRootId, hdAddressIdParent)
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx
  (PrefilteredUtxo, UtxoByAccount)
import Ariadne.Wallet.Face (WalletRestoreType(..))

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

getKeyFromMnemonic ::
       MonadThrow m
    => Bool
    ->[Text]
    -> PassPhrase
    -> m EncryptedSecretKey
getKeyFromMnemonic isAriadneMnemonic mnemonicWords pp =
    if
    | isAriadneMnemonic ->
        let seed = mnemonicToSeedNoPassword (unwords $ Unsafe.init mnemonicWords)
        in pure . snd $ Crypto.safeDeterministicKeyGen seed pp
    | length mnemonicWords == 12 ->
        case safeKeysFromPhrase pp (BackupPhrase mnemonicWords) of
            Left e        -> throwM $ WrongMnemonic e
            Right (sk, _) -> pure sk
    | otherwise -> throwM $ WrongMnemonic "Unknown mnemonic type"


-- | reads daedalus wallet keys from file for wallet restoration
readNonAriadneKeys :: FilePath -> IO [EncryptedSecretKey]
readNonAriadneKeys path = do
  keyFile <- BS.readFile path
  case decodeFull' keyFile of
    Left e   -> throwM $ SecretsDecodingError path e
    Right us -> pure $ us ^. usKeys0

getUtxoByAccount ::
       HasConfiguration
    => (CardanoMode ~> IO)
    -> EncryptedSecretKey
    -> WalletRestoreType
    -> IO UtxoByAccount
getUtxoByAccount runCardanoMode esk  = \case
    WalletRestoreQuick -> pure mempty
    WalletRestoreFull  -> runCardanoMode $ collectUtxo esk

collectUtxo ::
       HasConfiguration
    => EncryptedSecretKey
    -> CardanoMode UtxoByAccount
collectUtxo esk = do
    m <- discoverHDAddressWithUtxo $ Crypto.deriveHDPassphrase $ Crypto.encToPublic esk
    pure $ groupAddresses $ filterAddresses m
  where
    toHdAddressId :: Bip44DerivationPath -> HdAddressId
    toHdAddressId = bip44PathToAddressId (eskToHdRootId esk)

    -- TODO: simply ignoring addresses which are not BIP-44 compliant
    -- is not perfect (though normal users shouldn't have addresses at
    -- different levels). We should probably at least show some
    -- message if we encounter such addresses. Let's do it after
    -- switching to modern wallet data layer.

    filterAddresses :: AddressWithPathToUtxoMap -> PrefilteredUtxo
    filterAddresses = Map.fromList . mapMaybe f . toPairs
      where
        f ((DerivationPath derPath, addr), utxo) =
            case decodeBip44DerivationPath derPath of
                Nothing           -> Nothing
                Just bip44DerPath -> Just ((toHdAddressId bip44DerPath, addr), utxo)

    groupAddresses :: PrefilteredUtxo -> UtxoByAccount
    groupAddresses =
        -- See https://hackage.haskell.org/package/lens-3.10.1/docs/Control-Lens-Iso.html#v:non
        -- or a comment in Ariadne.Wallet.Backend.AddressDiscovery.discoverHDAddressesWithUtxo
        -- for an explanation of how this works.
        let step :: UtxoByAccount ->
                    (HdAddressId, Address) ->
                    Utxo ->
                    UtxoByAccount
            step utxoByAccount addrWithId@(addressId, _) utxo =
                utxoByAccount &
                    at (addressId ^. hdAddressIdParent) .
                    non mempty .
                    at addrWithId ?~ utxo
        in Map.foldlWithKey' step mempty
