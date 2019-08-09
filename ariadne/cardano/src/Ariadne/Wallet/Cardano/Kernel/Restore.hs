-- | Cardano Wallet restoration logic.

module Ariadne.Wallet.Cardano.Kernel.Restore
       ( collectUtxo
       , restoreWallets
       , RestoreFrom (..)
       , WalletToRestore (..)
       , WrongMnemonic (..)
       ) where

import Control.Exception (Exception(displayException))
import Control.Lens (at, non, (?~))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Fmt (pretty)

import Pos.Binary.Class (decodeFull')
import Pos.Chain.Txp (Utxo)
import Pos.Crypto (EncryptedSecretKey, PassPhrase)
import qualified Pos.Crypto as Crypto
  (checkPassMatches, deriveHDPassphrase, emptyPassphrase, encToPublic,
  safeDeterministicKeyGen)
import qualified Pos.Util.Mnemonic as Daedalus
import Pos.Util.UserSecret (usKeys)

import Ariadne.Cardano.Face (Address, CardanoMode)
import Ariadne.Wallet.Cardano.Kernel.AddressDiscovery
  (AddressWithPathToUtxoMap, discoverHDAddressWithUtxo)
import Ariadne.Wallet.Cardano.Kernel.Bip32 (DerivationPath(..))
import Ariadne.Wallet.Cardano.Kernel.Bip39 (mnemonicToSeedNoPassword)
import Ariadne.Wallet.Cardano.Kernel.Bip44
  (Bip44DerivationPath(..), bip44PathToAddressId, decodeBip44DerivationPath)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (AssuranceLevel(..), HdAddressId, WalletName(..), eskToHdRootId,
  hdAddressIdParent)
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx
  (PrefilteredUtxo, UtxoByAccount)
import Ariadne.Wallet.Cardano.Kernel.Wallets
  (HasNonemptyPassphrase(..), mkHasPP)
import Ariadne.Wallet.Face (Mnemonic(..))

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

data RestoreFrom
  = RestoreFromMnemonic !(Either Mnemonic (Daedalus.Mnemonic 12))
                        !PassPhrase
  | RestoreFromKeyFile  !FilePath


data WalletToRestore = WalletToRestore
    !EncryptedSecretKey
    !HasNonemptyPassphrase
    !WalletName
    !AssuranceLevel

restoreWallets
    :: RestoreFrom
    -> WalletName
    -> IO [WalletToRestore]
restoreWallets rFrom walletName = do
  case rFrom of
    RestoreFromMnemonic eMnemonicBPhrase pp ->
      pure [restoreWalletFromMnemonic pp walletName eMnemonicBPhrase]
    RestoreFromKeyFile path ->
      restoreFromKeyFile walletName path

restoreWalletFromMnemonic
  :: PassPhrase
  -> WalletName
  -> Either Mnemonic (Daedalus.Mnemonic 12)
  -> WalletToRestore
restoreWalletFromMnemonic pp walletName eMnemonicBPhrase =
    WalletToRestore esk hasPP walletName assurance
  where
    hasPP = mkHasPP pp
    esk = getKeyFromMnemonic pp eMnemonicBPhrase
    -- TODO(AD-251): allow selecting assurance.
    assurance = AssuranceLevelNormal


restoreFromKeyFile
    :: WalletName
    -> FilePath
    -> IO [WalletToRestore]
restoreFromKeyFile walletName path = do
    esks <- readNonAriadneKeys path
    let templateName i (WalletName n) = WalletName $ n <> " " <> pretty i
    let hasPP esk = HasNonemptyPassphrase $ isNothing $ Crypto.checkPassMatches Crypto.emptyPassphrase esk
    pure $
      fmap
        (\(i,esk) -> WalletToRestore esk (hasPP esk) (templateName i walletName) assurance)
        (zip [(0 :: Int)..] esks)
    where
      -- TODO(AD-251): allow selecting assurance.
      assurance = AssuranceLevelNormal

getKeyFromMnemonic
    :: PassPhrase
    -> Either Mnemonic (Daedalus.Mnemonic 12)
    -> EncryptedSecretKey
getKeyFromMnemonic pp mnemonicOrMnemonic =
    snd $ Crypto.safeDeterministicKeyGen seed pp
  where
    seed = case mnemonicOrMnemonic of
      Left (Mnemonic mnemonic) -> mnemonicToSeedNoPassword mnemonic
      Right daedalusMnemonic -> Daedalus.mnemonicToSeed daedalusMnemonic

-- | Reads daedalus wallet keys from file for wallet restoration
readNonAriadneKeys :: FilePath -> IO [EncryptedSecretKey]
readNonAriadneKeys path = do
  keyFile <- BS.readFile path
  case decodeFull' keyFile of
    Left e   -> throwM $ SecretsDecodingError path e
    Right us -> pure $ us ^. usKeys

collectUtxo :: EncryptedSecretKey -> CardanoMode UtxoByAccount
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
        -- or a comment in Ariadne.Wallet.Cardano.Kernel.AddressDiscovery.discoverHDAddressesWithUtxo
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
