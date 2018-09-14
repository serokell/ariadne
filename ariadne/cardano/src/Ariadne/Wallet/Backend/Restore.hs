-- | Wallet restoration logic.

module Ariadne.Wallet.Backend.Restore
       ( restoreWallet
       , restoreFromKeyFile
       ) where

import qualified Universum.Unsafe as Unsafe (init)

import Control.Exception (Exception(displayException))
import Control.Lens (at, non, (?~))
import Control.Natural (type (~>))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import Pos.Binary.Class (decodeFull')
import Pos.Core.Configuration (HasConfiguration)
import Pos.Crypto (EncryptedSecretKey)
import qualified Pos.Crypto as Crypto
import Pos.Txp.Toil.Types (Utxo)
import Pos.Util.BackupPhrase (BackupPhrase(..), safeKeysFromPhrase)
import Pos.Util.UserSecret (usKeys0)

import Ariadne.Cardano.Face
import Ariadne.Wallet.Backend.AddressDiscovery
  (AddressWithPathToUtxoMap, discoverHDAddressWithUtxo)
import Ariadne.Wallet.Backend.KeyStorage (addWallet)
import Ariadne.Wallet.Cardano.Kernel.Bip39 (mnemonicToSeedNoPassword)
import Ariadne.Wallet.Cardano.Kernel.Bip44
  (Bip44DerivationPath(..), bip44PathToAddressId, decodeBip44DerivationPath)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
import Ariadne.Wallet.Cardano.Kernel.PrefilterTx (PrefilteredUtxo)
import Ariadne.Wallet.Cardano.Kernel.Wallets
  (HasNonemptyPassphrase(..), mkHasPP)
import Ariadne.Wallet.Cardano.WalletLayer.Types (PassiveWalletLayer(..))
import Ariadne.Wallet.Face

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
    => PassiveWalletLayer IO
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> IO PassPhrase
    -> Maybe WalletName
    -> Mnemonic
    -> WalletRestoreType
    -> IO ()
restoreWallet pwl face runCardanoMode getPassTemp mbWalletName (Mnemonic mnemonic) rType = do
    pp <- getPassTemp
    let mnemonicWords = words mnemonic
        isAriadneMnemonic = fromMaybe False $ do
          lastWord <- last <$> nonEmpty mnemonicWords
          pure (lastWord == "ariadne-v0") -- TODO AD-124: version parsing?
    esk <- if
      | isAriadneMnemonic ->
          let seed = mnemonicToSeedNoPassword (unwords $ Unsafe.init mnemonicWords)
          in pure . snd $ Crypto.safeDeterministicKeyGen seed pp
      | length mnemonicWords == 12 ->
          case safeKeysFromPhrase pp (BackupPhrase mnemonicWords) of
              Left e        -> throwM $ WrongMnemonic e
              Right (sk, _) -> pure sk
      | otherwise -> throwM $ WrongMnemonic "Unknown mnemonic type"
    let hasPP = mkHasPP pp
    restoreFromSecretKey pwl face runCardanoMode mbWalletName esk rType hasPP assurance
  where
    -- TODO(AD-251): allow selecting assurance.
    assurance = AssuranceLevelNormal

restoreFromKeyFile ::
       HasConfiguration
    => PassiveWalletLayer IO
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> Maybe WalletName
    -> FilePath
    -> WalletRestoreType
    -> IO ()
restoreFromKeyFile pwl face runCardanoMode mbWalletName path rType = do
    keyFile <- BS.readFile path
    us <- case decodeFull' keyFile of
      Left e   -> throwM $ SecretsDecodingError path e
      Right us -> pure us
    let templateName i (WalletName n) = WalletName $ n <> " " <> pretty i
    traverse_
        (\(i,esk) -> do
            let hasPP = HasNonemptyPassphrase $
                    isNothing $ Crypto.checkPassMatches Crypto.emptyPassphrase esk
            restoreFromSecretKey
                pwl
                face
                runCardanoMode
                (templateName i <$> mbWalletName)
                esk
                rType
                hasPP
                assurance)
        (zip [(0 :: Int)..] $ us ^. usKeys0)
  where
    -- TODO(AD-251): allow selecting assurance.
    assurance = AssuranceLevelNormal

restoreFromSecretKey ::
       HasConfiguration
    => PassiveWalletLayer IO
    -> WalletFace
    -> (CardanoMode ~> IO)
    -> Maybe WalletName
    -> EncryptedSecretKey
    -> WalletRestoreType
    -> HasNonemptyPassphrase
    -> AssuranceLevel
    -> IO ()
restoreFromSecretKey pwl face runCardanoMode mbWalletName esk rType hasPP assurance = do
    utxoByAccount <- case rType of
        WalletRestoreQuick -> pure mempty
        WalletRestoreFull  -> runCardanoMode $ collectUtxo esk
    addWallet pwl face esk mbWalletName utxoByAccount hasPP assurance

collectUtxo ::
       HasConfiguration
    => EncryptedSecretKey
    -> CardanoMode (Map HdAccountId PrefilteredUtxo)
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
            step utxoByAccount addrWithId@(addressId, _) utxo =
                utxoByAccount &
                    at (addressId ^. hdAddressIdParent) .
                    non mempty .
                    at addrWithId ?~ utxo
        in Map.foldlWithKey' step mempty
