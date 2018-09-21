-- | Helpers to work with BIP-32 implementation in Cardano.

module Ariadne.Wallet.Cardano.Kernel.Bip32
       ( DerivationPath
       , deriveHDSecretKeyByPath
       , makePubKeyHdwAddressUsingPath
       ) where

import Named (Named(..))

import Cardano.Crypto.Wallet.Types (DerivationIndex)
import Pos.Core.Common (Address, IsBootstrapEraAddr(..), makePubKeyHdwAddress)
import Pos.Crypto.HD
  (ShouldCheckPassphrase(..), deriveHDPassphrase, deriveHDSecretKey,
  packHDAddressAttr)
import Pos.Crypto.Signing (EncryptedSecretKey, PassPhrase, PublicKey)

-- TODO(AD-248): make it a newtype
type DerivationPath = [DerivationIndex]

-- | Like 'deriveHDSecretKey' from Cardano, but can derive not only
-- direct descendant.
deriveHDSecretKeyByPath
    :: ShouldCheckPassphrase
    -> PassPhrase
    -> EncryptedSecretKey
    -> DerivationPath
    -> Maybe EncryptedSecretKey
deriveHDSecretKeyByPath shouldCheck pp sk derPath =
    foldM
        (\sk' (check, idx) -> deriveHDSecretKey check pp sk' idx)
        sk
        -- check passphrase at most once
        (zip (shouldCheck : repeat (ShouldCheckPassphrase False)) derPath)

-- | Like 'makePubKeyHdwAddress', but also creates HDPassphrase
-- internally using derivation path and root public key.
makePubKeyHdwAddressUsingPath ::
       IsBootstrapEraAddr
    -> DerivationPath
    -> PublicKey `Named` "root" -> PublicKey `Named` "address" -> Address
makePubKeyHdwAddressUsingPath era derPath (Named rootPK) (Named addrPK) =
    makePubKeyHdwAddress era hdPayload addrPK
  where
    hdPP = deriveHDPassphrase rootPK
    hdPayload = packHDAddressAttr hdPP derPath
