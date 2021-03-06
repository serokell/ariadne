-- | Helpers to work with BIP-32 implementation in Cardano.

module Ariadne.Wallet.Cardano.Kernel.Bip32
       ( DerivationPath (..)
       , deriveHDSecretKeyByPath
       , makePubKeyHdwAddressUsingPath
       ) where

import Named ((:!), pattern Arg)

import Cardano.Crypto.Wallet.Types (DerivationIndex)
import Pos.Core.Common (Address, IsBootstrapEraAddr(..), makePubKeyHdwAddress)
import Pos.Core.NetworkMagic (NetworkMagic)
import Pos.Crypto.HD
  (ShouldCheckPassphrase(..), deriveHDPassphrase, deriveHDSecretKey,
  packHDAddressAttr)
import Pos.Crypto.Signing (EncryptedSecretKey, PassPhrase, PublicKey)

newtype DerivationPath = DerivationPath [DerivationIndex]
    deriving (Ord, Eq)

-- | Like 'deriveHDSecretKey' from Cardano, but can derive not only
-- direct descendant.
deriveHDSecretKeyByPath
    :: ShouldCheckPassphrase
    -> PassPhrase
    -> EncryptedSecretKey
    -> DerivationPath
    -> Maybe EncryptedSecretKey
deriveHDSecretKeyByPath shouldCheck pp sk (DerivationPath derPath) =
    foldM
        (\sk' (check, idx) -> deriveHDSecretKey check pp sk' idx)
        sk
        -- check passphrase at most once
        (zip (shouldCheck : repeat (ShouldCheckPassphrase False)) derPath)

-- | Like 'makePubKeyHdwAddress', but also creates HDPassphrase
-- internally using derivation path and root public key.
makePubKeyHdwAddressUsingPath ::
       NetworkMagic
    -> IsBootstrapEraAddr
    -> DerivationPath
    -> "root" :! PublicKey -> "address" :! PublicKey -> Address
makePubKeyHdwAddressUsingPath nm era (DerivationPath derPath)
  (Arg rootPK) (Arg addrPK) =
    makePubKeyHdwAddress nm era hdPayload addrPK
  where
    hdPP = deriveHDPassphrase rootPK
    hdPayload = packHDAddressAttr hdPP derPath
