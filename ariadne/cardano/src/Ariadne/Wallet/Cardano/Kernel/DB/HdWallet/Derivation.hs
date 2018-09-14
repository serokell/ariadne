module Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Derivation (
      mkHdAccountIx
    , mkHdAddressIx
    , deriveBip44KeyPair
    ) where

import qualified Data.Set as S
import qualified Data.Vector as V
import Named ((!))

import Pos.Core (Address, IsBootstrapEraAddr(..))
import Pos.Crypto
  (EncryptedSecretKey, PassPhrase, ShouldCheckPassphrase(..), encToPublic)

import qualified Ariadne.Wallet.Cardano.Kernel.Bip32 as Bip32
import Ariadne.Wallet.Cardano.Kernel.Bip44 (Bip44DerivationPath)
import qualified Ariadne.Wallet.Cardano.Kernel.Bip44 as Bip44
import qualified Ariadne.Wallet.Cardano.Kernel.DB.HdWallet as HD
import Ariadne.Wallet.Cardano.Kernel.Word31 (Word31)
import qualified Ariadne.Wallet.Cardano.Kernel.Word31 as Word31

-- TODO: make sure that account indexation should be sequential
mkHdAccountIx :: [HD.HdAccount] -> Maybe HD.HdAccountIx
mkHdAccountIx accList = HD.HdAccountIx <$>
    findFirstUnique
        (Word31.unsafeMkWord31 0)
        accIndices
  where
    accIndices :: Vector Word31
    accIndices =
        V.fromList (
            ( HD.getHdAccountIx
            . HD._hdAccountIdIx
            . HD._hdAccountId) <$> accList)

-- TODO: get random index with gap less than 20 (BIP-44)
mkHdAddressIx :: [HD.HdAddress] -> Maybe HD.HdAddressIx
mkHdAddressIx addrList = HD.HdAddressIx <$>
    findFirstUnique
        (Word31.unsafeMkWord31 0)
        addrIndices
  where
    addrIndices :: Vector Word31
    addrIndices =
        V.fromList (
            ( HD.getHdAddressIx
            . HD._hdAddressIdIx
            . HD._hdAddressId) <$> addrList)

-- | This function derives a 3-level address using account index, change index
--   and address index. The input key should be the master key (not the key
--   that was derived from purpose and coin type)
deriveBip44KeyPair
    :: IsBootstrapEraAddr
    -> PassPhrase
    -> EncryptedSecretKey
    -> Bip44DerivationPath
    -> Maybe (Address, EncryptedSecretKey)
deriveBip44KeyPair era pp rootSK bip44DerPath =
    toPair <$>
    Bip32.deriveHDSecretKeyByPath (ShouldCheckPassphrase True) pp rootSK derPath
  where
    derPath :: [Word32]
    derPath = Bip44.encodeBip44DerivationPath bip44DerPath
    toPair :: EncryptedSecretKey -> (Address, EncryptedSecretKey)
    toPair addrSK =
        ( Bip32.makePubKeyHdwAddressUsingPath
              era
              derPath
              ! #root (encToPublic rootSK)
              ! #address (encToPublic addrSK)
        , addrSK)

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

findFirstUnique :: (Ord a, Enum a, Bounded a) => a -> Vector a -> Maybe a
findFirstUnique lastIdx paths =
    fmap head
    $ nonEmpty
    $ dropWhile (`S.member` pathsSet) [lastIdx..maxBound]
  where
    pathsSet = V.foldr S.insert S.empty paths
