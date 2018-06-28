module Ariadne.Wallet.Cardano.Kernel.Bip44
    ( Bip44DerivationPath (..)
    , decodeBip44DerivationPath
    , encodeBip44DerivationPath
    , encodeBip44DerivationPathToAccount
    , encodeBip44DerivationPathFromAccount
    ) where

import Universum

import Pos.Crypto.HD (firstHardened, firstNonHardened, isHardened)

import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (HdAccountIx(..), HdAddressChain(..), HdAddressIx(..))
import Ariadne.Wallet.Cardano.Kernel.Word31
  (Word31, unsafeMkWord31, word31ToWord32)

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | BIP-44 derivation constant
bip44Purpose :: Word31
bip44Purpose = unsafeMkWord31 44

-- | ADA coin index. This is the year when Ada Lovelace was born.
-- https://github.com/satoshilabs/slips/blob/master/slip-0044.md
bip44CoinType :: Word31
bip44CoinType = unsafeMkWord31 1815

{-------------------------------------------------------------------------------
  BIP-44 conversion functions
-------------------------------------------------------------------------------}

-- | Full derivation path for an address in application-level terms.
--
-- In fact, this is almost HdAddressId, with the exception that
-- it does not include the corresponding HdRootId.
data Bip44DerivationPath = Bip44DerivationPath
    { bip44AccountIndex :: HdAccountIx
    , bip44AddressChain :: HdAddressChain
    , bip44AddressIndex :: HdAddressIx
    } deriving (Eq, Ord, Show)

decodeBip44DerivationPath :: [Word32] -> Maybe Bip44DerivationPath
decodeBip44DerivationPath derPathList = do
    -- The bang is needed due to a GHC bug with ApplicativeDo
    -- (https://ghc.haskell.org/trac/ghc/ticket/14105, fixed in 8.4.1)
    ![purpose', coinType', accIdx', chainType, addrIdx] <- pure derPathList

    purpose  <- fromHardened purpose'
    guard $ purpose  == bip44Purpose
    coinType <- fromHardened coinType'
    guard $ coinType == bip44CoinType

    bip44AccountIndex <- HdAccountIx <$> fromHardened accIdx'
    bip44AddressChain <- mkAddressChain =<< fromNonHardened chainType
    bip44AddressIndex <- HdAddressIx <$> fromNonHardened addrIdx
    pure $ Bip44DerivationPath {..}
  where
    fromNonHardened :: Word32 -> Maybe Word31
    fromNonHardened idx = do
        guard $ not $ isHardened idx
        pure $ unsafeMkWord31 $ idx - firstNonHardened

    fromHardened :: Word32 -> Maybe Word31
    fromHardened idx = do
        guard $ isHardened idx
        pure $ unsafeMkWord31 $ idx - firstHardened

    mkAddressChain :: Word31 -> Maybe HdAddressChain
    mkAddressChain (word31ToWord32 -> n)
        | n == 0 = Just HdChainExternal
        | n == 1 = Just HdChainInternal
        | otherwise = Nothing

-- | Construct a derivation path (a list of indices, as specified in
-- BIP-32) from root key to address, according to the BIP-44
-- specification.
encodeBip44DerivationPath :: Bip44DerivationPath -> [Word32]
encodeBip44DerivationPath Bip44DerivationPath {..} =
    encodeBip44DerivationPathToAccount bip44AccountIndex ++
    encodeBip44DerivationPathFromAccount bip44AddressChain bip44AddressIndex

-- | Construct a derivation path (a list of indices, as specified in
-- BIP-32) from root key to account, according to the BIP-44
-- specification.
encodeBip44DerivationPathToAccount :: HdAccountIx -> [Word32]
encodeBip44DerivationPathToAccount (HdAccountIx accountIdx) =
    [ toHardened bip44Purpose
    , toHardened bip44CoinType
    , toHardened accountIdx
    ]
  where
    toHardened :: Word31 -> Word32
    toHardened (word31ToWord32 -> n) = firstHardened + n

-- | Construct a derivation path (a list of indices, as specified in
-- BIP-32) from account to address, according to the BIP-44
-- specification.
encodeBip44DerivationPathFromAccount :: HdAddressChain -> HdAddressIx -> [Word32]
encodeBip44DerivationPathFromAccount hdAddressChain (HdAddressIx addressIdx) =
    [ toNonHardened $ case hdAddressChain of
        HdChainExternal -> unsafeMkWord31 0
        HdChainInternal -> unsafeMkWord31 1
    , toNonHardened addressIdx
    ]
  where
    toNonHardened :: Word31 -> Word32
    toNonHardened (word31ToWord32 -> n) = firstNonHardened + n
