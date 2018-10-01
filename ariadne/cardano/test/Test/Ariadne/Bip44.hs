{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Ariadne.Bip44
       ( bip44PathGen
       , bip44KeyPairGen
       ) where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, conjoin, property, (.&&.), (===))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen(..))

import Ariadne.Wallet.Cardano.Kernel.Bip44
  (Bip44DerivationPath(..), bip44Purpose, decodeBip44DerivationPath,
  encodeBip44DerivationPath)
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet
  (HdAccountIx(..), HdAddressChain(..), HdAddressIx(..))
import Ariadne.Wallet.Cardano.Kernel.DB.HdWallet.Derivation
  (deriveBip44KeyPairUnwrapped)
import Ariadne.Wallet.Cardano.Kernel.Word31
  (Word31, unsafeMkWord31, word31ToWord32)
import Cardano.Crypto.Wallet (XPrv, generate, toXPub, unXPrv, xprv, xpub)

import Pos.Core (Address(..), IsBootstrapEraAddr(..), makePubKeyHdwAddress)
import Pos.Core.NetworkMagic (NetworkMagic(..))
import Pos.Crypto.HD (firstHardened, firstNonHardened)

import qualified Data.ByteString.Base16 as B16
import Data.Default (def)
import qualified Pos.Crypto as S

instance Arbitrary Bip44DerivationPath where
    arbitrary = do
        bip44AccountIndex <- arbitrary
        bip44AddressChain <- arbitrary
        bip44AddressIndex <- arbitrary
        return Bip44DerivationPath {..}

{-------------------------------------------------------------------------------
  Check keys derivation
-------------------------------------------------------------------------------}

-- | Test vector. Contains seed, passphrase, path and reference (derived) private and public keys.
data TestVector = TestVector
    { tvSeed       :: !ByteString
    , tvPassPhrase :: !ByteString
    , tvPath       :: ![Word32]
    , tvXPrvString :: !ByteString
    , tvXPubString :: !ByteString
    }


-- | Test vectors are taken from
-- https://github.com/input-output-hk/cardano-crypto/blob/master/tests/goldens/cardano/crypto/wallet/BIP39-128
testVectors :: [TestVector]
testVectors =
    [ TestVector
        { tvSeed = fst $ B16.decode $
            "58202ed4c71d91bc68c7b50feeb5bc7a785fe884dd0aeddce029df3d612cd3680fd3"
        , tvPassPhrase = ""
        , tvPath = []
        , tvXPrvString = fst $ B16.decode $
            "6065a956b1b34145c4416fdc3ba3276801850e91a77a31a7be782463288aea5360ba\
            \6e25b1a02157fb69c5d1d7b96c4619736e545447069a6a6f0ba90844bc8e64b20fa0\
            \82b3143d6b5eed42c6ef63f99599d0888afe060620abc1b319935fe1739f4b3caca4\
            \c9ad4fcd4bdc2ef42c8601af8d6946999ef85ef6ae84f66e72eb"
        , tvXPubString = fst $ B16.decode $
            "64b20fa082b3143d6b5eed42c6ef63f99599d0888afe060620abc1b319935fe1739f\
            \4b3caca4c9ad4fcd4bdc2ef42c8601af8d6946999ef85ef6ae84f66e72eb"
        },
      TestVector
        { tvSeed = fst $ B16.decode $
            "58202ed4c71d91bc68c7b50feeb5bc7a785fe884dd0aeddce029df3d612cd3680fd3"
        , tvPassPhrase = ""
        , tvPath = [2147483648]
        , tvXPrvString = fst $ B16.decode $
            "e7d27516538403a53a8b041656a3f570909df641a0ab811fe7d87c9ba02a830c794a\
            \2c54ad8b525b781773c87d38cbf4197636bc427a9d551368286fe4c294a495bb82ff\
            \d5707716bc65170ab4e8dafeed90fbe0ce9258713b7751e962d931df6755cb82e892\
            \d6614c007a5efbceb21d95a5244e269d0e206b48b9a495390b03"
        , tvXPubString = fst $ B16.decode $
            "95bb82ffd5707716bc65170ab4e8dafeed90fbe0ce9258713b7751e962d931df6755\
            \cb82e892d6614c007a5efbceb21d95a5244e269d0e206b48b9a495390b03"
        },
      TestVector
        { tvSeed = fst $ B16.decode $
            "58202ed4c71d91bc68c7b50feeb5bc7a785fe884dd0aeddce029df3d612cd3680fd3"
        , tvPassPhrase = ""
        , tvPath = [2147483649]
        , tvXPrvString = fst $ B16.decode $
            "9b5a3d9a4c60bcd49bb64b72c082b164314d0f61d842f2575fd1d4fb30a28a0cb093\
            \e376f41eb7bf80abcd0073a52455d25b5d21815bc758e5f6f81536aedebb79fc8154\
            \554b97e4c56ef2f9dbb4c1421ff19509688931a1e964bda5dec0f19f47a242713bd1\
            \8608231147c066b6083bfc1e9066fec9f621844c84fed6228a34"
        , tvXPubString = fst $ B16.decode $
            "79fc8154554b97e4c56ef2f9dbb4c1421ff19509688931a1e964bda5dec0f19f47a2\
            \42713bd18608231147c066b6083bfc1e9066fec9f621844c84fed6228a34"
        },
      TestVector
        { tvSeed = fst $ B16.decode $
            "58202ed4c71d91bc68c7b50feeb5bc7a785fe884dd0aeddce029df3d612cd3680fd3"
        , tvPassPhrase = ""
        , tvPath = [2147483648, 2147483649]
        , tvXPrvString = fst $ B16.decode $
            "52e0c98aa600cfdcd1ff28fcda5227ed87063f4a98547a78b771052cf102b40c6c18\
            \d9f8075b1a6a1833540607479bd58b7beb8a83d2bb01ca7ae02452a25803dc907c7c\
            \06e6314eedd9e18c9f6c6f9cc4e205fb1c70da608234c319f1f7b0d6d6798491b9fa\
            \4612370ae5ef3c623a0b6872f3ad8f26970885fa67c83bdc425e"
        , tvXPubString = fst $ B16.decode $
            "dc907c7c06e6314eedd9e18c9f6c6f9cc4e205fb1c70da608234c319f1f7b0d6d679\
            \8491b9fa4612370ae5ef3c623a0b6872f3ad8f26970885fa67c83bdc425e"
        },
      TestVector
        { tvSeed = fst $ B16.decode $
            "58202ed4c71d91bc68c7b50feeb5bc7a785fe884dd0aeddce029df3d612cd3680fd3"
        , tvPassPhrase = ""
        , tvPath = [2147483648, 2147483649, 2147483650]
        , tvXPrvString = fst $ B16.decode $
            "11fd6462a3a92b35c22703f6f1c124ddcf36b7c2b09cc2784f320e1cfa12ec04c278\
            \5803c61c46aeca192a1bb1b7b20a8c4cc7fa01db57fc5d1d8a5473402352839775a4\
            \1876e328986aa26168958bba1176e67819b357eea84afceab8b1db784169a2a32e36\
            \18a903e930bd1a713033a38f92389093408394e29ac37a1752ea"
        , tvXPubString = fst $ B16.decode $
            "839775a41876e328986aa26168958bba1176e67819b357eea84afceab8b1db784169\
            \a2a32e3618a903e930bd1a713033a38f92389093408394e29ac37a1752ea"
        },
      TestVector
        { tvSeed = fst $ B16.decode $
            "58202ed4c71d91bc68c7b50feeb5bc7a785fe884dd0aeddce029df3d612cd3680fd3"
        , tvPassPhrase = ""
        , tvPath = [2147483648, 2147483649, 2147483650, 2147483650]
        , tvXPrvString = fst $ B16.decode $
            "5b1e5cad02274ba461f4708d8598d3497faf8fe3e894a379573aa6ac3a03e505ba17\
            \9d2e3c67aabb486c48d16002b51ad32eab434c738a1550962313b07098cd75eb8d19\
            \7ec8627c85af88e66aa1e49065dd8ac98ed8991db52ece01635dfb763ae9c99a5925\
            \cba2dcf121baf3a0254f3dea23c129f9eb70a8a7e8897c5199ba"
        , tvXPubString = fst $ B16.decode $
            "75eb8d197ec8627c85af88e66aa1e49065dd8ac98ed8991db52ece01635dfb763ae9\
            \c99a5925cba2dcf121baf3a0254f3dea23c129f9eb70a8a7e8897c5199ba"
        },
      TestVector
        { tvSeed = fst $ B16.decode $
            "58202ed4c71d91bc68c7b50feeb5bc7a785fe884dd0aeddce029df3d612cd3680fd3"
        , tvPassPhrase = ""
        , tvPath = [2147483648, 2147483649, 2147483650, 2147483650, 3147483648]
        , tvXPrvString = fst $ B16.decode $
            "624b47150f58dfa44284fbc63c9f99b9b79f808c4955a461f0e2be44eb0be50d097\
            \aa006d694b165ef37cf23562e5967c96e49255d2f20faae478dee83aa5b02058858\
            \9cd9b51dfc028cf225674069cbe52e0e70deb02dc45b79b26ee3548b0015c450b86\
            \dd7dd83b31951d9ee03eb1a7925161d817bd517c69cf09e3671f1ca"
        , tvXPubString = fst $ B16.decode $
            "0588589cd9b51dfc028cf225674069cbe52e0e70deb02dc45b79b26ee3548b0015c\
            \450b86dd7dd83b31951d9ee03eb1a7925161d817bd517c69cf09e3671f1ca"
        }
    ]

-- | This function checks equality of derived keypair with reference
-- (from test vector)
checkKeyPairGeneration :: TestVector -> Either String Property
checkKeyPairGeneration TestVector{..} = do
    expectedXPrv <- xprv tvXPrvString
    expectedAddress <- mkExpectedAddress

    let (actualAddress, actualEsk) =
            fromMaybe (error "Failed to derive bip44 keypair") $
                deriveBip44KeyPairUnwrapped
                    NMNothing
                    era
                    S.emptyPassphrase
                    rootEsk
                    tvPath
        (S.SecretKey actualXPrv) = S.encToSecret actualEsk

    return $ property $
        expectedAddress === actualAddress .&&. (unXPrv expectedXPrv) === (unXPrv actualXPrv)
  where
    era :: IsBootstrapEraAddr
    era = IsBootstrapEraAddr False

    rootXPrv :: XPrv
    rootXPrv = generate tvSeed tvPassPhrase

    rootEsk :: S.EncryptedSecretKey
    rootEsk = S.EncryptedSecretKey rootXPrv testEncryptedPass
      where
        scryptParams :: S.ScryptParams
        scryptParams =
            fromMaybe (error "failed to make scrypt params through builder") $
                S.mkScryptParams def

        testEncryptedPass :: S.EncryptedPass
        testEncryptedPass =
            S.encryptPassWithSalt scryptParams S.emptySalt ("" :: ByteString)

    mkExpectedAddress :: Either String Address
    mkExpectedAddress = do
        publicKey <- S.PublicKey <$> xpub tvXPubString
        let rootPubKey = toXPub rootXPrv
            rootPassPhrase = S.deriveHDPassphrase (S.PublicKey rootPubKey)
            payload = S.packHDAddressAttr rootPassPhrase tvPath
        return $ makePubKeyHdwAddress NMNothing era payload publicKey

checkAllVectors :: [TestVector] -> Property
checkAllVectors vectors =
    let propertiesE = checkKeyPairGeneration <$> vectors
    in if null (lefts propertiesE)
        then conjoin $ rights propertiesE
        else error (unlines $ toText <$> lefts propertiesE)

bip44KeyPairGen :: Spec
bip44KeyPairGen = describe "Bip44 keypair generation" $ do
    prop "Check keypair derivation" $
        checkAllVectors testVectors

{-------------------------------------------------------------------------------
  Check path encoding and decoding
-------------------------------------------------------------------------------}

bip44ReferencePath :: Bip44DerivationPath
bip44ReferencePath =
    let bip44AccountIndex = HdAccountIx $ unsafeMkWord31 0
        bip44AddressChain = HdChainExternal
        bip44AddressIndex = HdAddressIx $ unsafeMkWord31 1
    in Bip44DerivationPath{..}

bip44W32Rep :: [Word32]
bip44W32Rep = [2147483692, 2147485463, 2147483648, 0, 1]

newtype Bip44Rep = Bip44Rep [Word32]
    deriving Show

genBip44DerPathUntyped :: Gen Bip44Rep
genBip44DerPathUntyped = do
    let purpose = toHardened bip44Purpose
    accIndex <- toHardened <$> arbitrary
    chainType <- arbitrary
    let chainTypeRep =
            toNonHardened $ case chainType of
                HdChainExternal -> unsafeMkWord31 0
                HdChainInternal -> unsafeMkWord31 1
    addrIndex <- toNonHardened <$> arbitrary
    return $ Bip44Rep [purpose, accIndex, chainTypeRep, addrIndex]
  where
    toNonHardened :: Word31 -> Word32
    toNonHardened (word31ToWord32 -> n) = firstNonHardened + n
    toHardened :: Word31 -> Word32
    toHardened (word31ToWord32 -> n) = firstHardened + n

instance Arbitrary Bip44Rep where
    arbitrary = genBip44DerPathUntyped

bip44RandomEncoding :: Bip44DerivationPath -> Property
bip44RandomEncoding derPath = do
    let converted = (decodeBip44DerivationPath . encodeBip44DerivationPath) derPath
    property (Just derPath === converted)

-- | Here we check encoding of the reference path and
-- the property that (decodeBip44DerivationPath . encodeBip44DerivationPath)
-- is identity
bip44PathGen :: Spec
bip44PathGen = describe "Bip44 path conversion between representations." $ do
    prop "Convert from randomly-generated untyped path" bip44RandomEncoding
    prop "Encode reference bip44 path" $ property $
        encodeBip44DerivationPath bip44ReferencePath === bip44W32Rep
