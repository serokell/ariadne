{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Ariadne.Cardano.Arbitrary where

import Universum

import Ariadne.Config.Cardano (CardanoConfig(..), CommonArgs(..)
    , CommonNodeArgs(..), ConfigurationOptions(..), NetworkConfigOpts(..))
import Ariadne.Config.Presence (Presence(..))
import Data.Text (pack)
import Data.Time.Units (fromMicroseconds)
import Pos.Core.Slotting (Timestamp(..))
import Pos.Infra.Statistics (EkgParams(..), StatsdParams(..))
import Pos.Infra.Util.TimeWarp (NetworkAddress)
import Test.QuickCheck (Gen, suchThat)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose, elements, listOf, oneof)

import qualified Data.ByteString.Char8 as B8

instance Arbitrary CardanoConfig where
  arbitrary = CardanoConfig <$> genCommonNodeArgs

genCommonNodeArgs :: Gen CommonNodeArgs
genCommonNodeArgs = do
    dbPath <- genMaybe genValidString
    rebuildDB <- arbitrary
    devGenesisSecretI <- arbitrary
    keyfilePath <- genValidString
    networkConfigOpts <- genNetworkConfigOpts
    commonArgs <- genCommonArgs
    enableMetrics <- arbitrary
    ekgParams <- genMaybe genEkgParams
    return CommonNodeArgs {..}

genStatsdParams :: Gen StatsdParams
genStatsdParams = do
    statsdHost <- genValidText
    statsdPort <- (arbitrary :: Gen Int) `suchThat` (> 0)
    statsdInterval <- arbitrary
    statsdDebug <- arbitrary
    statsdPrefix <- genValidText
    statsdSuffix <- genValidText
    return StatsdParams {..}

genNetworkConfigOpts :: Gen NetworkConfigOpts
genNetworkConfigOpts = do
    ncoTopology <- File <$> genMaybe genValidString
    ncoPort <- arbitrary
    return NetworkConfigOpts {..}

genCommonArgs :: Gen CommonArgs
genCommonArgs = do
    logConfig <- File <$> genMaybe genValidString
    logPrefix <- genMaybe genValidString
    configurationOptions <- genConfigurationOptions
    return CommonArgs {..}

genEkgParams :: Gen EkgParams
genEkgParams = do
    ekgPort <- (arbitrary :: Gen Int) `suchThat` (> 0)
    ekgHost <- genValidByteString
    return EkgParams {..}

genConfigurationOptions :: Gen ConfigurationOptions
genConfigurationOptions = do
    cfo <- File . Just <$> genValidString
    return ConfigurationOptions {..}

-- Assume that Timestamp is mlillions of microseconds so seconds is integer.
-- It is needed due to rounding in Millisecond -> Second convertion in injection
genValidTimestamp :: Gen Timestamp
genValidTimestamp = Timestamp . fromMicroseconds . (toInteger @Int) . (* 10 ^ (6 :: Int)) <$> choose (0, 100 :: Int)

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = oneof [pure Nothing, Just <$> g]

genValidText :: Gen Text
genValidText = fmap pack genValidString

genValidByteString :: Gen ByteString
genValidByteString = fmap B8.pack genValidString

genValidString :: Gen String
genValidString = listOf (elements symbList)
  where
    symbList :: String
    symbList =
        ['a'..'z'] <>
        ['A'..'Z'] <>
        ['0'..'9'] <>
        ['.', '/']

genNetworkAddress :: Gen NetworkAddress
genNetworkAddress = do
  host <- genValidByteString
  port <- arbitrary
  return (host, port)
