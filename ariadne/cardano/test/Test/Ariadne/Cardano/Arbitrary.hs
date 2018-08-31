{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Ariadne.Cardano.Arbitrary where

import Universum

import Ariadne.Config.Cardano (CardanoConfig(..))
import Data.Text (pack)
import Data.Time.Units (fromMicroseconds)
import Pos.Core.Slotting (Timestamp(..))
import Pos.Infra.Network.Types (NodeName(..))
import Pos.Infra.Statistics (EkgParams(..))
import Pos.Infra.Util.TimeWarp (NetworkAddress)
import Pos.Launcher (ConfigurationOptions(..))
import Test.QuickCheck (Gen, suchThat)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose, elements, listOf, oneof)

import qualified Data.ByteString.Char8 as B8

instance Arbitrary CardanoConfig where
    arbitrary = do
        ccDbPath <- genMaybe genValidString
        ccRebuildDB <- arbitrary
        ccNetworkTopology <- genMaybe genValidString
        ccNetworkNodeId <- genMaybe $ NodeName <$> genValidText
        ccNetworkPort <- arbitrary
        ccLogConfig <- genMaybe genValidString
        ccLogPrefix <- genMaybe genValidString
        ccConfigurationOptions <- genConfigurationOptions
        ccEnableMetrics <- arbitrary
        ccEkgParams <- genMaybe genEkgParams
        return CardanoConfig {..}

genEkgParams :: Gen EkgParams
genEkgParams = do
    ekgPort <- (arbitrary :: Gen Int) `suchThat` (> 0)
    ekgHost <- genValidByteString
    return EkgParams {..}

genConfigurationOptions :: Gen ConfigurationOptions
genConfigurationOptions = do
    cfoFilePath <- genValidString
    cfoKey <- genValidText
    cfoSystemStart <- genMaybe genValidTimestamp
    cfoSeed <- genMaybe ((arbitrary :: Gen Integer) `suchThat` (> 0))
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
