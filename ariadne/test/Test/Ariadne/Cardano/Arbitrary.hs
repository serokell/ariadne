{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Ariadne.Cardano.Arbitrary where

import Universum

import Ariadne.Config.Cardano (CardanoConfig(..))
import Data.Text (pack)
import Data.Time.Units (fromMicroseconds)
import Pos.Client.CLI.NodeOptions (CommonNodeArgs(..))
import Pos.Client.CLI.Options (CommonArgs(..))
import Pos.Core.Slotting (Timestamp(..))
import Pos.Launcher
import Pos.Network.CLI (NetworkConfigOpts(..))
import Pos.Network.Types (NodeName(..))
import Pos.Statistics (EkgParams(..), StatsdParams(..))
import Pos.Util.TimeWarp (NetworkAddress)
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
    jlPath <- genMaybe genValidString
    commonArgs <- genCommonArgs
    updateLatestPath <- genValidString
    updateWithPackage <- arbitrary
    noNTP <- arbitrary
    route53Params <- genMaybe genNetworkAddress
    enableMetrics <- arbitrary
    ekgParams <- genMaybe genEkgParams
    statsdParams <- genMaybe genStatsdParams
    cnaDumpGenesisDataPath <- genMaybe genValidString
    cnaDumpConfiguration <- arbitrary
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
    ncoTopology <- genMaybe genValidString
    ncoKademlia <- genMaybe genValidString
    ncoSelf <- genMaybe $ NodeName <$> genValidText
    ncoPort <- arbitrary
    ncoPolicies <- genMaybe genValidString
    ncoBindAddress <- genMaybe genNetworkAddress
    ncoExternalAddress <- genMaybe genNetworkAddress
    return NetworkConfigOpts {..}

genCommonArgs :: Gen CommonArgs
genCommonArgs = do
    logConfig <- genMaybe genValidString
    logPrefix <- genMaybe genValidString
    reportServers <- listOf genValidText
    updateServers <- listOf genValidText
    configurationOptions <- genConfigurationOptions
    return CommonArgs {..}

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
