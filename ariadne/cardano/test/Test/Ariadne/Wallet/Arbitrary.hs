{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Ariadne.Wallet.Arbitrary () where

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (chooseAny, elements)

import Ariadne.Config.Wallet (WalletConfig(..))

import Test.Ariadne.Cardano.Arbitrary (genValidString)

import Time (sec)

instance Arbitrary WalletConfig where
    arbitrary = do
        wcEntropySize       <- elements [16, 20, 24, 28, 32]
        wcKeyfilePath       <- genValidString
        wcAcidDBPath        <- genValidString
        wcNumStoredArchives <- chooseAny
        wcDBCleanupPeriod   <- sec . realToFrac @Double <$> chooseAny
        return WalletConfig {..}
