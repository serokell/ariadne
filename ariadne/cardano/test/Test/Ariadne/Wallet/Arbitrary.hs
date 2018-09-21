{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Ariadne.Wallet.Arbitrary where

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import Ariadne.Config.Wallet (WalletConfig(..))

import Test.Ariadne.Cardano.Arbitrary (genValidString)

instance Arbitrary WalletConfig where
    arbitrary = do
        wcEntropySize <- elements [16, 20, 24, 28, 32]
        wcKeyfilePath <- genValidString
        wcAcidDBPath <- genValidString
        return WalletConfig {..}
