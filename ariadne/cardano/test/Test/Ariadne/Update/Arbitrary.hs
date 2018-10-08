{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Ariadne.Update.Arbitrary () where

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import Ariadne.Config.Update (UpdateConfig(..))

import Test.Ariadne.Cardano.Arbitrary (genValidText)

instance Arbitrary UpdateConfig where
    arbitrary = do
        ucVersionCheckUrl <- genValidText
        ucUpdateUrl <- genValidText
        ucCheckDelay <- elements [3600, 7200..21600]
        return UpdateConfig {..}
