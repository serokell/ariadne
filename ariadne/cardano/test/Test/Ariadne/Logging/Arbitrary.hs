{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Ariadne.Logging.Arbitrary () where

import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Ariadne.Config.Logging (LoggingConfig(..))

import Test.Ariadne.Cardano.Arbitrary (genValidString)

instance Arbitrary LoggingConfig where
    arbitrary = do
        lcPath <- genValidString
        return LoggingConfig {..}
