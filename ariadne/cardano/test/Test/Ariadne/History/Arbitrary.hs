{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Ariadne.History.Arbitrary () where

import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Ariadne.Config.History (HistoryConfig(..))

import Test.Ariadne.Cardano.Arbitrary (genValidString)

instance Arbitrary HistoryConfig where
    arbitrary = do
        hcPath <- genValidString
        return HistoryConfig {..}
