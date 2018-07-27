{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ariadne.Cardano.Orphans where

import Universum

import Pos.Block.Configuration (BlockConfiguration(..))
import Pos.Configuration (NodeConfiguration(..))
import Pos.Core (CoreConfiguration(..), FakeAvvmOptions (..), GenesisConfiguration(..)
    , GenesisInitializer(..), GenesisSpec(..), TestnetBalanceOptions(..))
import Pos.Client.CLI.NodeOptions (CommonNodeArgs(..))
import Pos.Client.CLI.Options (CommonArgs(..))
import Pos.Client.Txp.Util (InputSelectionPolicy(..))
import Pos.Infra.Network.CLI (NetworkConfigOpts(..))
import Pos.Infra.Ntp.Configuration (NtpConfiguration (..))
import Pos.Infra.Statistics (EkgParams(..), StatsdParams(..))
import Pos.Launcher (Configuration(..), ConfigurationOptions(..))
import Pos.Update.Configuration (UpdateConfiguration(..))
import Pos.Ssc.Configuration (SscConfiguration(..))

deriving instance Eq CommonNodeArgs
deriving instance Eq NetworkConfigOpts
deriving instance Eq CommonArgs
deriving instance Eq EkgParams
deriving instance Eq ConfigurationOptions
deriving instance Eq StatsdParams
deriving instance Eq BlockConfiguration
deriving instance Eq CoreConfiguration
deriving instance Eq FakeAvvmOptions
deriving instance Eq GenesisConfiguration
deriving instance Eq GenesisInitializer
deriving instance Eq GenesisSpec
deriving instance Eq NodeConfiguration
deriving instance Eq NtpConfiguration
deriving instance Eq UpdateConfiguration
deriving instance Eq SscConfiguration
deriving instance Eq TestnetBalanceOptions
deriving instance Eq Configuration

deriving instance Ord InputSelectionPolicy
