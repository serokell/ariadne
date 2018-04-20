{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ariadne.Cardano.Orphans where

import Universum

import Pos.Client.CLI.NodeOptions (CommonNodeArgs(..))
import Pos.Client.CLI.Options (CommonArgs(..))
import Pos.Launcher
import Pos.Network.CLI (NetworkConfigOpts(..))
import Pos.Statistics (EkgParams(..), StatsdParams(..))

deriving instance Eq CommonNodeArgs
deriving instance Eq NetworkConfigOpts
deriving instance Eq CommonArgs
deriving instance Eq EkgParams
deriving instance Eq ConfigurationOptions
deriving instance Eq StatsdParams
