{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ariadne.Cardano.Orphans () where

import Pos.Client.CLI.NodeOptions (CommonNodeArgs(..))
import Pos.Client.CLI.Options (CommonArgs(..))
import Pos.Client.Txp.Util (InputSelectionPolicy(..))
import Pos.Infra.Network.CLI (NetworkConfigOpts(..))
import Pos.Infra.Statistics (EkgParams(..), StatsdParams(..))
import Pos.Launcher

deriving instance Eq CommonNodeArgs
deriving instance Eq NetworkConfigOpts
deriving instance Eq CommonArgs
deriving instance Eq EkgParams
deriving instance Eq ConfigurationOptions
deriving instance Eq StatsdParams

deriving instance Ord InputSelectionPolicy
