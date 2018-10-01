module Ariadne.Version (currentAriadneVersion) where

import Data.Version (Version(..))
import Paths_ariadne_core (version)

currentAriadneVersion :: Data.Version.Version
currentAriadneVersion = version
