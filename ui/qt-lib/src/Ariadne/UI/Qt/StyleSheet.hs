module Ariadne.UI.Qt.StyleSheet
       ( styleSheet
       ) where

import Data.FileEmbed

styleSheet :: Text
styleSheet = $(embedStringFile "resources/stylesheet.qss")
