module Ariadne.UI.Qt.StyleSheet
  ( styleSheet
  ) where

import Data.FileEmbed
import Data.Text

styleSheet :: Text
styleSheet = $(embedStringFile "resources/stylesheet.qss")
