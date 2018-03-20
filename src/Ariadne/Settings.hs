module Ariadne.Settings
       (runSettings) where

import Universum

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

ui :: Widget ()
ui = withBorderStyle unicode $
     borderWithLabel (str "Settings") $
     (center (str "Some settings") <+> vBorder <+> center (str "Some settings"))

runSettings :: IO ()
runSettings = simpleMain ui
