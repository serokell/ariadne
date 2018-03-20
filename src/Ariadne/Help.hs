module Ariadne.Help
       (runHelp) where

import Universum

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

ui :: Widget ()
ui = withBorderStyle unicode $
     borderWithLabel (str "Help") $
     (center (str "Some help") <+> vBorder <+> center (str "Some help"))

runHelp :: IO ()
runHelp = simpleMain ui
