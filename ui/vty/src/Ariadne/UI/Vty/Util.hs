module Ariadne.UI.Vty.Util (postfixLFields) where

import Control.Lens (LensRules, lensField, lensRules, mappingNamer, (&), (.~))
import Prelude ((++))

postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ mappingNamer (\s -> [s++"L"])
