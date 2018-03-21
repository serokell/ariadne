module Ariadne.UI.Controller where

import qualified Brick as B

import Ariadne.Face (AuxxFace, AuxxEvent)
import Ariadne.UI.Model

-- The Ariadne UI view and controller a single record.
app :: AuxxFace -> B.App AppState AuxxEvent ()
app _ = B.simpleApp (B.txt "Ariadne UI")
