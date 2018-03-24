module Knit.Core where

import Data.Text
import Data.Scientific

import Knit.Value
import Knit.Syntax

data Core

data instance ComponentValue v Core
  = ValueBool Bool
  | ValueNumber Scientific
  | ValueString Text
  | ValueUnit
  | ValueFilePath FilePath
  | ValueList [v]

data instance ComponentLit Core
  = LitNumber Scientific
  | LitString String
  | LitFilePath FilePath
