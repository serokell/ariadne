module Knit.Core where

import Data.Text
import Data.Scientific

import Knit.Value
import Knit.Syntax
import Knit.Procedure
import Knit.Eval

data Core

data instance ComponentValue components Core
  = ValueBool Bool
  | ValueNumber Scientific
  | ValueString Text
  | ValueUnit
  | ValueFilePath FilePath
  | ValueList [Value components]

data instance ComponentLit Core
  = LitNumber Scientific
  | LitString Text
  | LitFilePath FilePath

data instance ComponentCommandRepr components Core
  = CommandIdentity (Value components)

instance ComponentLitToValue components Core where
  componentLitToValue = \case
    LitNumber x -> ValueNumber x
    LitString x -> ValueString x
    LitFilePath x -> ValueFilePath x
