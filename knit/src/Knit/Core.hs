module Knit.Core where

import Data.Text
import Data.Scientific
import Control.Lens
import Control.Monad

import Knit.Value
import Knit.Argument
import Knit.Syntax
import Knit.Procedure
import Knit.Eval
import Knit.Tokenizer
import Knit.Utils

data Core

data instance ComponentValue components Core
  = ValueBool Bool
  | ValueNumber Scientific
  | ValueString Text
  | ValueUnit
  | ValueFilePath FilePath
  | ValueList [Value components]

deriving instance Eq (Value components) => Eq (ComponentValue components Core)
deriving instance Ord (Value components) => Ord (ComponentValue components Core)
deriving instance Show (Value components) => Show (ComponentValue components Core)

makePrisms 'ValueBool

data instance ComponentLit Core
  = LitNumber Scientific
  | LitString Text
  | LitFilePath FilePath
  | LitUnit
  deriving (Eq, Ord, Show)

data instance ComponentToken Core

deriving instance Eq (ComponentToken Core)
deriving instance Ord (ComponentToken Core)
deriving instance Show (ComponentToken Core)

data instance ComponentCommandRepr components Core
  = CommandIdentity (Value components)

instance ComponentLitToValue components Core where
  componentLitToValue = \case
    LitNumber x -> ValueNumber x
    LitString x -> ValueString x
    LitFilePath x -> ValueFilePath x
    LitUnit -> ValueUnit

data instance ComponentExecContext Core = CoreExecCtx

instance Applicative m => ComponentCommandExec m components Core where
  componentCommandExec CoreExecCtx (CommandIdentity v) = pure v

instance Elem components Core => ComponentCommandProcs components Core where
  componentCommandProcs =
    [ CommandProc
        { cpName = "true"
        , cpArgumentPrepare = id
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandIdentity (toValue (ValueBool True))
        , cpHelp = "The logical truth value"
        }
    , CommandProc
        { cpName = "false"
        , cpArgumentPrepare = id
        , cpArgumentConsumer = pure ()
        , cpRepr = \() -> CommandIdentity (toValue (ValueBool False))
        , cpHelp = "The logical falsehood value"
        }
    , CommandProc
        { cpName = "not"
        , cpArgumentPrepare = id
        , cpArgumentConsumer = getArg tyBool "a"
        , cpRepr = \v -> CommandIdentity (toValue (ValueBool (not v)))
        , cpHelp = "The logical falsehood value"
        }
    ]

tyBool :: Elem components Core => TyProjection components Bool
tyBool =
  TyProjection
    { tpTypeName = "Bool"
    , tpMatcher = preview _ValueBool <=< fromValue
    }
