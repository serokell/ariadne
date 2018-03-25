module Knit.Eval where

import Data.Vinyl.TypeLevel

import Knit.Syntax
import Knit.Value
import Knit.Utils
import Knit.Procedure
import Knit.Name
import Knit.Argument

import Control.Monad.Except



data EvalError components = InvalidArguments Name (ProcError components)

deriving instance (Eq (Value components)) => Eq (EvalError components)
deriving instance (Ord (Value components)) => Ord (EvalError components)
deriving instance (Show (Value components)) => Show (EvalError components)

type EvalT components = ExceptT (EvalError components)

class ComponentCommandExec m components component where
  componentCommandExec :: ComponentCommandRepr components component -> m (Value components)

class ComponentLitToValue components component where
  componentLitToValue :: ComponentLit component -> ComponentValue components component

literalToValue :: forall components. AllConstrained (ComponentLitToValue components) components => Lit components -> Value components
literalToValue  = Value . umapConstrained @(ComponentLitToValue components) componentLitToValue . getLitUnion

evalProcCall :: AllConstrained (ComponentCommandExec m components) components => Union Proxy components -> ProcCall (CommandProc components component) (Value components) -> EvalT components m (Value components)
evalProcCall (ProcCall CommandProc{..} args) = do
    e <- either (throwError . InvalidArguments cpName) return $
         consumeArguments cpArgumentConsumer $
         cpArgumentPrepare args
    lift . componentCommandExec $ cpRepr e
