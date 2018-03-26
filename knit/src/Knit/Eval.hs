module Knit.Eval where

import Data.Vinyl.TypeLevel
import Data.Type.Equality
import Data.Union

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
  componentCommandExec
    :: ComponentCommandRepr components component
    -> m (Value components)

class ComponentLitToValue components component where
  componentLitToValue
    :: ComponentLit component
    -> ComponentValue components component

evaluate
  :: ( AllConstrained (ComponentCommandExec m components) components
     , AllConstrained (ComponentLitToValue components) components
     , Monad m
     , Ord (Value components)
     )
  => Expr (Some (Elem components) (CommandProc components)) components
  -> m (Either (EvalError components) (Value components))
evaluate expr = runExceptT (eval expr)

eval
  :: ( AllConstrained (ComponentCommandExec m components) components
     , AllConstrained (ComponentLitToValue components) components
     , Monad m
     , Ord (Value components)
     )
  => Expr (Some (Elem components) (CommandProc components)) components
  -> EvalT components m (Value components)
eval = \case
  ExprLit l -> return (literalToValue l)
  ExprProcCall procCall ->
    evalProcCall =<< traverse eval procCall

evalProcCall
  :: forall m components.
     ( AllConstrained (ComponentCommandExec m components) components
     , Monad m
     , Ord (Value components)
     )
  => ProcCall (Some (Elem components) (CommandProc components)) (Value components)
  -> EvalT components m (Value components)
evalProcCall (ProcCall (Some commandProc) args) =
  componentEvalProcCall (ProcCall commandProc args)

literalToValue
  :: forall components.
     AllConstrained (ComponentLitToValue components) components
  => Lit components
  -> Value components
literalToValue =
    Value
  . umapConstrained @(ComponentLitToValue components) componentLitToValue
  . getLitUnion

componentEvalProcCall
  :: forall m component components.
     ( AllConstrained (ComponentCommandExec m components) components
     , Elem components component
     , Monad m
     , Ord (Value components)
     )
  => ProcCall (CommandProc components component) (Value components)
  -> EvalT components m (Value components)
componentEvalProcCall (ProcCall CommandProc{..} args) = do
    e <- either (throwError . InvalidArguments cpName) return $
         consumeArguments cpArgumentConsumer $
         cpArgumentPrepare args
    lift $ commandExec (elemEv @components @component) (cpRepr e)
  where
    commandExec
      :: forall components'.
         AllConstrained (ComponentCommandExec m components) components'
      => Union ((:~:) component) components'
      -> ComponentCommandRepr components component
      -> m (Value components)
    commandExec (This Refl) = componentCommandExec
    commandExec (That i) = commandExec i
