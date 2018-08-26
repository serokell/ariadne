module Knit.Eval where

import Control.Monad.Except
import Data.Type.Equality

import Knit.Argument
import Knit.Prelude
import Knit.Procedure
import Knit.Syntax
import Knit.Value

data EvalError components = InvalidArguments CommandId (ProcError components)

deriving instance (Eq (Value components)) => Eq (EvalError components)
deriving instance (Ord (Value components)) => Ord (EvalError components)
deriving instance (Show (Value components)) => Show (EvalError components)

type EvalT components = ExceptT (EvalError components)

type ExecContext m components = Rec (ComponentExecContext m components) components

data family ComponentExecContext (m :: * -> *) (components :: [*]) component

class ComponentCommandExec m components component where
  componentCommandExec
    :: ComponentExecContext m components component
    -> ComponentCommandRepr components component
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
  => ExecContext m components
  -> Expr (SomeCommandProc components) components
  -> m (Either (EvalError components) (Value components))
evaluate ctxs expr = runExceptT (eval ctxs expr)

eval
  :: ( AllConstrained (ComponentCommandExec m components) components
     , AllConstrained (ComponentLitToValue components) components
     , Monad m
     , Ord (Value components)
     )
  => ExecContext m components
  -> Expr (SomeCommandProc components) components
  -> EvalT components m (Value components)
eval ctxs = \case
  ExprLit l -> return (literalToValue l)
  ExprProcCall procCall ->
    evalProcCall ctxs =<< traverse (eval ctxs) procCall

evalProcCall
  :: forall m components.
     ( AllConstrained (ComponentCommandExec m components) components
     , Monad m
     , Ord (Value components)
     )
  => ExecContext m components
  -> ProcCall (SomeCommandProc components) (Value components)
  -> EvalT components m (Value components)
evalProcCall ctxs (ProcCall (SomeCommandProc commandProc) args) =
  componentEvalProcCall (rget ctxs) (ProcCall commandProc args)

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
  => ComponentExecContext m components component
  -> ProcCall (CommandProc components component) (Value components)
  -> EvalT components m (Value components)
componentEvalProcCall ctx (ProcCall CommandProc{..} args) = do
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
    commandExec (Base v) = absurd v
    commandExec (Step (Left Refl)) = componentCommandExec ctx
    commandExec (Step (Right i)) = commandExec i
