module Knit.Eval where

import Data.Vinyl.Core
import Data.Vinyl.TypeLevel
import Data.Type.Equality
import Data.Union

import Knit.Syntax
import Knit.Value
import Knit.Utils
import Knit.Procedure
import Knit.Argument

import Control.Monad.Except

data EvalError components = InvalidArguments CommandName (ProcError components)

deriving instance (Eq (Value components)) => Eq (EvalError components)
deriving instance (Ord (Value components)) => Ord (EvalError components)
deriving instance (Show (Value components)) => Show (EvalError components)

type EvalT components = ExceptT (EvalError components)

type ExecContext = Rec ComponentExecContext

data family ComponentExecContext component

class ComponentCommandExec m components component where
  componentCommandExec
    :: ComponentExecContext component
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
  => ExecContext components
  -> Expr (Some (Elem components) (CommandProc components)) components
  -> m (Either (EvalError components) (Value components))
evaluate ctxs expr = runExceptT (eval ctxs expr)

eval
  :: ( AllConstrained (ComponentCommandExec m components) components
     , AllConstrained (ComponentLitToValue components) components
     , Monad m
     , Ord (Value components)
     )
  => ExecContext components
  -> Expr (Some (Elem components) (CommandProc components)) components
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
  => ExecContext components
  -> ProcCall (Some (Elem components) (CommandProc components)) (Value components)
  -> EvalT components m (Value components)
evalProcCall ctxs (ProcCall (Some commandProc) args) =
  componentEvalProcCall (rgetElem ctxs) (ProcCall commandProc args)

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
  => ComponentExecContext component
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
    commandExec (This Refl) = componentCommandExec ctx
    commandExec (That i) = commandExec i
