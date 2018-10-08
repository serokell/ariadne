module Knit.Procedure
       ( ComponentCommandRepr
       , CommandProc(..)
       , ComponentCommandProcs(..)
       , SomeCommandProc(..)
       , commandProcs
       , resolveProcNames
       ) where

import Control.Lens
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.Proxy
import Data.Text
import Data.Validation as Validation

import Knit.Argument
import Knit.Prelude
import Knit.Syntax
import Knit.Value

data family ComponentCommandRepr (components :: [*]) component

data CommandProc components component = forall e. CommandProc
  { cpName :: CommandId
  , cpArgumentPrepare :: [Arg (Value components)] -> [Arg (Value components)]
  , cpArgumentConsumer :: ArgumentConsumer components e
  , cpRepr :: e -> ComponentCommandRepr components component
  , cpHelp :: Text
  }

class Elem components component => ComponentCommandProcs components component where
  componentCommandProcs :: [CommandProc components component]

data SomeCommandProc components where
  SomeCommandProc
    :: Elem components component
    => CommandProc components component
    -> SomeCommandProc components

commandProcs :: forall components.
     (AllConstrained (ComponentCommandProcs components) components, KnownSpine components)
  => [SomeCommandProc components]
commandProcs = go (knownSpine @components)
  where
    go
      :: forall components'.
         (AllConstrained (ComponentCommandProcs components) components')
      => Spine components'
      -> [SomeCommandProc components]
    go (Base ()) = []
    go (Step (Proxy :: Proxy component, xs)) =
      List.map SomeCommandProc (componentCommandProcs @_ @component) ++ go xs

resolveProcNames
  :: Eq name
  => (x -> name)
  -> [x]
  -> Expr name components
  -> Either (NonEmpty name) (Expr x components)
resolveProcNames nameOf xs =
    over _Left NonEmpty.nub . Validation.toEither . go
  where
    go = \case
      ExprLit l -> pure (ExprLit l)
      ExprProcCall procCall -> ExprProcCall <$> goProcCall procCall

    goProcCall (ProcCall procName args) =
      ProcCall
        <$> lookupProcName procName
        <*> (traverse.traverse) go args

    lookupProcName procName =
      Validation.fromEither $
      maybe (Left (procName :| [])) Right $
      List.find (\x -> nameOf x == procName) xs
