module Knit.Procedure where

import Data.Text
import Data.List.NonEmpty as NonEmpty
import Data.Validation as Validation
import Control.Lens
import Data.List as List
import Data.Vinyl.TypeLevel
import Data.Vinyl.Core
import Data.Proxy

import Knit.Name
import Knit.Value
import Knit.Argument
import Knit.Syntax
import Knit.Utils

data family ComponentCommandRepr (components :: [*]) component

data CommandProc components component = forall e. CommandProc
  { cpName :: Name
  , cpArgumentPrepare :: [Arg (Value components)] -> [Arg (Value components)]
  , cpArgumentConsumer :: ArgumentConsumer components e
  , cpRepr :: e -> ComponentCommandRepr components component
  , cpHelp :: Text
  }

class Elem components component => ComponentCommandProcs components component where
  componentCommandProcs :: [CommandProc components component]

commandProcs :: forall components.
     (AllConstrained (ComponentCommandProcs components) components, KnownSpine components)
  => [Some (Elem components) (CommandProc components)]
commandProcs = go (knownSpine @components)
  where
    go :: forall components'.
      (AllConstrained (ComponentCommandProcs components) components')
      => Spine components' -> [Some (Elem components) (CommandProc components)]
    go RNil = []
    go ((Proxy :: Proxy component) :& xs) = List.map Some (componentCommandProcs @_ @component) ++ go xs

resolveProcNames ::
    (x -> Name) ->
    [x] ->
    Expr Name components ->
    Either (NonEmpty Name) (Expr x components)
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
