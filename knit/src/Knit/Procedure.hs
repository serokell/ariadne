module Knit.Procedure where

import Data.Text
import Data.List.NonEmpty as NonEmpty
import Data.Validation as Validation
import Control.Lens
import Data.List as List

import Knit.Name
import Knit.Value
import Knit.Argument
import Knit.Syntax

data family ComponentCommandRepr (components :: [*]) component

data CommandProc components component = forall e. CommandProc
  { cpName :: Name
  , cpArgumentPrepare :: [Arg (Value components)] -> [Arg (Value components)]
  , cpArgumentConsumer :: ArgumentConsumer components e
  , cpRepr :: e -> ComponentCommandRepr components component
  , cpHelp :: Text
  }

resolveProcNames ::
    (x -> Name) ->
    [x] ->
    Expr Name components ->
    Either (NonEmpty Name) (Expr x components)
resolveProcNames nameOf xs =
    over _Left NonEmpty.nub . Validation.toEither . go
  where
    go ExprUnit                = pure ExprUnit
    go (ExprLit l)             = pure (ExprLit l)
    go (ExprGroup exprs)       = ExprGroup <$> traverse go exprs
    go (ExprProcCall procCall) = ExprProcCall <$> goProcCall procCall

    goProcCall (ProcCall procName args) =
        ProcCall
            <$> lookupProcName procName
            <*> (traverse.traverse) go args

    lookupProcName procName =
        Validation.fromEither $
        maybe (Left (procName :| [])) Right $
        List.find (\x -> nameOf x == procName) xs
