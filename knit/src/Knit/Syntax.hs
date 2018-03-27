module Knit.Syntax where

import Data.Union
import Data.String

import Knit.Name

data Operator
  = OpSemicolon
  | OpUnit
  deriving (Eq, Ord, Show)

data CommandName
  = ProcedureName Name
  | OperatorName Operator
  deriving (Eq, Ord, Show)

instance IsString CommandName where
  fromString = ProcedureName . fromString

data Expr cmd components
  = ExprProcCall (ProcCall cmd (Expr cmd components))
  | ExprLit (Lit components)

deriving instance (Eq (Lit components), Eq cmd) => Eq (Expr cmd components)
deriving instance (Ord (Lit components), Ord cmd) => Ord (Expr cmd components)
deriving instance (Show (Lit components), Show cmd) => Show (Expr cmd components)

data family ComponentLit component

newtype Lit components =
  Lit { getLitUnion :: Union ComponentLit components }

deriving instance Eq (Union ComponentLit components) => Eq (Lit components)
deriving instance Ord (Union ComponentLit components) => Ord (Lit components)
deriving instance Show (Union ComponentLit components) => Show (Lit components)

data ProcCall cmd a = ProcCall cmd [Arg a]
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq cmd, Eq a) => Eq (ProcCall cmd a)
deriving instance (Ord cmd, Ord a) => Ord (ProcCall cmd a)
deriving instance (Show cmd, Show a) => Show (ProcCall cmd a)

data Arg a = ArgPos a | ArgKw Name a
  deriving (Functor, Foldable, Traversable)

deriving instance Eq a => Eq (Arg a)
deriving instance Ord a => Ord (Arg a)
deriving instance Show a => Show (Arg a)
