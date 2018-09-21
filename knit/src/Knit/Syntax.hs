module Knit.Syntax
       ( Operator(..)
       , CommandId(..)
       , Expr(..)
       , ComponentLit
       , Lit(..)
       , ProcCall(..)
       , Arg(..)
       , toLit
       , fromLit
       ) where

import Data.String

import Knit.Name
import Knit.Prelude

-- | An operator is an identifier for a command that does not map to a lexical
-- identifier. For instance, the and-then operator is binary, infix, and
-- lexically represented by a punctuation character (a semicolon). The unit
-- operator is nullary and is represented by empty space (possibly, but not
-- necessarily, enclosed in parentheses).
data Operator
  = OpAndThen
  | OpUnit
  deriving (Eq, Ord, Show)

-- | A command identifier, either an alphabetic name or an operator.
data CommandId
  = CommandIdName Name
  | CommandIdOperator Operator
  deriving (Eq, Ord, Show)

instance IsString CommandId where
  fromString = CommandIdName . fromString

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

toLit
  :: forall components component.
     Elem components component
  => ComponentLit component
  -> Lit components
toLit = Lit . ulift

fromLit
  :: forall components component.
     Elem components component
  => Lit components
  -> Maybe (ComponentLit component)
fromLit = umatch . getLitUnion

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
