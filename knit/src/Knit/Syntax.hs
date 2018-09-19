{-# LANGUAGE ConstraintKinds, PolyKinds #-}

module Knit.Syntax where

import Data.String
import GHC.Exts (Constraint)

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

data NoExt = NoExt
    deriving (Eq, Ord, Show)

data Expr ext cmd components
  = ExprProcCall (XExprProcCall ext) (ProcCall ext cmd (Expr ext cmd components))
  | ExprLit (XExprLit ext) (Lit components)

type family XExprProcCall ext
type family XExprLit ext

type ForallXExpr (constr :: * -> Constraint) (ext :: *) =
    ( constr (XExprProcCall ext)
    , constr (XExprLit ext)
    )

type instance XExprProcCall NoExt = NoExt
type instance XExprLit NoExt = NoExt

deriving instance
    ( Eq (Lit components)
    , Eq cmd
    , ForallXExpr Eq ext
    , ForallXProcCall Eq ext
    , ForallXArg Eq ext
    ) => Eq (Expr ext cmd components)
deriving instance
    ( Ord (Lit components)
    , Ord cmd
    , ForallXExpr Ord ext
    , ForallXProcCall Ord ext
    , ForallXArg Ord ext
    ) => Ord (Expr ext cmd components)
deriving instance
    ( Show (Lit components)
    , Show cmd
    , ForallXExpr Show ext
    , ForallXProcCall Show ext
    , ForallXArg Show ext
    ) => Show (Expr ext cmd components)

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

data ProcCall ext cmd a = ProcCall (XProcCall ext) cmd [Arg ext a]
  deriving (Functor, Foldable, Traversable)

type family XProcCall ext

type ForallXProcCall (constr :: * -> Constraint) (ext :: *) = (constr (XProcCall ext))

type instance XProcCall NoExt = NoExt

deriving instance
    ( Eq cmd
    , Eq a
    , ForallXProcCall Eq ext
    , ForallXArg Eq ext
    ) => Eq (ProcCall ext cmd a)
deriving instance
    ( Ord cmd
    , Ord a
    , ForallXProcCall Ord ext
    , ForallXArg Ord ext
    ) => Ord (ProcCall ext cmd a)
deriving instance
    ( Show cmd
    , Show a
    , ForallXProcCall Show ext
    , ForallXArg Show ext
    ) => Show (ProcCall ext cmd a)

data Arg ext a = ArgPos (XArgPos ext) a | ArgKw (XArgKw ext) Name a
  deriving (Functor, Foldable, Traversable)

type family XArgPos ext
type family XArgKw ext

type ForallXArg (constr :: * -> Constraint) (ext :: *) =
    ( constr (XArgPos ext)
    , constr (XArgKw ext)
    )

type instance XArgPos NoExt = NoExt
type instance XArgKw NoExt = NoExt

deriving instance (Eq a, ForallXArg Eq ext) => Eq (Arg ext a)
deriving instance (Ord a, ForallXArg Ord ext) => Ord (Arg ext a)
deriving instance (Show a, ForallXArg Show ext) => Show (Arg ext a)
