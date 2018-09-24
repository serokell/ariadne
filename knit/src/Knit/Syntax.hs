{-# LANGUAGE ConstraintKinds #-}

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
  = ExprProcCall (XExprProcCall ext cmd components) (ProcCall ext cmd (Expr ext cmd components))
  | ExprLit (XExprLit ext cmd components) (Lit components)
  | XExpr (XXExpr ext cmd components)

type family XExprProcCall (ext :: *) (cmd :: *) (components :: [*])
type family XExprLit (ext :: *) (cmd :: *) (components :: [*])
type family XXExpr (ext :: *) (cmd :: *) (components :: [*])

type ForallXExpr (constr :: * -> Constraint) (ext :: *) (cmd :: *) (components :: [*]) =
    ( constr (XExprProcCall ext cmd components)
    , constr (XExprLit ext cmd components)
    , constr (XXExpr ext cmd components)
    )

type instance XExprProcCall NoExt _ _ = NoExt
type instance XExprLit NoExt _ _ = NoExt
type instance XXExpr NoExt _ _ = Void

deriving instance
    ( Eq (Lit components)
    , Eq cmd
    , ForallXExpr Eq ext cmd components
    , ForallXProcCall Eq ext cmd (Expr ext cmd components)
    , ForallXArg Eq ext (Expr ext cmd components)
    ) => Eq (Expr ext cmd components)
deriving instance
    ( Ord (Lit components)
    , Ord cmd
    , ForallXExpr Ord ext cmd components
    , ForallXProcCall Ord ext cmd (Expr ext cmd components)
    , ForallXArg Ord ext (Expr ext cmd components)
    ) => Ord (Expr ext cmd components)
deriving instance
    ( Show (Lit components)
    , Show cmd
    , ForallXExpr Show ext cmd components
    , ForallXProcCall Show ext cmd (Expr ext cmd components)
    , ForallXArg Show ext (Expr ext cmd components)
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

data ProcCall ext cmd a = ProcCall (XProcCall ext cmd a) cmd [Arg ext a]

type ProcCall' ext cmd components = ProcCall ext cmd (Expr ext cmd components)

type family XProcCall ext cmd a

type ForallXProcCall (constr :: * -> Constraint) (ext :: *) (cmd :: *) (a :: *) =
    ( constr (XProcCall ext cmd a)
    )

type instance XProcCall NoExt _ _ = NoExt

deriving instance
    ( Eq cmd
    , Eq a
    , ForallXProcCall Eq ext cmd a
    , ForallXArg Eq ext a
    ) => Eq (ProcCall ext cmd a)
deriving instance
    ( Ord cmd
    , Ord a
    , ForallXProcCall Ord ext cmd a
    , ForallXArg Ord ext a
    ) => Ord (ProcCall ext cmd a)
deriving instance
    ( Show cmd
    , Show a
    , ForallXProcCall Show ext cmd a
    , ForallXArg Show ext a
    ) => Show (ProcCall ext cmd a)

instance Functor (ProcCall NoExt cmd) where
    fmap f (ProcCall NoExt cmd args) = ProcCall NoExt cmd $ map (fmap f) args

instance Foldable (ProcCall NoExt cmd) where
    foldMap f (ProcCall NoExt _ args) = foldMap (foldMap f) args

instance Traversable (ProcCall NoExt cmd) where
    sequenceA (ProcCall NoExt cmd args) = ProcCall NoExt cmd <$> traverse sequenceA args

data Arg ext a
    = ArgPos (XArgPos ext a) a
    | ArgKw (XArgKw ext a) Name a
    | XArg (XXArg ext a)

type Arg' ext cmd components = Arg ext (Expr ext cmd components)

type family XArgPos ext a
type family XArgKw ext a
type family XXArg ext a

type ForallXArg (constr :: * -> Constraint) (ext :: *) (a :: *) =
    ( constr (XArgPos ext a)
    , constr (XArgKw ext a)
    , constr (XXArg ext a)
    )

type instance XArgPos NoExt _ = NoExt
type instance XArgKw NoExt _ = NoExt
type instance XXArg NoExt _ = Void

deriving instance (Eq a, ForallXArg Eq ext a) => Eq (Arg ext a)
deriving instance (Ord a, ForallXArg Ord ext a) => Ord (Arg ext a)
deriving instance (Show a, ForallXArg Show ext a) => Show (Arg ext a)

instance Functor (Arg NoExt) where
    fmap f (ArgPos NoExt a) = ArgPos NoExt (f a)
    fmap f (ArgKw NoExt name a) = ArgKw NoExt name (f a)
    fmap _ (XArg xxArg) = absurd xxArg

instance Foldable (Arg NoExt) where
    foldMap f (ArgPos NoExt a) = f a
    foldMap f (ArgKw NoExt _ a) = f a
    foldMap _ (XArg xxArg) = absurd xxArg

instance Traversable (Arg NoExt) where
    sequenceA (ArgPos NoExt a) = ArgPos NoExt <$> a
    sequenceA (ArgKw NoExt name a) = ArgKw NoExt name <$> a
    sequenceA (XArg xxArg) = absurd xxArg
