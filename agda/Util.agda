module Util where

open import Data.String
open import Data.Maybe

liftA2 : {A B C : Set} → (A → B → C) → Maybe A → Maybe B → Maybe C
liftA2 f (just x) (just y) = just (f x y)
liftA2 f nothing  _ = nothing
liftA2 f _ nothing  = nothing

Name : Set
Name = String

Filepath : Set
Filepath = String

Variable : Set
Variable = String

AtomicType : Set
AtomicType = String

data NonEmpty (A : Set) : Set where
  !_! : A → NonEmpty A
  _**_ : A → NonEmpty A → NonEmpty A

mapSeq : {A B : Set} → (A → B) → NonEmpty A → NonEmpty B
mapSeq f ! x ! = ! f x !
mapSeq f (x ** l) = f x ** mapSeq f l
