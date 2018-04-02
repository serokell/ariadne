module TypeCheck where

open import Data.Product 
open import Data.List
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; setoid; sym; trans; subst; _≢_)
open import Data.String renaming (_++_ to _+++_)
open import Data.Float
open import Data.Nat
open import Data.Fin
open import Data.Sum 
open import Data.List.Any.Membership
open import Data.Bool
open import Function
open import Data.Maybe

Name : Set
Name = String

Filepath : Set
Filepath = String

Variable : Set
Variable = String

AtomicType : Set
AtomicType = String

data Lit : Set where
  string : String → Lit
  number : Float → Lit
  filepath : Filepath → Lit

data Seq (A : Set) : Set where
  !_! : A → Seq A
  _**_ : A → Seq A → Seq A

data Type : Set where
  atom : AtomicType → Type
  ⋆ : Type
  Pi : Variable → Type → Type → Type
  Sig : Variable → Type → Type → Type
  ProcCall : Name → Seq (Name × Type) → Type → Type
  ⊔ : Seq Type → Type 

data Term : Set → Set where
  var : ∀ {S : Set} → Variable → S → Term S
  lit : ∀ {S : Set} → Lit → S → Term S
  procCall : ∀ {S : Set} → Name → (l : Seq (Term S ⊎ (Name × Term S))) → S → Term S 
  lam : ∀ {S : Set} → Variable → Term S → S → Term S
  app : ∀ {S : Set} → Term S → Term S → S → Term S
  pair : ∀ {S : Set} → Term S → Term S → S → Term S
  π₁ : ∀ {S : Set} → Term S → S → Term S
  π₂ : ∀ {S : Set} → Term S → S → Term S
  type : ∀ {S : Set} → Type → S → Term S

FV : ∀ {S : Set} → Term S → List Variable
FV (var x x₁) = [ x ]
FV (lit x x₁) = []
FV (procCall x ! inj₁ x₁ ! s) = FV x₁
FV (procCall x ! inj₂ (proj₃ , proj₄) ! s) = FV proj₄
FV (procCall x (inj₁ x₁ ** l) s) = (FV x₁) ++ FV (procCall x l s)
FV (procCall x (inj₂ y ** l) s) = FV (proj₂ y) ++ FV (procCall x l s)
FV (lam x t s) = FV t
FV (app t t₁ s) = FV t ++ FV t₁
FV (pair t t₁ s) = FV t ++ FV t₁
FV (π₁ t s) = FV t
FV (π₂ t s) = FV t
FV (type (atom x) s) = [ x ]
FV (type ⋆ s) = [ _ ]
FV (type (Pi x x₁ x₂) s) = FV (type x₂ s)
FV (type (Sig x x₁ x₂) s) = FV (type x₂ s)
FV (type (ProcCall x ! x₁ ! t) s) = (FV (type (proj₂ x₁) s)) ++ FV (type t s)
FV (type (ProcCall x (x₁ ** l) t) s) = FV (type (proj₂ x₁) s) ++ FV (type (ProcCall x l t) s)
FV (type (⊔ ! x !) s) = FV (type x s)
FV (type (⊔ (x ** x₁)) s) = (FV (type x s)) ++ (FV (type (⊔ x₁) s))

typeSubst : ∀ {S} → Type → Variable × Term S → Type
typeSubst (atom x) p = atom x
typeSubst ⋆ p = ⋆
typeSubst (Pi x t t₁) p = if x == proj₁ p then Pi x t t₁ else Pi x t (typeSubst t₁ p) 
typeSubst (Sig x t t₁) p = if x == proj₁ p then Sig x t t₁ else Sig x t (typeSubst t₁ p)
typeSubst (ProcCall x seq t) p = ProcCall x (seqTySubst seq p) t
  where
  seqTySubst : ∀ {S} → Seq (Name × Type) → Variable × Term S → Seq (Name × Type)
  seqTySubst ! x₂ ! p = ! (proj₁ x₂ , typeSubst (proj₂ x₂) p) !
  seqTySubst (x₂ ** seq) p = ((proj₁ x₂ , typeSubst (proj₂ x₂) p)) ** seqTySubst seq p
typeSubst (⊔ seq) p = ⊔ (seqTySubst' seq p)
  where
  seqTySubst' : ∀ {S} → Seq Type → Variable × Term S → Seq Type
  seqTySubst' ! x ! p = ! typeSubst x p !
  seqTySubst' (x ** seq) p = typeSubst x p ** seqTySubst' seq p

substitution : ∀ {S : Set} → Term S → Variable × Term S → Term S  
substitution (var x x₁) (proj₃ , proj₄) = if x == proj₃ then proj₄ else (var x x₁)
substitution (lit x x₁) p = lit x x₁
substitution (procCall x l s) p = procCall x (seqSubst l p) s
  where
  seqSubst : ∀ {S} → Seq (Term S ⊎ (Name × Term S)) → Variable × Term S → Seq (Term S ⊎ (Name × Term S))
  seqSubst ! inj₁ x ! p = ! inj₁ (substitution x p) !
  seqSubst ! inj₂ (n , t) ! p = ! inj₂ (n , (substitution t p)) !
  seqSubst (inj₁ x ** l) p = (inj₁ (substitution x p)) ** seqSubst l p
  seqSubst (inj₂ y ** l) p = (inj₂ ((proj₁ y) , (substitution (proj₂ y) p))) ** seqSubst l p 
substitution (lam x t s) p = if (x == (proj₁ p)) then (lam x t s) else lam x (substitution t p) s
substitution (app t t₁ s) p = app (substitution t p) (substitution t₁ p) s
substitution (pair t t₁ s) p = pair (substitution t p) (substitution t₁ p) s
substitution (π₁ t s) p = π₁ (substitution t p) s
substitution (π₂ t s) p = π₂ (substitution t p) s
substitution (type t s) p = type (typeSubst t p) s
