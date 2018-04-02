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

data Type : Set where
  atom : AtomicType → Type
  ⋆ : Type
  Pi : Variable → Type → Type → Type
  Sig : Variable → Type → Type → Type
  ProcCall : Name → List (Name × Type) → Type → Type
  ⊔ : List Type → Type

postulate axiom : ∀ {A : Type} → (⊔ (A ∷ [])) ≡ A 
  
data Term : Set → Set where
  var : ∀ {S : Set} → Variable → S → Term S
  lit : ∀ {S : Set} → Lit → S → Term S
  procCall : ∀ {S : Set} → Name → (l : List (Term S ⊎ (Name × Term S))) → {f : l ≢ [ _ ]} → S → Term S 
  lam : ∀ {S : Set} → Variable → Term S → S → Term S
  app : ∀ {S : Set} → Term S → Term S → S → Term S
  pair : ∀ {S : Set} → Term S → Term S → S → Term S
  π₁ : ∀ {S : Set} → Term S → S → Term S
  π₂ : ∀ {S : Set} → Term S → S → Term S
  type : ∀ {S : Set} → Type → S → Term S

FV : ∀ {S : Set} → Term S → List Variable
FV (var x x₁) = [ x ]
FV (lit x x₁) = [ _ ]
FV (procCall x [] s) = [ _ ]
FV (procCall x (inj₁ x₁ ∷ l) s) = FV x₁ ++ FV (procCall x l s)
FV (procCall x (inj₂ y ∷ l) s) = FV (proj₂ y) ++ FV (procCall x l s)
FV (lam x t s) = FV t
FV (app t t₁ s) = FV t ++ FV t₁
FV (pair t t₁ s) = FV t ++ FV t₁
FV (π₁ t s) = FV t
FV (π₂ t s) = FV t
FV (type (atom x) s) = [ _ ]
FV (type ⋆ s) = [ _ ]
FV (type (Pi x x₁ x₂) s) = FV (type x₂ s)
FV (type (Sig x x₁ x₂) s) = FV (type x₂ s)
FV (type (ProcCall n [] t) s) = FV (type t s)
FV (type (ProcCall n ((proj₃ , proj₄) ∷ l) t) s) = FV (type proj₄ s) ++ FV (type (ProcCall n l t) s)
FV (type (⊔ []) s) = [ _ ]
FV (type (⊔ (x ∷ x₁)) s) = FV (type x s) ++ FV (type (⊔ x₁) s)

typeSubst' : ∀ {S : Set} → Type → Variable × Term S → Type
typeSubst' = {!!}

substitution : ∀ {S : Set} → Term S → Variable × Term S → Term S  
substitution (var x x₁) p = {!!}
substitution (lit x x₁) p = {!!}
substitution (procCall x x₁ x₂) p = {!!}
substitution (lam x t s) p = {!!}
substitution (app t t₁ s) p = {!!}
substitution (pair t t₁ s) p = {!!}
substitution (π₁ t s) p = {!!}
substitution (π₂ t s) p = {!!}
substitution (type x s) p = {!!}












  

  
                                    
                                    
