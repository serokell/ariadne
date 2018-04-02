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
  procCall : ∀ {S : Set} → Name → (l : List (Term S ⊎ (Name × Term S))) → S → Term S 
  lam : ∀ {S : Set} → Variable → Term S → S → Term S
  app : ∀ {S : Set} → Term S → Term S → S → Term S
  pair : ∀ {S : Set} → Term S → Term S → S → Term S
  π₁ : ∀ {S : Set} → Term S → S → Term S
  π₂ : ∀ {S : Set} → Term S → S → Term S
  type : ∀ {S : Set} → Type → S → Term S

FV : ∀ {S : Set} → Term S → List Variable
FV (var x x₁) = [ x ]
FV (lit x x₁) = []
FV (procCall x [] s) = []
FV (procCall x (inj₁ x₁ ∷ l) s) = FV x₁ ++ FV (procCall x l s)
FV (procCall x (inj₂ y ∷ l) s) = FV (proj₂ y) ++ FV (procCall x l s)
FV (lam x t s) = FV t
FV (app t t₁ s) = FV t ++ FV t₁
FV (pair t t₁ s) = FV t ++ FV t₁
FV (π₁ t s) = FV t
FV (π₂ t s) = FV t
FV (type (atom x) s) = [ x ]
FV (type ⋆ s) = []
FV (type (Pi x x₁ x₂) s) = FV (type x₂ s)
FV (type (Sig x x₁ x₂) s) = FV (type x₂ s)
FV (type (ProcCall n [] t) s) = FV (type t s)
FV (type (ProcCall n ((proj₃ , proj₄) ∷ l) t) s) = FV (type proj₄ s) ++ FV (type (ProcCall n l t) s)
FV (type (⊔ []) s) = []
FV (type (⊔ (x ∷ x₁)) s) = FV (type x s) ++ FV (type (⊔ x₁) s)

mutual
  typeSubst' : ∀ {S : Set} → Type → Variable × Term S → Type
  typeSubst' (atom x) (proj₃ , type x₁ x₂) = x₁
  typeSubst' (atom x) (proj₃ , _) = atom x
  typeSubst' ⋆ p = ⋆
  typeSubst' (Pi x t t₁) p = Pi x t (typeSubst' t₁ p)
  typeSubst' (Sig x t t₁) p = Sig x t (typeSubst' t₁ p)
  typeSubst' (ProcCall x l t) p = ProcCall x (typeSubst l p) t
  typeSubst' (⊔ l) p = ⊔ (typeSubst'' l p)
    where
    typeSubst'' : ∀ {S : Set} → List Type → Variable × Term S → List Type
    typeSubst'' [] t = []
    typeSubst'' (x ∷ l) p = typeSubst' x p ∷ typeSubst'' l p

  typeSubst : ∀ {S : Set} → List (Name × Type) → Variable × Term S → List (Name × Type)
  typeSubst [] p = []
  typeSubst ((proj₃ , proj₄) ∷ l) p = (proj₃ , typeSubst' proj₄ p) ∷ typeSubst l p

substitution : ∀ {S : Set} → Term S → Variable × Term S → Term S  
substitution (var x x₁) (proj₃ , proj₄) = if x == proj₃ then proj₄ else (var x x₁)
substitution (lit x x₁) p = lit x x₁
substitution (procCall x l x₂) p = procCall x (stuff l p) x₂
  where
  stuff : ∀ {S : Set} → List (Term S ⊎ (Name × Term S)) → Variable × Term S → List (Term S ⊎ (Name × Term S))
  stuff [] p = []
  stuff (inj₁ x ∷ l) p = inj₁ (substitution x p) ∷ stuff l p
  stuff (inj₂ (proj₃ , proj₄) ∷ l) p = inj₂ (proj₃ , substitution proj₄ p) ∷ stuff l p
substitution (lam x t s) p = lam x (substitution t p) s
substitution (app t t₁ s) p = app (substitution t p) (substitution t₁ p) s
substitution (pair t t₁ s) p = pair (substitution t p) (substitution t₁ p) s
substitution (π₁ t s) p = π₁ (substitution t p) s
substitution (π₂ t s) p = π₂ (substitution t p) s
substitution (type t s) p = type (typeSubst' t p) s
