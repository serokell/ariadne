module TypeCheck where

open import Data.Product 
open import Data.List
open import Data.String renaming (_++_ to _+++_)
open import Data.Float
open import Data.Nat
open import Data.Fin
open import Data.Sum 
open import Data.List.Any.Membership
open import Data.Bool
open import Function

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
  str : Type
  num : Type
  fp : Type
  U : Type
  Pi : Variable → Type → Type → Type
  Sig : Variable → Type → Type → Type
  ProcCall : Name → Type → Type
  
data Term : Set where
  var : Variable → Term
  lit : Lit → Term
  procCall : Name → List (Term ⊎ (Name × Term)) → Term 
  lam : Variable → Term → Term
  app : Term → Term → Term
  pair : Term → Term → Term
  π₁ : Term → Term
  π₂ : Term → Term
  type : Type → Term

Bind : Set
Bind = Variable × Type

Declaration : Set 
Declaration = Term × Type

BindToDec : Bind → Declaration
BindToDec (x , y) = (var x) , y

Ctx : Set
Ctx = List Bind

FV : Term → List Variable
FV (var x) = [ x ]
FV (lit x) = [ _ ]
FV (procCall x []) = [ _ ]
FV (procCall x (inj₁ x₁ ∷ x₂)) = FV x₁ ++ FV (procCall x x₂)
FV (procCall x (inj₂ (proj₁ , proj₂) ∷ x₃)) = FV proj₂ ++ FV (procCall x x₃)
FV (lam x t) = FV t
FV (app t t₁) = FV t ++ FV t₁
FV (pair t t₁) = FV t ++ FV t₁
FV (π₁ t) = FV t
FV (π₂ t) = FV t
FV (type (atom x)) =  [ x ]
FV (type str) = [ _ ]
FV (type num) = [ _ ]
FV (type fp) = [ _ ]
FV (type U) = [ _ ]
FV (type (Pi x t t₁)) = FV (type t) ++ FV (type t₁)
FV (type (Sig x t t₁)) = FV (type t) ++ FV (type t₁)
FV (type (ProcCall x t)) = FV (type t)

typeSubst' : Type → Variable × Term → Type
typeSubst' (atom x₁) p = if x₁ == proj₁ p then atom x₁ else atom (proj₁ p)
typeSubst' str p = str  
typeSubst' num p = num 
typeSubst' fp p = fp
typeSubst' U p = U
typeSubst' (Pi x₁ t t₁) p = Pi x₁ t (typeSubst' t₁ p)
typeSubst' (Sig x₁ t t₁) p = Sig x₁ t (typeSubst' t₁ p)
typeSubst' (ProcCall x t) p = ProcCall x (typeSubst' t p)


typeSubst : Type → Variable × Term → Term
typeSubst t p = type (typeSubst' t p)

subst : Term → Variable × Term → Term  
subst (var x) (proj₃ , proj₄) = if x == proj₃ then proj₄ else var x
subst (lit x) p = lit x
subst (procCall x l) p = procCall x (stuff l p)
  where
  stuff : List (Term ⊎ (Name × Term)) → Variable × Term → List (Term ⊎ (Name × Term))
  stuff [] p = []
  stuff (inj₁ x ∷ l) p = inj₁ (subst x p) ∷ stuff l p
  stuff (inj₂ (proj₃ , proj₄) ∷ l) p = inj₂ (proj₃ , subst proj₄ p) ∷ stuff l p
subst (lam x t) p = subst t p
subst (app t t₁) p = app (subst t p) (subst t₁ p)
subst (pair t t₁) p = pair (subst t p) (subst t₁ p)
subst (π₁ t) p = π₁ (subst t p)
subst (π₂ t) p = π₁ (subst t p)
subst (type x) p = typeSubst x p 

data _⊢_ : Ctx → Declaration → Set where
  axiom : {Γ : Ctx}{p : Bind} → (p ∷ Γ) ⊢ (BindToDec p)
  weakening : {Γ : Ctx}{p : Bind}{d : Declaration} → (Γ ⊢ d) → ((p ∷ Γ) ⊢ d)
  lit₁ : {Γ : Ctx}{s : String} → Γ ⊢ (lit (string s) , str)
  lit₂ : {Γ : Ctx}{n : Float} → Γ ⊢ (lit (number n) , num)
  lit₃ : {Γ : Ctx}{fpath : Filepath} → Γ ⊢ (lit (filepath fpath) , fp)
  Piᵢ : {Γ : Ctx}{p : Bind}{B : Type} →
                               (p ∷ Γ) ⊢ (type B , U) →
                               Γ ⊢ (type (Pi (proj₁ p) (proj₂ p) B) , U)
  PiᵢT : {Γ : Ctx}{p : Bind}{d : Declaration} →
                               (p ∷ Γ) ⊢ d →
                               Γ ⊢ (lam (proj₁ p) (proj₁ d) , Pi (proj₁ p) (proj₂ p) (proj₂ d))
  Piₑ : {Γ : Ctx}{M N : Term}{x : Variable}{A B : Type} → Γ ⊢ (M , Pi x A B) → Γ ⊢ (N , A) → Γ ⊢ (app M N , typeSubst' B (x , N))
  Σᵢ : {Γ : Ctx}{p : Bind}{B : Type} → (p ∷ Γ) ⊢ (type B , U) → Γ ⊢ (type (Sig (proj₁ p) (proj₂ p) B), U) 
  ΣᵢT : {Γ : Ctx}{p : Bind}{B : Type}{M N : Term}{B : Type} →
                                (p ∷ Γ) ⊢ (type B , U) →
                                Γ ⊢ (M , proj₂ p) →
                                Γ ⊢ (N , typeSubst' B (proj₁ p , M)) →
                                Γ ⊢ (pair M N , Sig (proj₁ p) (proj₂ p) B)
  Σₑ1 : {Γ : Ctx}{M : Term}{x : Variable}{A B : Type} →
                                Γ ⊢ (M , Sig x A B) →
                                Γ ⊢ (π₁ M , A)
  Σₑ2 : {Γ : Ctx}{M : Term}{x : Variable}{A B : Type} →
                                Γ ⊢ (M , Sig x A B) →
                                Γ ⊢ (π₂ M , typeSubst' B (x , π₁ M))
  ProcCallT : {Γ : Ctx}{L : List (Term ⊎ (Name × Term))}{M : Term}{N : Name}{A : Type} →
                                Γ ⊢ (procCall N L , ProcCall N A)

data _⇒_ : Term → Term → Set where
  ΠSubst : {x : Variable}{M : Term}{N : Term}
                                     → app (lam x M) N ⇒ subst M (x , N)
  ΠRed₁ : {M N P : Term} → M ⇒ N → app M P ⇒ app N P
  ΠRed₂ : {M N P : Term} → N ⇒ P → app M N ⇒ app M P
  ΣRed₁ : {M N P : Term} → M ⇒ N → pair M P ⇒ pair N P
  ΣRed₂ : {M N P : Term} → N ⇒ P → pair M N ⇒ pair M P
  πRed₁ : {M N : Term} → M ⇒ N → π₁ M ⇒ π₁ N
  πRed₂ : {M N : Term} → M ⇒ N → π₂ M ⇒ π₂ N
  ProcCallRed₁ : {M : Name}{N P : Term}{L : List (Term ⊎ (Name × Term))} →
                                     N ⇒ P →
                                     procCall M (inj₁ N ∷ L) ⇒ procCall M (inj₁ P ∷ L)
  ProcCallRed₂ : {M₁ M₂ : Name}{N P : Term}{L : List (Term ⊎ (Name × Term))} →
                                     N ⇒ P →
                                     procCall M₁ ((inj₂ (M₂ , N)) ∷ L) ⇒ procCall M₁ ((inj₂ (M₂ , P)) ∷ L)

subjectReduction : {Γ : Ctx}{p : Declaration}{N : Term} → Γ ⊢ p → proj₁ p ⇒ N → Γ ⊢ (N , proj₂ p)
subjectReduction a p = {!!}
