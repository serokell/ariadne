module TypeCheck where

open import Data.Product hiding (map)
open import Data.List
open import Data.String renaming (_++_ to _+++_ ; _≟_ to _===_)
open import Relation.Nullary using (yes ; no)
open import Agda.Builtin.Equality
open import Data.Float
open import Data.Nat 
open import Data.Sum hiding (map)
open import Function
open import Data.Bool
open import Data.Maybe hiding (map)
open import Data.Unit
open import Util

data Lit : Set where
  string : String → Lit
  number : Float → Lit
  filepath : Filepath → Lit

data Type : Set where
  atom : AtomicType → Type
  ⋆ : Type
  Pi : Variable → Type → Type → Type
  ProcCall : Name → NonEmpty (Name × Type) → Type → Type
  ⊔ : NonEmpty Type → Type

data Equal : Type → Type → Set where
  oui : {τ : Type} → Equal τ τ
  non : {τ σ : Type} → Equal τ σ


_=?=_ : (τ σ : Type) → Equal τ σ
atom x =?= atom x₁ =
  case x === x₁ of
  λ { (yes refl) → oui
     ; (no _) → non
     }
atom x =?= ⋆ = non
atom x =?= Pi x₁ σ σ₁ = non
atom x =?= ProcCall x₁ x₂ σ = non
atom x =?= ⊔ x₁ = non
⋆ =?= atom x = non
⋆ =?= ⋆ = oui
⋆ =?= Pi x σ σ₁ = non
⋆ =?= ProcCall x x₁ σ = non
⋆ =?= ⊔ x = non
Pi x τ τ₁ =?= atom x₁ = non
Pi x τ τ₁ =?= ⋆ = non
Pi x τ τ₁ =?= Pi x₁ σ σ₁ =
 case x === x₁ of
 λ { (yes refl) → case (τ =?= σ) , (τ₁ =?= σ₁) of
                  λ { (oui , oui) → oui
                     ; (non , oui) → non
                     ; (oui , non) → non
                     ; (non , non) → non
                     }
    ; (no _) → non
    }
Pi x τ τ₁ =?= ProcCall x₁ x₂ σ = non
Pi x τ τ₁ =?= ⊔ x₁ = non
ProcCall x x₁ τ =?= atom x₂ = non
ProcCall x x₁ τ =?= ⋆ = non
ProcCall x x₁ τ =?= Pi x₂ σ σ₁ = non
ProcCall x ! (n , p) ! τ =?= ProcCall x₂ ! (n₁ , p₁) ! σ  with τ =?= σ
ProcCall x ! (n , p) ! τ =?= ProcCall x₂ ! (n₁ , p₁) ! σ | oui = case x === x₂ of
                                                     λ { (yes refl) → case n === n₁ of
                                                                       λ { (yes refl) → case p =?= p₁ of
                                                                                        λ { oui → oui
                                                                                          ; non → non
                                                                                          }
                                                                          ; (no _) → non
                                                                          }
                                                        ; (no _) → non
                                                        }                                                                
ProcCall x ! (n , p) ! τ =?= ProcCall x₂ ! (n₁ , p₁) ! σ | non = non
ProcCall x ! x₁ ! τ =?= ProcCall x₂ (x₃ ** l₂) σ = non
ProcCall x (x₁ ** l₁) τ =?= ProcCall x₂ ! x₃ ! σ = non
ProcCall x (x₁ ** l₁) τ =?= ProcCall x₂ (x₃ ** l₂) σ with ProcCall x l₁ τ =?= ProcCall x₂ l₂ σ
ProcCall x ((n , t) ** l₁) τ =?= ProcCall x₂ ((n₁ , t₁) ** l₂) σ | oui = case n === n₁ of
                                                                          λ { (yes refl) → case t =?= t₁ of
                                                                                           λ { oui → oui
                                                                                              ; non → non
                                                                                              }
                                                                             ; (no _) → non
                                                                             }
ProcCall x (x₁ ** l₁) τ =?= ProcCall x₂ (x₃ ** l₂) σ | non = non
ProcCall x x₁ τ =?= ⊔ x₂ = non
⊔ x =?= atom x₁ = non
⊔ x =?= ⋆ = non
⊔ x =?= Pi x₁ σ σ₁ = non
⊔ x =?= ProcCall x₁ x₂ σ = non
⊔ ! x ! =?= ⊔ ! x₁ ! with x =?= x₁
⊔ ! x ! =?= ⊔ ! x₁ ! | oui = oui
⊔ ! x ! =?= ⊔ ! x₁ ! | non = non
⊔ (x ** x₁) =?= ⊔ ! x₂ ! = non
⊔ ! x ! =?= ⊔ (x₁ ** x₂) = non
⊔ (x ** x₃) =?= ⊔ (x₁ ** x₂) with ⊔ x₃ =?= ⊔ x₂
⊔ (x ** x₃) =?= ⊔ (x₁ ** x₂) | oui = case x =?= x₁ of
                                    λ { oui → oui
                                       ; non → non
                                       }
⊔ (x ** x₃) =?= ⊔ (x₁ ** x₂) | non = non

data Term : Set → Set where
  var : ∀ {S : Set} → Variable → S → Term S
  lit : ∀ {S : Set} → Lit → S → Term S
  name : ∀ {S : Set} → Name → S → Term S
  procCall : ∀ {S : Set} → Name → NonEmpty (Term S ⊎ (Name × Term S)) → S → Term S
  lam : ∀ {S : Set} → Variable → Term S → S → Term S
  app : ∀ {S : Set} → Term S → Term S → S → Term S
  type : ∀ {S : Set} → Type → S → Term S

FV : ∀ {S : Set} → Term S → List Variable
FV (var x x₁) = [ x ]
FV (lit x x₁) = []
FV (name x x₁) = []
FV (procCall x ! inj₁ x₁ ! s) = FV x₁
FV (procCall x ! inj₂ y ! s) = FV (proj₂ y)
FV (procCall x (inj₁ x₁ ** l) s) = FV x₁ ++ FV (procCall x l s)
FV (procCall x (inj₂ y ** l) s) = FV (proj₂ y) ++ FV (procCall x l s)
FV (lam x t s) = FV t
FV (app t t₁ s) = FV t ++ FV t₁
FV (type (atom x) s) = [ x ]
FV (type ⋆ s) = [ _ ]
FV (type (Pi x x₁ x₂) s) = FV (type x₂ s)
FV (type (ProcCall x ! x₁ ! t) s) = (FV (type (proj₂ x₁) s)) ++ FV (type t s)
FV (type (ProcCall x (x₁ ** l) t) s) = FV (type (proj₂ x₁) s) ++ FV (type (ProcCall x l t) s)
FV (type (⊔ ! x !) s) = FV (type x s)
FV (type (⊔ (x ** l)) s) = FV (type x s) ++ FV (type (⊔ l) s)


typeSubst : ∀ {S} → Type → Variable × Term S → Type
typeSubst (atom x) p = atom x
typeSubst ⋆ p = ⋆
typeSubst (Pi x t t₁) p = if x == proj₁ p then Pi x t t₁ else Pi x t (typeSubst t₁ p)
typeSubst (ProcCall x l t) p = ProcCall x (NameTypeSubst l p) (typeSubst t p)
  where
  NameTypeSubst : ∀ {S} → NonEmpty (Name × Type) → Variable × Term S → NonEmpty (Name × Type)
  NameTypeSubst ! n , t ! p = ! n , typeSubst t p !
  NameTypeSubst (x₂ ** l) p = (proj₁ x₂ , typeSubst (proj₂ x₂) p) ** NameTypeSubst l p
typeSubst (⊔ l) p = ⊔ (SeqSubst l p)
  where
  SeqSubst : ∀ {S} → NonEmpty Type → Variable × Term S → NonEmpty Type
  SeqSubst ! x ! p = ! typeSubst x p !
  SeqSubst (x ** seq) p = typeSubst x p ** SeqSubst seq p

substitution : ∀ {S : Set} → Term S → Variable × Term S → Term S
substitution (var x x₁) (proj₃ , proj₄) = if x == proj₃ then proj₄ else (var x x₁)
substitution (lit x x₁) p = lit x x₁
substitution (name x x₁) p = name x x₁
substitution (procCall x l s) p = procCall x (seqSubst l p) s
  where
  seqSubst : ∀ {S} → NonEmpty (Term S ⊎ (Name × Term S)) → Variable × Term S → NonEmpty (Term S ⊎ (Name × Term S))
  seqSubst ! inj₁ x ! p = ! inj₁ (substitution x p) !
  seqSubst ! inj₂ (n , t) ! p = ! inj₂ (n , (substitution t p)) !
  seqSubst (inj₁ x ** l) p = (inj₁ (substitution x p)) ** seqSubst l p
  seqSubst (inj₂ y ** l) p = (inj₂ ((proj₁ y) , (substitution (proj₂ y) p))) ** seqSubst l p 
substitution (lam x t s) p = if (x == (proj₁ p)) then (lam x t s) else lam x (substitution t p) s
substitution (app t t₁ s) p = app (substitution t p) (substitution t₁ p) s
substitution (type t s) p = type (typeSubst t p) s

subst : ∀ {S : Set} → Term S → Variable × Term S → Term S
subst t p = case any (_==_ (proj₁ p)) (FV t) of
  λ { false → t
     ; true → substitution t p
     }

termToS : ∀ {S : Set} → Term S → S
termToS (var x x₁) = x₁
termToS (lit x x₁) = x₁
termToS (name x x₁) = x₁
termToS (procCall x l x₁) = x₁
termToS (lam x t x₁) = x₁
termToS (app t t₁ x) = x
termToS (type x x₁) = x₁

typeCheck : (Variable → Type) → (Lit → Type) → (Name → Type) → Term ⊤ → Maybe (Term Type)
typeCheck h f g (var x tt) = just (var x (h x))
typeCheck h f g (lit x tt) = just (lit x (f x))
typeCheck h f g (name x tt) = just (name x (g x))
typeCheck h f g (lam x t x₁) = case (typeCheck h f g t) of
  λ { nothing → nothing
     ; (just t') → just (lam x t' (Pi x (h x) (termToS t')))
     }
typeCheck h f g (app t t₁ tt) =
  case (typeCheck h f g t) , (typeCheck h f g t₁) of
  λ { (just fun , just arg) → case termToS fun , termToS arg of
                              λ { (Pi x τ₁ τ₂ , τ) → case τ₁ =?= τ of
                                                     (λ { oui → (just (app fun arg τ))
                                                        ; non → nothing
                                                        })
                                 ; _ → nothing
                                 }
     ; _ → nothing
     }
typeCheck h f g (procCall x l tt) =
  case typeCheckSeq l of
  λ { (just l') → just (procCall x l' (ProcCall x (seqNameType l') ((⊔ (mapSeq proj₂ (seqNameType l'))))))
     ; nothing → nothing
     }
  where
  sumCheck : Term ⊤ ⊎ (Name × Term ⊤) → Maybe (Term Type ⊎ (Name × Term Type))
  sumCheck (inj₁ x) =
    case (typeCheck h f g x) of
    λ { nothing → nothing
       ; (just t') → just (inj₁ t')
       }
  sumCheck (inj₂ y) =
    case typeCheck h f g (proj₂ y) of
    λ { nothing → nothing
       ; (just t') → just (inj₂ (proj₁ y , t'))
       }
  typeCheckSeq : NonEmpty (Term ⊤ ⊎ (Name × Term ⊤)) → Maybe (NonEmpty (Term Type ⊎ (Name × Term Type)))
  typeCheckSeq ! x ! =
    case sumCheck x of
    λ { nothing → nothing
       ; (just t') → just ! t' !
       }
  typeCheckSeq (x₁ ** l) = liftA2 _**_ (sumCheck x₁) (typeCheckSeq l)
  seqNameType : NonEmpty (Term Type ⊎ (Name × Term Type)) → NonEmpty (Name × Type)
  seqNameType ! inj₁ x ! = ! (" ", termToS x)  !
  seqNameType ! inj₂ y ! = ! proj₁ y , termToS (proj₂ y)  !
  seqNameType (inj₁ x ** l) = (" ", termToS x) ** seqNameType l
  seqNameType (inj₂ y ** l) = (" ", termToS (proj₂ y)) ** seqNameType l
typeCheck h f g (type x tt) = just (type x ⋆)
