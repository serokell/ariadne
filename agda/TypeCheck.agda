module TypeCheck where

open import Data.Product 
open import Data.List 
open import Data.String renaming (_++_ to _+++_ ; _≟_ to _===_)
open import Relation.Nullary using (yes ; no)
open import Agda.Builtin.Equality
open import Data.Float
open import Data.Nat
open import Data.Sum
open import Function
open import Data.Bool
open import Data.Maybe
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
  Sig : Variable → Type → Type → Type
  ProcCall : Name → Seq (Name × Type) → Type → Type
  ⊔ : Seq Type → Type

data Equal? : Type → Type → Set where
  oui : ∀ {τ : Type} → Equal? τ τ
  non : ∀ {τ σ : Type} → Equal? τ σ

_=?=_ : (τ σ : Type) → Equal? τ σ
atom x =?= atom x₁ = case (x === x₁) of
  (λ { (yes refl) → oui
     ; (no _) → non }) 
atom x =?= _ = non
⋆ =?= ⋆ = oui
⋆ =?= _ = non
Pi x τ τ₁ =?= Pi x₁ σ σ₁ =
  case (τ =?= σ , τ₁ =?= σ₁) of
  (λ { (oui , oui) → case x === x₁ of
                     (  (λ { (yes refl) → oui
                        ;    (no _) → non
                        })) 
     ; _ → non
     })
Pi x τ τ₁ =?= _ = non
Sig x τ τ₁ =?= Sig x₁ σ σ₁ =
  case (τ =?= σ , τ₁ =?= σ₁) of
  λ { (oui , oui) → case x === x₁ of
                    λ { (yes refl) → oui
                       ; (no _) → non
                       }
     ;  _ → non
     }
Sig x τ τ₁ =?= _ = non
ProcCall x ! proj₃ , proj₄ ! τ =?= ProcCall x₂ ! proj₅ , proj₆ ! σ =
  case x === x₂ of
  λ { (yes refl) → case τ =?= σ of
                λ { oui → case proj₃ === proj₅ of
                          λ { (yes refl) → case proj₄ =?= proj₆ of
                                        (λ { oui → oui
                                           ; non → non }) 
                             ; (no _) → non
                             }
                   ; non → non
                   }
    ; (no _) → non
    }
ProcCall x ! x₁ ! τ =?= ProcCall x₂ (x₃ ** x₄) σ = non
ProcCall x (x₁ ** x₄) τ =?= ProcCall x₂ ! x₃ ! σ = non
ProcCall x (x₁ ** x₄) τ =?= ProcCall x₂ (x₃ ** x₅) σ =
  case (ProcCall x ! x₁ ! τ) =?= (ProcCall x₂ ! x₃ ! σ) of
    λ { oui → case (ProcCall x x₄ τ) =?= (ProcCall x x₅ σ) of
                   λ { oui → oui
                     ; non → non
                     }
      ; non → non
      }
ProcCall x x₁ τ =?= _ = non
⊔ ! x ! =?= ⊔ ! x₁ ! =
  case x =?= x₁ of
    λ { oui → oui
      ; non → non
      }
⊔ ! x ! =?= ⊔ (x₁ ** y) = non
⊔ (x ** x₁) =?= ⊔ ! x₂ ! = non
⊔ (x ** seq) =?= ⊔ (x₁ ** seq₁) =
  case (x =?= x₁ , ⊔ seq =?= ⊔ seq₁) of
  (λ { (oui , oui) → oui
     ; _ → non
     }) 
⊔ x =?= _ = non

data Term : Set → Set where
  var : ∀ {S : Set} → Variable → S → Term S
  lit : ∀ {S : Set} → Lit → S → Term S
  name : ∀ {S : Set} → Name → S → Term S
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
FV (name x x₁) = []
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
substitution (name x x₁) p = name x x₁
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

subst : ∀ {S : Set} → Term S → Variable × Term S → Term S
subst t p = case any (_==_ (proj₁ p)) (FV t) of
  λ { false → t
     ; true → substitution t p
     }


litToType : Lit → Type
litToType (string x) = atom "string"
litToType (number x) = ⊔ (atom "double" **
                         (atom "int" **
                         (atom "word32" **
                         (atom "integer" **
                         ! atom "float" !))))
litToType (filepath x) = atom "filepath"

termToS : ∀ {S : Set} → Term S → S
termToS (var x x₁) = x₁
termToS (lit x x₁) = x₁
termToS (name x x₁) = x₁
termToS (procCall x l x₁) = x₁
termToS (lam x t x₁) = x₁
termToS (app t t₁ x) = x
termToS (pair t t₁ x) = x
termToS (π₁ t x) = x
termToS (π₂ t x) = x
termToS (type x x₁) = x₁

typeCheck : (Variable → Type) → (Lit → Type) → (Name → Type) → Term ⊤ → Maybe (Term Type)
typeCheck h f g (var x tt) = just (var x (h x))
typeCheck h f g (lit x tt) = just (lit x (f x))
typeCheck h f g (name x tt) = just (name x (g x))
typeCheck h f g (lam x t x₁) = case (typeCheck h f g t) of
  λ { nothing → nothing 
     ; (just t') → just (lam x t' (Pi x (h x) (termToS t'))) 
     }
typeCheck h f g (app t t₁ x) = case (typeCheck h f g t , typeCheck h f g t₁) of
  λ { (just t' , just t'') → case termToS t' of
            (λ { (Pi x τ₁ τ₂) → case (τ₁ =?= termToS t'') of
                     λ {  oui → just t''
                        ; non → nothing 
                         }
            ; _ → nothing
            })
     ; (_ , _) → nothing
     }
typeCheck h f g (pair t t₁ x) =
  case (typeCheck h f g t , typeCheck h f g t₁) of
     λ { (just t' , just t'') → just (pair t' t'' (Sig "x" (termToS t') (termToS t'')))
         ; (_ , _) → nothing
        }
typeCheck h f g (π₁ t x) =
  case typeCheck h f g t of
     (λ { (just (pair p₁ p₂ (Sig v τ₁ τ₂))) → just p₁
        ; _ → nothing })
typeCheck h f g (π₂ t x) =
  case typeCheck h f g t of
     (λ { (just (pair p₁ p₂ (Sig v τ₁ τ₂))) → just p₂
        ; _ → nothing })
typeCheck h f g (procCall x seq tt) =
  case (typeCheckSeq seq) of
    (λ { nothing → nothing
       ; (just s) → just (procCall x s (ProcCall x (seqNameType s) (⊔ (mapSeq proj₂ (seqNameType s)))))
       })
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
  typeCheckSeq : Seq (Term ⊤ ⊎ (Name × Term ⊤)) → Maybe (Seq (Term Type ⊎ (Name × Term Type)))
  typeCheckSeq ! x ! =
    case sumCheck x of
    λ { nothing → nothing
       ; (just t') → just ! t' !
       }
  typeCheckSeq (x₁ ** l) = liftA2 _**_ (sumCheck x₁) (typeCheckSeq l)
  seqNameType : Seq (Term Type ⊎ (Name × Term Type)) → Seq (Name × Type)
  seqNameType ! inj₁ x ! = ! (" ", termToS x)  !
  seqNameType ! inj₂ y ! = ! proj₁ y , termToS (proj₂ y)  !
  seqNameType (inj₁ x ** l) = (" ", termToS x) ** seqNameType l
  seqNameType (inj₂ y ** l) = (" ", termToS (proj₂ y)) ** seqNameType l
typeCheck h f g (type x tt) = just (type x ⋆)
