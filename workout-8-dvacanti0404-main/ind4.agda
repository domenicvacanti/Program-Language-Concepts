module ind4 where

open import lib
open import ntree

-- I found I needed the +0 theorem from nat-thms.agda 
perfect-numLeaves : ∀(n : ℕ) → numLeaves (perfect n) ≡ 2 pow n
perfect-numLeaves zero = refl
perfect-numLeaves (suc n) rewrite +0 (2 pow n) | perfect-numLeaves n = refl

