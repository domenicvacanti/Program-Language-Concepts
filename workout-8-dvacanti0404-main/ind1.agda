module ind1 where

open import lib

----------------------------------------------------------------------
-- For the theorems in the files ind*agda, I expect you will need to
-- use proof by induction, possibly also using theorems in the IAL.
----------------------------------------------------------------------

thm1 : ∀(n : ℕ) → list-any iszero (repeat n 1) ≡ ff
thm1 zero = refl
thm1 (suc n) rewrite thm1 n = refl
