module uselib3 where

open import lib

-- see comment at top of uselib1.agda

{- hint: _+_ is parsed left associatively, so if you see
   
   a + b + c

   please remember this is (a + b) + c.  This affects how you apply +assoc at the end
   to get the summands in exactly the same grouping. -}
poly : ∀(x y : ℕ) → square x + 2 * x * y + square y ≡ square (x + y)
poly x y rewrite +0 x | *distribr x x y | *distribr x y (x + y) | *distribl x x y | *distribl y x y | *comm y x | +assoc (x * x + x * y) (x * y) (y * y) | +assoc (x * x) (x * y) (x * y) = refl


