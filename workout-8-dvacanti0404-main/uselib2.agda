module uselib2 where

open import lib

-- see comment at top of uselib1.agda


{- hint: I needed *1, which takes an implicit argument; you would write *1{theArgument}
   instead of theArgument you put the actual implicit argument you need.  I also needed
   one other theorem from nat-thms.agda -}
square-square : ∀(x : ℕ) → square (square x) ≡ x pow 4
square-square x rewrite *1{x} | *assoc x x (x * x) = refl
