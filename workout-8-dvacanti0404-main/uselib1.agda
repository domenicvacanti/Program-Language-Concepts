module uselib1 where

open import lib

{----------------------------------------------------------------------
 For the theorems in this file and the other uselib* files, you should
 NOT prove them by induction.  We will not penalize you if you do; it
 will just be harder -- maybe much harder -- than necessary.  Instead, use
 theorems from the IAL. 

 Reminder: you can look up the definition of a symbol by typing Alt-.
 with your cursor over the definition (so hold the Alt key and then type
 a dot).

 Hint: theorems in nat-thms.agda will be helpful for these.
----------------------------------------------------------------------}

-- hint: use iszeromult: it takes in two proofs, but those are easy to write in this case
iszero-square-suc : ∀(x : ℕ) → iszero (square (suc x)) ≡ ff
iszero-square-suc x = refl
