module bools4 where

open import lib

----------------------------------------------------------------------
-- some additional problems
----------------------------------------------------------------------

&&-distrib : ∀ x y z → x && (y || z) ≡ (x && y) || (x && z)
&&-distrib tt y z = refl
&&-distrib ff y z = refl
