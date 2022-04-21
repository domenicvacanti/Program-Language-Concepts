module bools1 where

open import lib

----------------------------------------------------------------------
-- these first problems are about the nand operator, also known as the
-- Scheffer stroke.
--
-- You can solve all of them just by writing equations covering
-- all the possibilities for the boolean inputs.  The booleans
-- in the IAL are tt for true and ff for false.  Remember that
-- refl proves x ≡ x for any x you want.  Also, Agda automatically
-- simplifies expressions using the definitions of functions, so
-- refl will prove equations like 2 + 2 ≡ 4, because 2 + 2 simplifies
-- to 4.
----------------------------------------------------------------------
nand-not : ∀ (b : 𝔹) → ~ b ≡ b nand b
nand-not tt = refl
nand-not ff = refl

