module bools3 where

open import lib

nand-and : ∀ (b1 b2 : 𝔹) → b1 && b2 ≡ (b1 nand b2) nand (b1 nand b2)
nand-and tt tt = refl
nand-and tt ff = refl
nand-and ff tt = refl
nand-and ff ff = refl

