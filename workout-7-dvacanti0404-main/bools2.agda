module bools2 where

open import lib

nand-or : ∀ (b1 b2 : 𝔹) → b1 || b2 ≡ (b1 nand b1) nand (b2 nand b2)
nand-or tt tt  = refl
nand-or tt ff = refl
nand-or ff tt = refl
nand-or ff ff = refl

