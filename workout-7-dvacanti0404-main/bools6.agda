module bools6 where

open import lib

ite-not : ∀(A : Set)(x : 𝔹)(y : A)(z : A) → if x then y else z ≡ if ~ x then z else y
ite-not A tt y z = refl
ite-not A ff y z = refl
