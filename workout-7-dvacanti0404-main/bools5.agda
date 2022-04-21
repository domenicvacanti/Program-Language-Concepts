module bools5 where

open import lib

combK : ∀ x y → x imp (y imp x) ≡ tt
combK  tt tt = refl
combK tt ff = refl
combK ff tt = refl
combK ff ff = refl

ite-not : ∀(A : Set)(x : 𝔹)(y : A)(z : A) → if x then y else z ≡ if ~ x then z else y
ite-not A tt y z = refl
ite-not A ff y z = refl

