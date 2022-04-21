module ind5 where

open import lib
open import ntree

-- I needed +0, +suc
perfect-size : ∀(n : ℕ) → suc (size (perfect n)) ≡ 2 pow (suc n)
perfect-size zero = refl
perfect-size (suc n) rewrite sym (perfect-size n) | +0 (size(perfect n)) | sym (+suc(size(perfect n))(size(perfect n)))= refl
