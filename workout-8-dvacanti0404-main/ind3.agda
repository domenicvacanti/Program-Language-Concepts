module ind3 where

open import lib
open import ntree

-- see comment at top of ind1.agda

mirror-mirror : ∀ (t : Tree) → mirror (mirror t) ≡ t
mirror-mirror Leaf = refl
mirror-mirror (Node x y z) rewrite mirror-mirror z | mirror-mirror y = refl
