module eq2 where

open import lib

postulate
  A : Set
  a : A
  b : A
  c : A
  d : A    
  f : A → A
  g : A → A 
  h : A → A → A
  p : a ≡ b
  q : b ≡ c
  r : f a ≡ a
  s : h a a ≡ a
  t : ∀ x → f (g x) ≡ g (f x)
  u : ∀ x → f x ≡ x → g (g x) ≡ x
  v : ∀ x → h x d ≡ x

L2 : c ≡ a
L2 rewrite p | q = refl
