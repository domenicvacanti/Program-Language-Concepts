module ntree where

open import lib

-- simple Tree type storing natural numbers
data Tree : Set where
  Node : ℕ → Tree → Tree → Tree
  Leaf : Tree

-- a few functions on trees, used in problems in some of the ind*agda files

mirror : Tree → Tree
mirror (Node x t1 t2) = Node x (mirror t2) (mirror t1)
mirror Leaf = Leaf

perfect : ℕ → Tree
perfect zero = Leaf
perfect (suc n) = Node 1 (perfect n) (perfect n)

numLeaves : Tree → ℕ
numLeaves (Node x t t₁) = numLeaves t + numLeaves t₁
numLeaves Leaf = 1

size : Tree → ℕ
size (Node x t t₁) = 1 + size t + size t₁
size Leaf = 1
