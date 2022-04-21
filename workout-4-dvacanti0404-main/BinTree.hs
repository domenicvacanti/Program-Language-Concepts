{- polymorphic binary trees with data at the leaves,
   and a few example trees -}
module BinTree where

data BinTree a = Leaf | Node a (BinTree a) (BinTree a)
  deriving (Eq,Ord)

-- singleton: make a new BinTree holding just one piece of data
sing :: a -> BinTree a
sing x = Node x Leaf Leaf

-- an example
e = Node 1 (sing 2) (Node 3 (sing 3) (sing 4))
e1 = Node 1 (Node 3 (sing 4) (sing 3)) (sing 2)
e2 = Node 10 (Node 30 (sing 40) (sing 30)) (sing 20)
e3 = Node 10 (sing 30) (sing 20)
e4 = Node 1 (sing 4) (Node 9 (sing 9) (sing 16))
e5 = Node 1 (sing 2) (Node 3 e3 (sing 4))

showTree :: Show a => BinTree a -> String
showTree Leaf = "*"
showTree (Node x l r) = "[" ++ show x ++ " " ++ showTree l ++ " " ++ showTree r ++ "]"

instance Show a => Show (BinTree a) where
  show = showTree

instance Functor BinTree where
  fmap f Leaf = Leaf
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Applicative BinTree where
  pure x = Node x (pure x) (pure x)
  Leaf <*> t = Leaf
  t <*> Leaf = Leaf
  (Node f l r) <*> (Node x l' r') = Node (f x) (l <*> l') (r <*> r')
