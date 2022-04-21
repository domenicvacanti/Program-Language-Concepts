{- search for 'undefined' to find the code you need to implement -}
module Exercises(module Exercises,module BinTree) where
import BinTree

----------------------------------------------------------------------
-- Functors

-- copy of Maybe type, for exercise below
data MyMaybe a = MyNothing | MyJust a
  deriving (Eq , Show)

{- see Section 8.5 (page 99) of the Hutton book for more on instance
   declarations. -}

instance Functor MyMaybe where
  fmap f (MyJust x) = MyJust (f x)
  fmap f MyNothing = MyNothing

{- imagine that we want to keep a list of the values stored
   in a tree, as well as the tree structure itself.  ValTree
   does this. -}
data ValTree a = ValTree [a] (BinTree a)
  deriving (Eq , Show)

instance Functor ValTree where
  fmap f (ValTree x y) = ValTree (fmap f x) (fmap f y)

-- trees with data at the leaves, and allowing an empty tree
data Tree a = Lf | Nd a [Tree a]
  deriving (Eq, Show)

t4 :: Tree Bool
t4 = Nd True [Nd False [], Nd False [Nd True []], Nd True []]
r4 = Nd False [Nd True [], Nd True [Nd False []], Nd False []]

instance Functor Tree where
  fmap f Lf = Lf
  fmap f (Nd x y) = Nd (f x) (fmap(fmap f) y)

{- When a1 is called with arguments d and x:

   Replace all the data in d under the functor
   with x.  So 'a1 [1,2,3] 5' should give you [5,5,5].
-}
a1 :: Functor f => f a -> a -> f a
a1 d x = fmap (const x) d

-- fmap a function through two functors
a2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
a2 = fmap fmap fmap

----------------------------------------------------------------------
-- Applicatives

{- given data with functions under application functor f,
   return new data composing those functions. -}
a3 :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
a3 x y = (.) <$> x <*> y

----------------------------------------------------------------------
-- Applicative example: logging

{- a value of type 'Logging a' is list of Strings together with an a.
   The list of Strings is the log.  The intention is to use it as a list
   of log messages, generated during execution of some function -}
data Logging a = Logging [String] a
  deriving (Show , Eq)

{-- lg s should return a new Logging value which has s as the sole string
    in the log, and the identity function ('id') as the value.  This is
    similar to what we did in class week4 with the 'add' function.

    Be careful not to call 'log' in your code below, because this is
    a different function, from the Prelude.

    Note that lg and fmap for Logging are covered by just one test
    in PublicTests.hs (so you need to solve both to get points for
    these).
-}
lg :: String -> Logging (a -> a)
lg s = Logging [s] id

instance Functor Logging where
  fmap f (Logging x y) = Logging x (f y)

-- when combining two Logging values, the logs should be concatenated
instance Applicative Logging where
  pure v = Logging [] v
  (Logging l1 v1) <*> (Logging l2 v2) = Logging (l1 ++ l2) (v1 v2)

{- The functions rle and rleh below implement run-length encoding.

   Run-length encoding is a basic data-compression algorithm that
   summarizes runs of length n of a repeated element x as just the
   pair (n,x).  Run rle on some example lists to see what it is doing.

   Your job is to implement the rlelh function below, to be similar
   to rleh, but in the Logging functor, with the following log messages:

   -- "base" for the base case
   -- "change" for when we enter the else branch in the second equation -}

rleh :: Eq a => Int -> a -> [a] -> [(Int,a)]
rleh c h [] = [(c,h)]
rleh c h' (h:t) =
  if h == h' then
    rleh (c+1) h t
  else
    (c,h') : rleh 1 h t

rle :: Eq a => [a] -> [(Int,a)]
rle [] = []
rle (h:t) = rleh 1 h t

{- copy the code above and then tweak it to use the Logging applicative.
   See the filterc, filtera, filterg examples from week4's Inclass.hs. -}
rlelh :: Eq a => Int -> a -> [a] -> Logging [(Int,a)]
rlelh c h [] = (lg "base") <*> pure [(c,h)]
rlelh c h' (h:t) =
  if h == h' then
    rlelh (c+1) h t 
  else
    ((c,h') :) <$> ((lg "change") <*> rlelh 1 h t)

rlel :: Eq a => [a] -> Logging [(Int,a)]
rlel [] = pure []
rlel (h:t) = rlelh 1 h t

----------------------------------------------------------------------
-- Applicative example: accumulating

{- This is like Logging, except that now we are accumulating a list
   of value of some type b (instead of a list of Strings).  Let us
   call this list the accumulation. -}
data Accum b a = Accum [b] a
  deriving (Show , Eq)

-- accum s l should return a new Accum a which has s at the front of the accumulation
accum :: b -> Accum b (a -> a)
accum s = Accum [s] id

instance Functor (Accum b) where
  fmap f (Accum l x) = Accum l (f x)

-- when combining two Accum values, the accumulations should be concatenated
instance Applicative (Accum b) where
  pure v = Accum [] v
  (Accum l1 v1) <*> (Accum l2 v2) = Accum (l1 ++ l2) (v1 v2)

{- Here is a solution for btSubtree, which you had in workout3.

   Your job is to write a new version in the Accum a applicative,
   where you accumulate the list of values stored at the nodes along
   the path.   -}
btSubtree :: [Bool] -> BinTree a -> Maybe (BinTree a)
btSubtree [] t = Just t
btSubtree _ Leaf = Nothing
btSubtree (b:bs) (Node x l r) = btSubtree bs $ if b then l else r

btSubtreea :: [Bool] -> BinTree a -> Accum a (BinTree a)
btSubtreea [] t = Accum [] t
btSubtreea (b:bs) Leaf = Accum [] Leaf
btSubtreea (b:bs) (Node x l r) = Accum [x] id <*> btSubtreea bs (if b then l else r)

