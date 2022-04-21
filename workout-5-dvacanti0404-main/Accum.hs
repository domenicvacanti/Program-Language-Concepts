{- applicative for accumulating things in a list, as a side effect -}
module Accum where

data Accum a b = Accum [a] b
  deriving (Show,Eq)

instance Functor (Accum a) where
  fmap f (Accum x y) = Accum x (f y)

instance Applicative (Accum a) where
  pure x = Accum [] x
  (Accum x f) <*> (Accum x' a) = Accum (x ++ x') (f a)

instance Monad (Accum a) where
  (Accum x a) >>= f = let (Accum x' a') = f a in
                        Accum (x ++ x') a'

add :: x -> Accum x (y -> y)
add x = Accum [x] id
