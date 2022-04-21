module Exercises where

import Data.Ratio
import Data.Maybe
import Data.List
import Data.Bool (bool)

----------------------------------------------------------------------
-- higher-order functions with some basic datatypes
----------------------------------------------------------------------

-- p1 f x should call f three times (in nested fashion) on x
p1 :: (a -> a) -> a -> a
p1 f x = f (f (f x))

-- p2 f x should call f with x and x as the two inputs to f
p2 :: (a -> a -> c) -> a -> c
p2 f x = f x x

-- apply a function to each component of a tuple, building a new tuple
p3 :: (a -> b) -> (a,a) -> (b,b)
p3 f (x,y) = (f x, f y)

{- for p3 and p4, there is only one way to return a value of the
   desired output type.  (So the type completely specifies what
   the code must do.) -}
p4 :: (a -> b -> c) -> (a -> b) -> a -> c
p4 f g x = f x y
   where y = g x

p5 :: (b,d) -> (b -> c) -> ((c,d) -> e) -> e
p5 (x,y) f g = g t
   where t = (f x, y)

{- change the order of the types in the input tuple of a function.   -}
p6 :: ((a,b) -> c) -> (b,a) -> c
p6 f (x,y) = f (y,x)

----------------------------------------------------------------------
-- higher-order functions on lists
--
-- You should just use combinators to write these functions.  Recursion
-- is not allowed.  You may use list comprehensions (which we did not
-- cover yet) if you want, but the problems can be solved without them.
--
-- Also you can see the docs for Data.List:
--       https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html
----------------------------------------------------------------------
-- with the help of the function "even", return those elements of the given input list that are even
p7 :: [Integer] -> [Integer]
p7 = filter even

-- return the number of zero values in the input list (you can use the section (== 0) as a predicate to test for 0)
p8 :: [Integer] -> Int
p8 xs = length (filter (==0) xs)

-- using the function all, return True iff all the elements in the list are nonzero.
p9 :: [Integer] -> Bool
p9 = all (/=0)

{- given f , p , and xs, return the list of those elements x of xs
   for which f x satisfies the given predicate p. -}
p10 :: (a -> b) -> (b -> Bool) -> [a] -> [a]
p10 f p xs = filter (\ x -> p (f x)) xs

{- given a function and a list of inputs, construct the list of input-output pairs
   for that function on those inputs.  Hint: this is a good place to use an anonymous
   function, to go from a value of type a to a value of type (a,b). -}
p11 :: (a -> b) -> [a] -> [(a,b)]
p11 f xs = map (\x -> (x, f x)) xs

-- Multiply all the numbers in the input list
p12 :: [Integer] -> Integer
p12 = product

{- given an element x and a non-empty list of lists l, return a new list of lists
   which is just like l except that it has an additional element at the
   head, namely x:h, where h is the head of l.

   So p13 x (y:ys) should equal (x:y):y:ys (in fact, that pretty much shows you
   the code you should write). -}
p13 :: a -> [[a]] -> [[a]]
p13 x [] = []
p13 x (y:ys) = (x:y):y:ys

{- Using foldr and p13, construct the list of all sublists of the given
   input list.  So for [1,2,3], you would construct [[1,2,3],[2,3],[3],[]] -}
p14 :: [a] -> [[a]]
p14 xs = foldr p13 [[]] xs

{- Add corresponding numbers in the two lists, dropping any excess.

   So p15 [1,2,3] [10,20,30,40] should return [11,22,33]

   Hint: use zipWith -}
p15 :: Num a => [a] -> [a] -> [a]
p15 = zipWith (+)

{- compose corresponding functions from the two lists,
   creating a list of the resulting compositions.  If one
   list is longer, drop the excess.

   Hint: again, use zipWith -}
p16 :: [b -> c] -> [a -> b] -> [a -> c]
p16 = zipWith (.)

----------------------------------------------------------------------
