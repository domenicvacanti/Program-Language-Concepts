module Basic where

import Data.Ratio
import Data.List

hello :: String
hello = "Welcome to Haskell!"

----------------------------------------------------
-- Tuples
----------------------------------------------------

-- add the components of the input tuple
p1 :: (Int,Int,Int) -> Int
p1 (a,b,c) = a+b+c

-- swap the components of the input tuple
p2 :: (a,b) -> (b,a)
p2 (x,y) = (y,x)

-- add the two input tuples as vectors (so add first components, then add second components)
p3 :: (Int,Int) -> (Int,Int) -> (Int,Int)
p3 (a,b) (x,y) = (a+x,b+y)

-- combine two pairs into a tuple with four components
p4 :: (a,b) -> (c,d) -> (a,b,c,d)
p4 (a,b) (c,d) = (a,b,c,d)

-- break a flat tuple into two nested ones
p5 :: (a,b,c,d) -> ((a,b),(c,d))
p5 (a,b,c,d) = ((a,b),(c,d))

----------------------------------------------------
-- Strings and converting to strings
----------------------------------------------------

-- duplicate the input string (so return a string with two copies of s back to back)
p6 :: String -> String
p6 s = s++s

-- concatenate the input characters into a single string
p7 :: Char -> Char -> Char -> String
p7 a b c = [a]++[b]++[c]

-- Concatenate the strings with " is " in the middle.
-- So p8 "the dog" "lazy" should return "the dog is lazy"
p8 :: String -> String -> String
p8 s1 s2 = s1++"is"++s2

{- given x and y, print out the string with the string version of x, then " / ", and
   then the string version of y -}
p9 :: Integer -> Integer -> String
p9 x y = show x ++ "/" ++ show y

{- print the rational number as numerator / denominator

   HINT: figure out how to extract the numerator and denominator.
   Then you can just call p9.
   You can read the docs on Rational, here:
   http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Ratio.html
-}
p10 :: Rational -> String
p10 x = p9 a b
   where a = numerator x
         b = denominator x
----------------------------------------------------
-- Lists
--
-- Just call functions from Data.List to solve these.
-- You should not need to write any recursions yourself.
--
-- The docs for Data.List are here:
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html
----------------------------------------------------

-- return a new list which is the concatenation of the three inputs lists
p11 :: [a] -> [a] -> [a] -> [a]
p11 xs ys zs = xs++ys++zs

-- return a list which is just like the input list except without
-- the first and last elements (it can fail if the list has fewer
-- than 2 elements)
p12 :: [a] -> [a]
p12 l = tail (init l)

-- append xs with the reverse of xs (so p13 "abc" should return "abccba")
p13 :: [a] -> [a]
p13 xs = xs ++ reverse xs
