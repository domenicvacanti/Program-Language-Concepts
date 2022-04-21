module Extralib where
import Data.List

-- canonize a list of ordered elements
canonOrd :: Ord a => [a] -> [a] 
canonOrd = sort . nub

