module AssocList where

type AssocList a b = [(a,b)]

{- associate key with value in association list (just change first binding found) -}
update :: Eq a => a -> b -> AssocList a b -> AssocList a b
update key value ((key',value') : al) | key == key' = (key,value) : al
                                      | otherwise = (key',value') : update key value al
update key value [] = [(key,value)]

