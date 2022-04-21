module Reader where --(Reader,get,runReader,TableReader,tableLookup,tableUpdate) where

import Core
import Data.List
import AssocList

----------------------------------------------------------------------
-- Reader functor
--
-- This is like Stateful except that a Stateful contains a function
-- of type
--
--     s -> (s,x)
--
-- where here we just have
--
--     s -> x

data Reader s x = Reader (s -> x) 

fmapR :: (a -> b) -> Reader s a -> Reader s b
fmapR f (Reader h) =
  Reader (f . h)

pureR :: a -> Reader s a
pureR v = Reader (\ _ -> v)

joinR :: Reader s (Reader s a) -> Reader s a
joinR (Reader h) =
  Reader (\ s ->
           let (Reader h') = h s in
             h' s)

instance Core (Reader s) where
  fmapC = fmapR
  pureC = pureR
  joinC = joinR

instance Functor (Reader s) where
  fmap = fmapR

instance Applicative (Reader s) where
  pure = pureR
  (<*>) = appC

instance Monad (Reader s) where
  (>>=) = bindC

get :: Reader s s
get = Reader id

put :: s -> Reader s a -> Reader s a
put s (Reader h) = Reader (\ _ -> h s)

-- inspired by discussion here:
-- https://stackoverflow.com/questions/40131562/how-to-interpret-bind-of-the-function-instance/40136614#40136614
foldfun :: (a -> Reader s b) -> Reader s (a -> b)
foldfun f = Reader (\ r a -> let Reader g = f a in g r)

runReader :: s -> Reader s x -> x
runReader s (Reader h) = h s

----------------------------------------------------------------------
type TableReader a b c = Reader (AssocList a b) c

{- given a key, look it up in the current table (in the TableReader monad) -}
tableLookup :: Eq a => a -> TableReader a b (Maybe b)
tableLookup key =
  lookup key <$> get

tableUpdate :: Eq a => a -> b -> TableReader a b c -> TableReader a b c
tableUpdate key value (Reader h) =
  Reader
    (\ table ->
       h (update key value table))

----------------------------------------------------------------------
-- a Reader where the state being read is a list.
--
-- a couple convenience functions are provided for accessing the list.
type ListReader a b = Reader [a] b

-- return (within the ListReader monad) whether or not the given element is in the list
lrElem :: Ord a => a -> ListReader a Bool
lrElem x = do
  l <- get
  return $ elem x l

-- add the given element to the list that is part of the ListReader
lrAdd :: a -> ListReader a b -> ListReader a b 
lrAdd x r = do
  l <- get
  put (x:l) r