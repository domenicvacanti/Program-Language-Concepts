module Core where

----------------------------------------------------------------------
-- The three central ideas of a Monad:
class Core f where

  -- call a function in f-land
  fmapC :: (a -> b) -> f a -> f b

  -- move a value into f-land
  pureC :: a -> f a

  -- flatten nested f's (sequence effects; flatten nested data structures)
  joinC :: f (f a) -> f a

-- the application for an Applicative
appC :: Core f => f (a -> b) -> f a -> f b
appC h x = joinC $ fmapC (\ g -> fmapC g x) h

-- the bind for a Monad
bindC :: Core f => f a -> (a -> f b) -> f b
bindC x f = joinC (appC (pureC f) x)
