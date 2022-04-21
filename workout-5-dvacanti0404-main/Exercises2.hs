module Exercises2 where
import Extralib
import Reader
import Term

{- return the list of free variables in the given
   Expr.  The input list of variables is a list of
   variables that this call to freeVars should consider
   bound.  So if you encounter a variable in that list,
   it is not free at that point.  You can add a variable
   to that list to mark it as bound in a recursive call.

   The returned list may contain duplicates.  These are removed
   below.
-}
freeVarsh :: [Var] -> Term -> [Var]
freeVarsh x (App y z) = freeVarsh x y ++ freeVarsh x z
freeVarsh x (Var a) = [a | a `notElem` x]
freeVarsh x (Lam a b) = freeVarsh (x++[a]) b

freeVars :: Term -> [Var]
freeVars t = canonOrd (freeVarsh [] t)

-- rewrite freeVarsh using the ListReader monad (Reader.hs)
freeVarsh' :: Term -> ListReader Var [Var]
freeVarsh' (Var x) = fmap (\bool -> [x | not bool]) (lrElem x)
freeVarsh' (App y z) =(++)  <$> freeVarsh' y <*> freeVarsh' z
freeVarsh' (Lam x y) = lrAdd x (freeVarsh' y)

-- use freeVarsh' and canonOrd to get a list of free variables without duplicates
freeVars' :: Term -> [Var]
freeVars' x = canonOrd (runReader [] (freeVarsh' x))

{- given variables x, y, and z, check to see if z is either x or y.  If it is
   return the other one (so if z is x, return y; if z is y, return x).  If
   z is neither x nor y, return z. -}
swapVar :: Var -> Var -> Var -> Var
swapVar x y z
  | z == x = y
  | z == y = x
  | otherwise = z

-- using swapVar, change x into y and vice versa everywhere in the Term,
-- including at binding occurrences (first argument to Lam). 
swapNames :: Var -> Var -> Term -> Term
swapNames x y (App a b) = App (swapNames x y a) (swapNames x y b)
swapNames x y (Var z) = Var (swapVar x y z)
swapNames x y (Lam z c) = Lam (swapVar x y z) (swapNames x y c)

{- findNewName vs v

   Given list of variables vs (presumed to be finite),
   find a new version of v that is not in vs, by adding
   single quotation marks ' to the end of v.  If v itself
   is not in vs already, then just return it (no ' marks added) -}
findNewName :: [Var] -> Var -> Var
findNewName [] y = y
findNewName (x:xs) y = if take 1 x == take 1 y then findNewName xs (y++"'") else findNewName xs y

{- using findNewName and swapNames, safely rename all bound
   variables in the given Term to new names that are
   not in the given list of variables.

   The given list of variables may be assumed to
   contain all free variables of the given Term, and you
   should maintain that property when you recurse, so that
   new names you pick will not conflict with names that are
   bound deeper in the term. -}
renameAwayFrom :: [Var] -> Term -> Term
renameAwayFrom xs (App a b) = App (renameAwayFrom xs a) (renameAwayFrom xs b)
renameAwayFrom _ (Var x) = Var x
renameAwayFrom xs (Lam x y) = let z = findNewName xs x in
   Lam z (renameAwayFrom (xs++[z]) (swapNames z x y))