module Exercises1 where
import Extralib
import Expr
import Accum

{- given an expression e, extract all the variable definitions from it.

   In more detail: return a myList of all pairs (x,e) where there is a Let-expression
   in e defining x to be e.  Replace all those Let-expressions with their bodies.
   The myList should be in the order Lets are encountered as you recurse down into
   the term, recursing first into in the left subexpression of an Add or Mult,
   and then into the right subexpression.

   Note that you do need to perform this replacement on e1 in an expression
   of the form Let x e1 e2.  This is a tiny bit tricky, so there is a second
   test for it in PublicTests ("extractLets 2").

   I have this set up using the Accum monad, which works very elegantly here.
-}
extractLets :: Expr -> Accum (Var,Expr) Expr
extractLets (Let x y z) = do
        x1 <- extractLets y
        add (x, x1) <*> extractLets z
extractLets (Int a) = Accum [] (Int a)
extractLets (Use b) = Accum [] (Use b)
extractLets (Add y z) = Add <$> extractLets y <*> extractLets z
extractLets (Mult y z) = Mult <$> extractLets y <*> extractLets z

{- given a myList of pairs (v,e1) representing defining variable v to be e1,
   wrap those pairs around the given Expr e using Let-expressions. See
   the "wrapLets" test in PublicTests for an example. -}
wrapLets :: [(Var,Expr)] -> Expr -> Expr
wrapLets ((x,y):xs) a = Let x y (wrapLets xs a)
wrapLets [] a = a

{- using extractLets and wrapLets, lift all Lets out to the top of an Expr.
   See the "liftLets" test in PublicTests for an example. -}
liftLets :: Expr -> Expr
liftLets x =
   let Accum myList y = extractLets x
   in wrapLets myList y

{- collect all the variables that appear anywhere in the given expression.
   The returned myList may contain duplicates.  These are removed
   below. -}
collectVarsh :: Expr -> [Var]
collectVarsh (Int _) = []
collectVarsh (Use x) = [x]
collectVarsh (Let x y z) = [x] ++ collectVarsh y ++ collectVarsh z
collectVarsh (Add y z) = collectVarsh y ++ collectVarsh z
collectVarsh (Mult y z) = collectVarsh y ++ collectVarsh z

{- collect all the variables that appear anywhere in the given expression,
   with no duplicates -}
collectVars :: Expr -> [Var]
collectVars = canonOrd . collectVarsh
