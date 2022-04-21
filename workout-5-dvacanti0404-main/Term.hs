{- Terms of lambda calculus -}
module Term where

type Var = String

data Term = Var Var | App Term Term | Lam Var Term
  deriving Eq

showTerm :: Term -> String
showTerm (Var v) = v
showTerm (App t1 t2) = "(" ++ showTerm t1 ++ " " ++ showTerm t2 ++ ")"
showTerm (Lam v t) = "(λ " ++ v ++ " . " ++ showTerm t ++ ")"

instance Show Term where
  show = showTerm

-- assumes non-empty input list
app :: [Term] -> Term
app (t:ts) = foldl App t ts

lams :: [Var] -> Term -> Term
lams vs body = foldr Lam body vs

proj1 = lams ["x" , "y"] (Var "x")

proj2 = lams ["x" , "y"] (Var "y")

two = lams [ "s" , "z"] (App (Var "s") (App (Var "s") (Var "z")))

num :: Int -> Term
num n = lams [ "s", "z"] $ foldr (App . Var) (Var "z") $ take n $ repeat "s"

-- \ x -> x x does not type-check in Haskell
dup = Lam "x" (App (Var "x") (Var "x"))

ident = Lam "x" (Var "x")


suc = lams [ "n" , "s" , "z"] $ App (Var "s") (app [Var "n" , Var "s" , Var "z"])

add = lams [ "n" , "m" ] $ app [Var "n" ,  suc , Var "m"]

----------------------------------------------------------------------
-- booleans

tt = lams [ "x", "y"] $ Var "x"
ff = lams [ "x", "y"] $ Var "y"
aand = lams ["x", "y"] $ app [Var "x" , Var "y" , ff]

-- examples with free variables
tm1 = App (Var "x") (Lam "y" (App (Var "y") (App (Var "z") (Var "x"))))
tm2 = App (Var "y") (Lam "y" (App (Var "y") (Var "y")))
vs2 = ["x", "z"]
vs3 = ["y"]

s1 = App (Var "y") (Lam "x" (App (Var "x") (App (Var "z") (Var "y"))))

tm3 = Lam "y" (App (Var "y") (Lam "y'" (App (Var "z") (Var "y"))))
s2 = Lam "y'" (App (Var "y'") (Lam "y''" (App (Var "z") (Var "y'"))))