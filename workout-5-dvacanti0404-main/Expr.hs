{- expressions using basic arithmetic and locally scoped definitions ("let") -}
module Expr where
import Accum

data Var = Var String
  deriving (Read, Show, Eq, Ord)

data Expr = Int Int | Add Expr Expr | Mult Expr Expr | Use Var | Let Var Expr Expr
  deriving (Read , Show , Eq)

-- some expressions and expected results for tests in PublicTests

e1 = Add (Int 3)
         (Let (Var "x") (Int 4)
            (Add (Use (Var "x"))
                 (Let (Var "y") (Mult (Use (Var "x")) (Int 3))
                    (Use (Var "y")))))

e2 = Let (Var "x") (Let (Var "y") (Int 3) (Add (Use (Var "y")) (Int 2)))
      (Add (Use (Var "x")) (Int 1))

r1 = Accum [(Var "x",Int 4),(Var "y",Mult (Use (Var "x")) (Int 3))] (Add (Int 3) (Add (Use (Var "x")) (Use (Var "y"))))
r2 = Accum [(Var "y",Int 3),(Var "x",Add (Use (Var "y")) (Int 2))] (Add (Use (Var "x")) (Int 1))
r3 = Let (Var "x") (Int 3) (Let (Var "y") (Add (Use (Var "x")) (Int 2)) (Add (Use (Var "x")) (Use (Var "y"))))
r4 = Let (Var "x") (Int 4) (Let (Var "y") (Mult (Use (Var "x")) (Int 3)) (Add (Int 3) (Add (Use (Var "x")) (Use (Var "y")))))

l1 = [(Var "x",(Int 3)),(Var "y", (Add (Use (Var "x")) (Int 2)))]
e3 = Add (Use (Var "x")) (Use (Var "y"))

vs = [Var "x", Var "y"]