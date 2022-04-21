module PublicTests where

import Control.Applicative
import Control.Exception
import System.Exit
import Exercises1
import Exercises2
import Accum
import Expr
import Term

-- test x y n means to run test number n, to check that x equals y.
-- return True if the test passed; False if an exception (like for 'undefined') or failed.
test :: (Show a , Eq a) => String -> a -> a -> IO Bool
test name x y =
  do
    b <- catch (evaluate x >>= (\ ex -> return $ Just $ ex == y))
          (\ e -> let _ = e :: SomeException in return Nothing)
    case b of
      Nothing ->
        putStrLn ("Unimplemented: " ++ name ++ ".") >> return False
      Just True -> 
        putStrLn ("Passing: " ++ name ++ ".") >> return True
      Just False ->
        putStrLn ("Failing: " ++ name ++ ". Expected " ++ show y ++ ", computed " ++ show x) >> return False

main :: IO ()
main =
  do
    putStrLn ("Executing " ++ show (length tests) ++ " tests.")
    b <- sequence tests
    if and b then
      exitSuccess
    else
      exitFailure
  where
    -- e1, r1, etc. are defined in Expr.hs
    tests = 
      [
        -- tests for Exercises1:
        test "extractLets 1" (extractLets e1) r1,
        test "extractLets 2" (extractLets e2) r2,
        test "wrapLets" (wrapLets l1 e3) r3 ,
        test "liftLets" (liftLets e1) r4,
        test "collectVars" (collectVars e1) vs,

        -- tests for Exercises2:
        test "freeVars 1" (freeVars tm1) vs2,
        test "freeVars 2" (freeVars tm2) vs3,
        test "freeVars'" (freeVars' tm1) vs2,
        test "swapNames" (swapNames "x" "y" tm1) s1,
        test "renameAwayFrom" (renameAwayFrom ["y"] tm3) s2
      ]
      
