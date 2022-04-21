module PublicTests where

import Control.Applicative
import Control.Exception
import System.Exit
import Exercises

-- test x y n means to run test number n, to check that x equals y.
-- return True if the test passed; False if an exception (like for 'undefined') or failed.
test :: (Show a , Eq a) => a -> a -> String -> IO Bool
test x y name =
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
    -- it is nice we can just write a list of tests compactly as follows:
    tests = 
      [
        test (toListPost e) [2,3,4,3,1] "toListPost",
        test (mirror e) e1 "mirror",
        test (btMap (* 10) e1) e2 "btMap",
        test (swap2 [1,2,3,4,5,6,7]) [2,1,4,3,6,5,7] "swap2",
        test (knit [1,3,5,7,9] [2,4,6]) [1,2,3,4,5,6,7,9] "knit",
        test (btTake 2 e2) e3 "btTake",
        test (btDrop 2 e) [Leaf,Leaf,sing 3,sing 4] "btDrop",
        test (btZipWith (*) e e) e4 "btZipWith",
        test (btSubtree [False] e4) (Just $ Node 9 (sing 9) (sing 16)) "btSubtree",
        test (btSubst [False,True] e e3) e5 "btSubst"
      ]
      
