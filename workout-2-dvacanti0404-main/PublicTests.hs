module PublicTests where

import Control.Applicative
import Control.Exception
import System.Exit
import Exercises

-- test x y n means to run test number n, to check that x equals y.
-- return True if the test passed; False if an exception (like for 'undefined') or failed.
test :: (Show a , Ord a) => a -> a -> Int -> IO Bool
test x y n =
  let name = "test for p" ++ show n in
  do
    b <- catch (evaluate x >>= (\ ex -> return $ Just $ ex == y))
          (\ e -> let _ = e :: SomeException in return Nothing)
    case b of
      Nothing ->
        putStrLn ("Unimplemented p" ++ show n ++ ".") >> return False
      Just True -> 
        putStrLn ("Passing " ++ name ++ ".") >> return True
      Just False ->
        putStrLn ("Failing " ++ name ++ ": expected " ++ show y ++ ", computed " ++ show x) >> return False

main :: IO ()
main =
  do
    putStrLn ("Executing " ++ show (length tests) ++ " tests.")
    b <- sequence (zipWith ($) tests [1..])
    if and b then
      exitSuccess
    else
      exitFailure
  where
    -- it is nice we can just write a list of tests compactly as follows:
    tests = 
      [
        test (p1 not True) False ,
        test (p2 (+) 4) 8 ,
        test (p3 (+ 10) (1,2)) (11,12) ,
        test (p4 (+) (* 10) 3) 33 ,
        test (p5 (2,3) (* 10) fst) 20 ,
        test (p6 (uncurry (-)) (3,5)) 2 ,
        test (p7 [1..9]) [2,4,6,8] ,
        test (p8 [0,1,0,1,0,1,2]) 3 ,
        test (p9 [1..9]) True  ,
        test (p10 length even [[1..2],[1..3],[1..4],[1..5]]) [[1..2],[1..4]] ,
        test (p11 length [[1..2],[1..3],[1..4]]) [([1..2],2),([1..3],3),([1..4],4)] ,
        test (p12 [1,2,3,4,5]) 120,
        test (p13 1 [[2]]) [[1,2],[2]],
        test (p14 [1,2,3]) [[1,2,3],[2,3],[3],[]],
        test (p15 [1,2,3] [10,20,30]) [11,22,33] ,
        test (zipWith ($) (p16 [not,id,not] [not,not,id]) [True,True,True]) [True,False,False]
      ]
      
