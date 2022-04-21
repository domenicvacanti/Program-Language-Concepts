module PublicTests where

import Control.Applicative
import Control.Exception
import System.Exit
import Exercises

t1 :: MyMaybe Integer
t1 = MyJust 7
r1 = MyJust 70

t2 :: BinTree Integer
t2 = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)
r2 = Node 10 (Node 20 Leaf Leaf) (Node 30 (Node 40 Leaf Leaf) Leaf)

t3 :: ValTree Integer
t3 = ValTree [1,2,3,4] t2
r3 = ValTree [10,20,30,40] r2

functorTest :: (Functor f, Eq (f b), Show (f b)) => String -> (a -> b) -> f a -> f b -> IO Bool
functorTest s f t r = test ("fmap for " ++ s) (f <$> t) r 

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
    -- it is nice we can just write a list of tests compactly as follows:
    tests = 
      [
        functorTest "MyMaybe" (*10) t1 r1,
        functorTest "ValTree" (*10) t3 r3,
        functorTest "Tree" not t4 r4,
        test "a1" (a1 [1,2,3] 5) [5,5,5],
        test "a2" (a2 not (Just [True,False,False])) (Just [False,True,True]),
        test "a3" (zipWith ($) (a3 [(+4),(+5)] [(*10),(*100)]) [1,2,3,4]) [14,204,35,405],
        functorTest "Logging" (*10) (lg "hi" <*> pure 1) (Logging ["hi"] 10),
        test "Applicative Logging" ((lg "hi" <*> pure not) <*> (lg "bye" <*> pure True)) (Logging ["hi","bye"] False),
        test "rlel" (rlel "aaabbcccc") $ Logging ["change","change","base"] [(3,'a'),(2,'b'),(4,'c')],
        test "btSubtreea" (btSubtreea [True,True] e1) (Accum [1,3] (sing 4))
        
      ]
      
