import System.Environment
import Data.List
import Data.Char
import Control.Monad
import Data.String

main :: IO ()
main = do
    args <- getArgs
    case args of
        "Read":fileName:_ ->
            do
                contents <- readFile fileName
                writeFile "network.txt" (a1 (read contents :: [(Int, Int)]))
        "Run":fileName:sequence:_->
            do
                contents <- readFile fileName
                print (a2 (read contents :: [(Int, Int)]) (read sequence :: [Int]))
        "Parallel":fileName:_ ->
            do
                contents <- readFile fileName
                writeFile "parallel.txt" (a3 (read contents :: [(Int, Int)]))
        "Sorting":fileName:_ ->
            do
                contents <- readFile fileName
                print (a4 (read contents :: [(Int, Int)]))
        "Create":n:_ ->
            do
                writeFile "parallel.txt" (a5 (read n :: Int))
        _ -> fail "Oops! Try again."

a1 :: [(Int, Int)] -> String
a1 [] = ""
a1 ((a,b):xs) = show a ++ " -- " ++ show b ++ "\n" ++ a1 xs


a2 :: [(Int,Int)] -> [Int] -> [Int]
a2 [] y = y
a2 ((a,b):xs) y
    | (y !! (a-1)) <= (y !! (b-1)) = a2 xs y
    | otherwise = a2 xs (swapElm (a-1) (b-1) y)

swapElm :: Int -> Int -> [Int] -> [Int]
swapElm a b cs = let elmA = cs !! a
                     elmB = cs !! b
                     left = take a cs
                     mid = take (b-a-1) (drop(a+1) cs)
                     right = drop (b+1) cs
                in left ++ [elmB] ++ mid ++ [elmA] ++ right


a3 :: [(Int, Int)] -> String
a3 a = parPutStrLn (buildPar [] a)

buildPar :: [[(Int, Int)]] -> [(Int, Int)] -> [[(Int, Int)]]
buildPar a [] = a
buildPar a b = buildPar (a ++ [isThereDiff (firstPair b) [] b]) (isThereSmallerDiff (firstPair b) [] [] b)

firstPair :: [(Int, Int)] -> (Int, Int)
firstPair ((a1,a2):_) = (a1,a2)

isThereDiff :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
isThereDiff (a1,a2) [] list = isThereDiff (a1,a2) [(a1,a2)] list
isThereDiff (a1,a2) b ((c1,c2):cs) = if worksPar (c1,c2) b then isThereDiff (a1,a2) (b ++  [(c1,c2)]) cs else isThereDiff (a1,a2) b cs
isThereDiff _ output [] = output

isThereSmallerDiff :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
isThereSmallerDiff _ b _ [] = b
isThereSmallerDiff (x,y) b [] (a:as) = isThereSmallerDiff (x,y) b [(x,y)] as
isThereSmallerDiff (x,y) b c ((d1,d2):ds) = if worksPar (d1,d2) c then isThereSmallerDiff (x,y) b (c ++  [(d1,d2)]) ds else isThereSmallerDiff (x,y) (b ++ [(d1,d2)]) c ds

worksPar :: (Int, Int) -> [(Int, Int)] -> Bool
worksPar (a1,a2) ((b1,b2):xs) = ((a1 /= b1) && (a1 /= b2) && (a2 /= b1) && (a2 /= b2)) && worksPar (a1,a2) xs
worksPar _ [] = True


a4 :: [(Int,Int)] -> Bool
a4 a = doesItSort a (genNumbers (maxNumGen a) [])

maxNumGen :: [(Int,Int)] -> Int
maxNumGen [] = 0
maxNumGen ((x,y):cs) = max (max x y) (maxNumGen cs)

doesItSort :: [(Int, Int)] -> [[Int]] -> Bool
doesItSort a = foldr ((&&) . isItSorted . a2 a) True

genNumbers :: Int -> [Int] -> [[Int]]
genNumbers 0 a = [a]
genNumbers n a = genNumbers (n-1) (1 : a) ++ genNumbers (n-1) (0 : a)

isItSorted ::[Int] -> Bool
isItSorted [] = True
isItSorted [a] = True
isItSorted (a:b:cs) = (a <= b) && isItSorted (b:cs)

a5 :: Int -> String
a5 n = parPutStrLn (createSortComp 2 2 n)

parPutStrLn :: [[(Int,Int)]] -> String
parPutStrLn [] = ""
parPutStrLn (x:xs) = a1Different x ++ "\n" ++ parPutStrLn xs

a1Different :: [(Int, Int)] -> String
a1Different [] = ""
a1Different [(a,b)] = show a ++ " -- " ++ show b
a1Different ((a,b):xs) = show a ++ " -- " ++ show b ++ " , " ++ a1Different xs

createSortComp :: Int -> Int -> Int -> [[(Int, Int)]]
createSortComp _ _ 0 = []
createSortComp _ _ 1 = []
createSortComp _ 1 _ = []
createSortComp a b c = if a < c then newList b : createSortComp (a+1) (b+1) c else newList b : createSortComp (a+1) (b-1) c

newList :: Int -> [(Int,Int)]
newList 0 = []
newList 1 = []
newList a = newList (a-2) ++ [(a-1,a)]