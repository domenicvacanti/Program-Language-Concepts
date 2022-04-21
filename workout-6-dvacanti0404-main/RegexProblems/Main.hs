-- just calls the lexer and prints the resulting list of Tokens
import Tokens
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  putStrLn $ show $ alexScanTokens s
