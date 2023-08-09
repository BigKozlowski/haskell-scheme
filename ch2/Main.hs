module Main where
import System.Environment
import SimpleParser1 (readExpr)

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr $ head args