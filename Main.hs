module Main where
import System.Environment
import SimpleParser.Parser (readExpr)

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr $ head args