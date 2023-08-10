module Main where
import System.Environment
import SimpleParser.Parser (readExpr)
import Evaluator.Eval (showLisp)

main :: IO ()
main = do
    args <- getArgs
    case readExpr $ head args of
        Left err -> putStrLn err
        Right val -> putStrLn $ showLisp val