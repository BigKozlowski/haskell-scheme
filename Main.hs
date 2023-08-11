module Main where
import System.Environment
import SimpleParser.Parser (readExpr)
import Evaluator.Eval (showLisp, eval)

main :: IO ()
main = getArgs >>= putStrLn . showLisp . eval . readExpr . head