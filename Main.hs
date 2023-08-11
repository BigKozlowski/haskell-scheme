module Main where
import System.Environment
import SimpleParser.Parser (readExpr)
import Evaluator.Eval (eval, showLisp)
import SimpleParser.LispTypes (LispErr, LispVal)
import Errors.Err (showError, extractValue, trapError)
import Control.Monad (liftM)

instance Show LispErr where show = showError
instance Show LispVal where show = showLisp

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (head args) >>= eval
    case evaled of
        (Left err) -> print err
        (Right val) -> print val
