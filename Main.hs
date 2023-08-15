module Main where
import System.Environment
import SimpleParser.Parser (readExpr)
import Evaluator.Eval (eval, showLisp)
import SimpleParser.LispTypes (LispErr, LispVal)
import Errors.Err (showError, extractValue, trapError)
import Control.Monad (liftM)
import System.IO

instance Show LispErr where show = showError
instance Show LispVal where show = showLisp

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> evalAndPrint $ head args
        _ -> putStrLn "Program takes only 0 to 1 argument"

flushStr :: String -> IO()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp 1 >>> ") evalAndPrint