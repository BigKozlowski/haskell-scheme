module Main where
import System.Environment
import SimpleParser.Parser (readExpr)
import Evaluator.Eval (eval, showVal, runIOThrows, liftThrows, primitiveBindings)
import SimpleParser.LispTypes (LispErr (UnboundVar), LispVal, ThrowsError, extractValue, nullEnv, Env)
import Errors.Err (trapError)
import Control.Monad (liftM)
import System.IO

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> runOne $ head args
        _ -> putStrLn "Program takes only 0 to 1 argument"

flushStr :: String -> IO()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp 0.0.1>>> ") . evalAndPrint