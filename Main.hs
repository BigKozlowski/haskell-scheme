module Main where
import System.Environment
import SimpleParser.Parser (readExpr)
import Evaluator.Eval (eval, showVal, runIOThrows, liftThrows, primitiveBindings, bindVars)
import SimpleParser.LispTypes (LispErr (UnboundVar), LispVal, ThrowsError, extractValue, nullEnv, Env)
import Errors.Err (trapError)
import Control.Monad (liftM)
import System.IO
import qualified SimpleParser.LispTypes as Types

main :: IO ()
main = do
    args <- getArgs
    if null args
        then runRepl
        else runOne args

flushStr :: String -> IO()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", Types.List $ map Types.String $ drop 1 args)] 
    runIOThrows (show <$> eval env (Types.List [Types.Atom "load", Types.String (head args)])) 
        >>= hPutStrLn stderr


runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp 0.0.1>>> ") . evalAndPrint