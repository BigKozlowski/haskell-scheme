module Evaluator.Eval (showLisp, eval) where 
import SimpleParser.LispTypes
import Data.List
import Data.Array
import Control.Monad.Except

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply fn args = maybe 
    (throwError $ NotFunction "Unrecognized primitive function args" fn) 
    ($ args) 
    (lookup fn primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop mod)
             , ("remainder", numericBinop rem)
             , ("symbol?", unaryOp symbolp)
             , ("string?", unaryOp stringp)
             , ("number?", unaryOp numberp)
             , ("bool?", unaryOp boolp)
             , ("list?", unaryOp listp)
             ]
            
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [] = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
    if null parsed 
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _ = Bool False
numberp (Number _) = Bool True
numberp _ = Bool False
stringp (String _) = Bool True
stringp _ = Bool False
boolp (Bool _) = Bool True
boolp _ = Bool False
listp (List _ ) = Bool True
listp (DottedList _ _) = Bool False
listp _ = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _ = String ""
string2symbol (String s) = Atom s
string2symbol _ = Atom ""

showLisp :: LispVal -> String
showLisp (Atom a) = "Atom: " ++ a
showLisp (Bool b) = "Bool: " ++ show b
showLisp (Character c) = "Char: " ++ [c]
showLisp (Complex r i) = "Complex: " ++ show r ++ "+" ++ show i ++ "i"
showLisp (DottedList l v) = "Dotted list: " ++ showLisp (List l) ++ ", " ++ showLisp v
showLisp (Float f) = "Float: " ++ show f
showLisp (List l) = "List: " ++ "(" ++ (intercalate ", " $ map showLisp l) ++ ")"
showLisp (Number n) = "Number: " ++ show n
showLisp (Rational n d) = "Rational: " ++ show n ++ "/" ++ show d
showLisp (String s) = "String: " ++ s
showLisp (Vector v) = "Vector: " ++ (showLisp $ List $ elems v)