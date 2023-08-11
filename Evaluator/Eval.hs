module Evaluator.Eval (showLisp, eval) where 
import SimpleParser.LispTypes
import Data.List
import Data.Array

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply fn args = maybe (Bool False) ($ args) $ lookup fn primitives

primitives :: [(String, [LispVal] -> LispVal)]
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
            
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

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