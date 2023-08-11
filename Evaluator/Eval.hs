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
             ]
            
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
    if null parsed
        then 0
        else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

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