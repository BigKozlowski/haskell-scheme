module Evaluator.Eval (showLisp) where 
import SimpleParser.LispTypes
import Data.List
import Data.Array

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