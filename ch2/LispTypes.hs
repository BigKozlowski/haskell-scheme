module LispTypes where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number LispNum
             | Fractal Double
             | String String
             | Bool Bool
             | Character Char

data LispNum = Int Integer
             | Short Float
             | Long Double
             deriving (Show, Eq, Ord)