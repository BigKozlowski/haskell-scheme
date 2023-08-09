module LispTypes where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | Character Char
             | Rational Integer Integer
             | Complex Float Float

-- data LispNum = Int Integer
--              | Short Float
--              | Long Double
--              deriving (Show, Eq, Ord)