module LispTypes where
import Data.Array

data LispVal = Atom String
             | Bool Bool
             | Character Char
             | Complex Float Float
             | DottedList [LispVal] LispVal
             | Float Float
             | List [LispVal]
             | Number Integer
             | Rational Integer Integer
             | String String
             | Vector (Array Int LispVal)

-- data LispNum = Int Integer
--              | Short Float
--              | Long Double
--              deriving (Show, Eq, Ord)