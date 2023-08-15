module SimpleParser.LispTypes where
import Control.Monad.Except
import Data.Array
import Text.Parsec (ParseError)

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
             | Vector (Array Int LispVal) deriving (Eq)

data LispErr = NumArgs Integer [LispVal]
             | TypeMismatch String LispVal
             | Parser ParseError
             | BadSpecialForm String LispVal
             | NotFunction String String
             | UnboundVar String String
             | Default String

type ThrowsError = Either LispErr