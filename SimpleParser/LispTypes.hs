module SimpleParser.LispTypes where
import Control.Monad.Except ( ExceptT )
import Data.Array ( Array )
import Text.Parsec (ParseError)
import Data.List (intercalate)
import Data.IORef ( IORef, newIORef )
import GHC.IO.Handle (Handle)

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ExceptT LispErr IO

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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: Maybe String,
                      body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

data LispErr = NumArgs Integer [LispVal]
             | TypeMismatch String LispVal
             | Parser ParseError
             | BadSpecialForm String LispVal
             | NotFunction String String
             | UnboundVar String String
             | Default String

type ThrowsError = Either LispErr

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

showError :: LispErr -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ showVal form
showError (NotFunction message fn) = message ++ ": " ++ fn
showError (NumArgs expected found) = "Expected " ++ show expected
                                    ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                    ++ ", found " ++ showVal found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

instance Show LispErr where show = showError
instance Show LispVal where show = showVal

nullEnv :: IO Env
nullEnv = newIORef []