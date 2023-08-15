module Errors.Err where
import SimpleParser.LispTypes
import Evaluator.Eval
import Control.Monad.Error.Class (MonadError(catchError))

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

showError :: LispErr -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ showLisp form
showError (NotFunction message fn) = message ++ ": " ++ fn
showError (NumArgs expected found) = "Expected " ++ show expected
                                    ++ " args; found values " ++ showLisp (unwordsList found)
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                    ++ ", found " ++ showLisp found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

unwordsList :: [LispVal] -> LispVal
unwordsList ls = List ls