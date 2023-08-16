module Errors.Err where
import SimpleParser.LispTypes
import Control.Monad.Error.Class (MonadError(catchError))

trapError action = catchError action (return . show)
