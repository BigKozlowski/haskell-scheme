module Errors.Err where
import Control.Monad.Error.Class (MonadError(catchError))

trapError :: (MonadError e m, Show e) => m String -> m String
trapError = flip catchError (return . show)