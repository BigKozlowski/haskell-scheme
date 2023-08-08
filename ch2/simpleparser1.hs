module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Functor ((<&>))
import Control.Monad (liftM)

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr $ head args

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ showLisp val

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ many1 (noneOf "\"\\") <|> escapedChars 
    char '"'
    return $ String $ concat x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    let atom = first : rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseNumber' :: Parser LispVal
parseNumber' = Number . read <$> many1 digit

parseNumber'' :: Parser LispVal
parseNumber'' = do
    num <- many1 digit
    return $ Number $ read num

parseNumber''' :: Parser LispVal
parseNumber''' = many1 digit >>= return . Number . read

parseNumber'''' :: Parser LispVal
parseNumber'''' = many1 digit <&> Number . read

symbol :: Parser Char
symbol = oneOf "!$%&|*+ -/: <=? >@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser String
escapedChars = do
    char '\\'
    x <- oneOf "\\\"ntr"
    case x of
        '\\' -> do return [x]
        '"' -> do return [x]
        't' -> do return "\t"
        'n' -> do return "\n"
        'r' -> do return "\r"

showLisp :: LispVal -> String
showLisp (DottedList l v) = showLisp (List l) ++ showLisp v
showLisp (List l) = intercalate ", " $ map showLisp l
showLisp (Atom a) = a
showLisp (Number n) = show n
showLisp (String s) = s
showLisp (Bool b) = show b