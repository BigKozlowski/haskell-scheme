module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Functor ((<&>))
import Control.Monad (liftM)
import Numeric (readOct, readBin, readDec, readHex)

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr $ head args

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number LispNum
             | Fractal Double
             | String String
             | Bool Bool

data LispNum = Int Integer
             | Short Float
             | Long Double
             deriving (Show, Eq, Ord)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ showLisp val

parseExpr :: Parser LispVal
parseExpr = parseNumber 
    <|> parseAtom 
    <|> parseString
    <|> parseBool

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

-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit

-- parseNumber' :: Parser LispVal
-- parseNumber' = Number . read <$> many1 digit

-- parseNumber'' :: Parser LispVal
-- parseNumber'' = do
--     num <- many1 digit
--     return $ Number $ read num

-- parseNumber''' :: Parser LispVal
-- parseNumber''' = many1 digit >>= return . Number . read

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) 
        <|> (char 'f' >> return (Bool False))

parseNumber :: Parser LispVal
parseNumber = parseExactNumber 
  <|> parseInexactNumber

parseExactNumber :: Parser LispVal
parseExactNumber = parseBin 
    <|> parseOct 
    <|> parseDec 
    <|> parseHex

parseInexactNumber :: Parser LispVal
parseInexactNumber = parsePointedNum
    <|> parseExpNum
    <|> parseHashNum

parseBin :: Parser LispVal
parseBin = parseRadNum 'b' readBin

parseOct :: Parser LispVal
parseOct = parseRadNum 'o' readOct

parseDec :: Parser LispVal
parseDec = parseRadNum 'd' readDec

parseHex :: Parser LispVal
parseHex = parseRadNum 'x' readHex

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
showLisp (DottedList l v) = "Dotted list: " ++ showLisp (List l) ++ showLisp v
showLisp (List l) = "List: " ++ (intercalate ", " $ map showLisp l)
showLisp (Atom a) = "Atom: " ++ a
showLisp (Number n) = "Number: " ++ show n
showLisp (String s) = "String: " ++ s
showLisp (Bool b) = "Bool: " ++ show b

parseRadNum :: Char -> (String -> [(Integer, a)]) -> Parser LispVal
parseRadNum rd readRd = do
    string $ '#' : [rd]
    num <- many1 digit
    return $ Number $ Int $ fst $ head $ readRd num

parsePointedNum :: Parser LispVal
parsePointedNum = do
    i <- many1 digit
    char '.'
    f <- many1 digit
    return $ Number $ Long $ read $ i ++ "." ++ f

parseExpNum :: Parser LispVal
parseExpNum = do
    num <- many1 digit
    char 'e'
    exp <- many1 digit
    return $ Number $ Long $ ((read num) * 10 ** (read exp))

parseHashNum :: Parser LispVal
parseHashNum = do
    num <- many1 digit
    bangs <- many1 (oneOf "#")
    return $ Number $ Long $ (read num) * 10 ^ (length bangs)