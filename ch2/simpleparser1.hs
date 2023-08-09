module SimpleParser1 where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Functor ((<&>))
import Control.Monad (liftM)
import Numeric (readOct, readBin, readDec, readHex)
import ParseNumbers (parseDec, parseHex, parseDec', parseBin, parseOct)
import LispTypes

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

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) 
        <|> (char 'f' >> return (Bool False))

parseNumber :: Parser LispVal
parseNumber = parseExactNumber 

parseExactNumber :: Parser LispVal
parseExactNumber = parseDec
    <|> parseDec'
    <|> parseBin 
    <|> parseOct 
    <|> parseHex

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
