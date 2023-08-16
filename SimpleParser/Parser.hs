module SimpleParser.Parser (readExpr, readExprList) where
import Text.ParserCombinators.Parsec
import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Functor ((<&>))
import Control.Monad (liftM)
import Numeric (readOct, readBin, readDec, readHex)
import SimpleParser.ParseNumbers (parseExactNumber, parseFloatNumber, parseRationalNumber, parseComplexNumber)
import SimpleParser.LispTypes
import Data.Array
import Control.Monad.Except (MonadError(throwError))

-- readExpr :: String -> ThrowsError LispVal
-- readExpr input = case parse parseExpr "lisp" input of
--     Left err -> throwError $ Parser err
--     Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> try parseNumber
    <|> parseQuoted
    <|> parseQuasiQuoted
    <|> parseUnQuote
    <|> parseUnQuoteSplicing
    <|> try parseBool
    <|> try parseCharacter
    <|> try (do
        string "#("
        x <- parseVector
        char ')'
        return x)
    <|> parseList

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
    return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True))
        <|> (char 'f' >> return (Bool False))

parseNumber :: Parser LispVal
parseNumber = try parseComplexNumber
    <|> try parseFloatNumber
    <|> try parseRationalNumber
    <|> try parseExactNumber

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    val <- try (string "newline" <|> string "space")
        <|> do {x <- anyChar; notFollowedBy alphaNum ; return [x]}
    return $ Character $ case val of
        "space" -> ' '
        "newline" -> '\n'
        _ -> head val

parseList :: Parser LispVal
parseList = between beg end parseList'
           where beg = (char '(' >> skipMany space)
                 end = (skipMany space >> char ')')

parseList' :: Parser LispVal
parseList' = do 
    list <- sepEndBy parseExpr spaces
    maybeDatum <- optionMaybe (char '.' >> spaces >> parseExpr)
    return $ case maybeDatum of
                  Nothing -> List list
                  Just datum  -> DottedList list datum

parseVector :: Parser LispVal
parseVector = do
    arrayValues <- sepBy parseExpr spaces1
    return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do
    char ','
    char '@'
    x <- parseExpr
    return $ List [Atom "unquote-splicing", x]

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

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

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val