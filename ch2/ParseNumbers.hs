module ParseNumbers where
import Text.ParserCombinators.Parsec
import Numeric (readBin, readOct, readHex, readFloat)
import LispTypes

parseExactNumber :: Parser LispVal
parseExactNumber = parseDec
    <|> parseDec'
    <|> parseBin 
    <|> parseOct 
    <|> parseHex

parseFloatNumber :: Parser LispVal
parseFloatNumber = do
    i <- many1 digit
    char '.'
    f <- many1 digit
    return $ Float $ fst . head $ readFloat $ i ++ "." ++ f

parseDec :: Parser LispVal
parseDec = many1 digit >>= (return . Number . read)

parseDec' :: Parser LispVal
parseDec' = do 
    try $ string "#d"
    num <- many1 digit
    (return . Number . read) num

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    num <- many1 digit
    return $ Number (bin2dig num)

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    num <- many1 digit
    return $ Number (oct2dig num)

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    num <- many1 digit
    return $ Number (hex2dig num)


bin2dig = fst . head . readBin
oct2dig = fst . head . readOct
hex2dig = fst . head . readHex