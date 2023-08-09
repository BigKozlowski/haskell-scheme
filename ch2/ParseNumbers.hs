module ParseNumbers where
import Text.ParserCombinators.Parsec
import Numeric (readBin, readOct, readHex)
import LispTypes

parseDec :: Parser LispVal
parseDec = many1 digit >>= (return . Number . Int . read)

parseDec' :: Parser LispVal
parseDec' = do 
    try $ string "#d"
    num <- many1 digit
    (return . Number . Int . read) num

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


bin2dig = Int . fst . head . readBin
oct2dig = Int . fst . head . readOct
hex2dig = Int . fst . head . readHex