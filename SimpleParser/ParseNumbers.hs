module SimpleParser.ParseNumbers where
import Text.ParserCombinators.Parsec
    ( char, digit, string, many1, (<|>), Parser, try )
import Numeric (readBin, readOct, readHex, readFloat)
import SimpleParser.LispTypes
    ( LispVal(Number, Rational, Float, Complex) )
import Data.Functor ((<&>))

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

parseRationalNumber :: Parser LispVal
parseRationalNumber = do
    num <- many1 digit
    char '/'
    denom <- many1 digit
    let num' = read num :: Integer
        denom' = read denom :: Integer
        gcd' = gcd num' denom'
    return $ Rational (num' `div` gcd') (denom' `div` gcd')

parseComplexNumber :: Parser LispVal
parseComplexNumber = do
    r <- try parseFloatNumber <|> parseExactNumber
    char '+'
    i <- try parseFloatNumber <|> parseExactNumber
    char 'i'
    let r' = case r of
            (Float n) -> n
            (Number n) -> fromInteger n
        i' = case i of
            (Float n) -> n
            (Number n) -> fromInteger n
    return $ Complex r' i'

parseDec :: Parser LispVal
parseDec = many1 digit <&> Number . read

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