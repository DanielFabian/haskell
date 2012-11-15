-----------------------------------------------------------------------------
--
-- Module      :  Parser
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.List (isPrefixOf)
import Control.Monad
import Numeric (readHex, readOct, readFloat)
import Data.Ratio
import Data.Complex

symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"

data LispVal =
    Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char
    | Float Double
    | Ratio Rational
    | Complex (Complex Double)
    deriving (Show)

parseString = do
    char '"'
    x <- many $ noneOf "\"" <|> try escapedChars
    char '"'
    return $ String x

parseBool = do
    string "#"
    x <- oneOf "tf"
    return $ case x of
        't' -> Bool True
        'f' -> Bool False

parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = [first] ++ rest
    return $ case atom of
                 "#t"      -> Bool True
                 "#f"      -> Bool False
                 otherwise -> Atom atom

escapedChars = do
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
                 '\\' -> x
                 '"'  -> x
                 'n'  -> '\n'
                 'r'  -> '\r'
                 't'  -> '\t'

parseNumber = do
    num <- parseDigital1 <|> try parseDigital2 <|> try parseHex <|> try parseOct <|> try parseBin
    return num

parseDigital1 = do
    numStr <- many1 digit
    return . Number . read $ numStr

parseDigital2 = do
    string "#d"
    numStr <- many1 digit
    return . Number . read $ numStr

parseHex = do
    string "#x"
    numStr <- many1 hexDigit
    return . Number . fst . head . readHex $ numStr

parseOct = do
    string "#o"
    numStr <- many1 octDigit
    return . Number . fst . head . readOct $ numStr

parseBin = do
    string "#b"
    numStr <- many1 (oneOf "01")
    return . Number . readBin $ numStr

readBin string = foldl1 accumulate $ map readDigit string where
    readDigit '0' = 0
    readDigit '1' = 1
    accumulate acc = (2*acc +)

parseChar = do
    string "#\\"
    x <- parseCharName <|> anyChar
    return $ Character x

parseCharName = do
    char <- try (string "space" <|> string "newline")
        <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
    return $ case char of
        "space" -> ' '
        "newline" -> '\n'
        otherwise -> head char

parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return . Float . fst . head . readFloat $ x ++ "." ++ y

parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return . Ratio $ (read x) % (read y)

toDouble (Float f)  = f
toDouble (Number n) = fromIntegral n

parseComplex = do
    x <- (try parseFloat <|> parseDigital1)
    char '+'
    y <- (try parseFloat <|> parseDigital1)
    char 'i'
    return . Complex $ toDouble x :+ toDouble y

parseExpr =
    parseAtom
    <|> parseString
    <|> try parseComplex
    <|> try parseFloat
    <|> try parseRatio
    <|> try parseNumber
    <|> try parseBool
    <|> try parseChar
    <|> try parseQuoted
    <|> do
            char '('
            x <- (try parseList) <|> parseDottedList
            char ')'
            return x

parseList = liftM List $ sepBy parseExpr spaces

parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]






















