module Parser where

import Text.Parsec
import Text.Parsec.String

import Control.Monad (void)

-- GENERIC ------------------------------------------------

parseByLineUntil :: Parser a -> Parser b -> Parser [a]
parseByLineUntil parser end = do
    line <- parser
    lines <- many $ newline *> parser
    end
    return (line:lines)

parseByLine :: Parser a -> Parser [a]
parseByLine parser = parseByLineUntil parser eof

-- INTEGERS -----------------------------------------------

parseInteger :: Parser Int
parseInteger = read <$> many1 digit

parseIntegerLines :: Parser [Int]
parseIntegerLines = parseByLine parseInteger

-- UTIL ---------------------------------------------------

eol :: Parser ()
eol = void newline <|> eof
