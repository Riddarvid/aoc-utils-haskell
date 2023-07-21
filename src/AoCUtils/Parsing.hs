module AoCUtils.Parsing (numberParser) where

import           Text.Parsec (Parsec, char, digit, many1, optionMaybe)

numberParser :: Parsec String () Integer
numberParser = do
  sign <- optionMaybe (char '-')
  num <- read <$> many1 digit
  return $ case sign of
    Nothing -> num
    Just _  -> (-num)
