{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module AoCUtils.Parsing (
  unsignedIntegerParser,
  signedIntegerParser,
  parseUnsignedInts,
  parseSignedInts
) where

import           Text.Parsec (Parsec, anyToken, char, digit, many1, optionMaybe,
                              parse, sepBy)

parseUnsignedInts :: (Num a, Read a) => String -> Maybe [a]
parseUnsignedInts = parseMaybe (manyParser unsignedIntegerParser)

parseSignedInts :: (Num a, Read a) => String -> Maybe [a]
parseSignedInts = parseMaybe (manyParser signedIntegerParser)

parseMaybe :: Parsec String () a -> String -> Maybe a
parseMaybe p str = case parse p "" str of
  Right res -> Just res
  Left _    -> Nothing

manyParser :: Parsec String () a -> Parsec String () [a]
manyParser p = sepBy p anyToken

unsignedIntegerParser :: (Num a, Read a) => Parsec String () a
unsignedIntegerParser = read <$> many1 digit

signedIntegerParser :: (Num a, Read a) => Parsec String () a
signedIntegerParser = do
  sign <- optionMaybe (char '-')
  num <- unsignedIntegerParser
  return $ case sign of
    Nothing -> num
    Just _  -> (-num)
