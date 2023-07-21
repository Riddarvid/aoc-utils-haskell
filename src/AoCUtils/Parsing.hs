{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module AoCUtils.Parsing (
  unsignedIntegerParser,
  signedIntegerParser,
  parseUnsignedInts,
  parseSignedInts
) where

import           Text.Parsec (ParseError, Parsec, anyChar, char, digit,
                              lookAhead, many1, manyTill, optionMaybe, parse,
                              sepEndBy)

parseUnsignedInts :: (Num a, Read a) => String -> Either ParseError [a]
parseUnsignedInts = parseMaybe (manyParser unsignedIntegerParser)

parseSignedInts :: (Num a, Read a) => String -> Either ParseError [a]
parseSignedInts = parseMaybe (manyParser signedIntegerParser)

parseMaybe :: Parsec String () a -> String -> Either ParseError a
parseMaybe p = parse p ""

manyParser :: Parsec String () a -> Parsec String () [a]
manyParser p = sepEndBy p (manyTill anyChar (lookAhead p))

unsignedIntegerParser :: (Num a, Read a) => Parsec String () a
unsignedIntegerParser = read <$> many1 digit

signedIntegerParser :: (Num a, Read a) => Parsec String () a
signedIntegerParser = do
  sign <- optionMaybe (char '-')
  num <- unsignedIntegerParser
  return $ case sign of
    Nothing -> num
    Just _  -> (-num)
