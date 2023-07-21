{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module AoCUtils.Parsing (
  unsignedIntegerParser,
  signedIntegerParser,
  parseUnsignedInts,
  parseSignedInts
) where

import           Text.Parsec     (Parsec, char, digit, many1, optionMaybe)
import           Text.Regex.PCRE (AllTextMatches (getAllTextMatches), (=~))

-- Regex parsers

parseUnsignedInts :: (Num a, Read a) => String -> [a]
parseUnsignedInts = parseMany unsignedIntRegex

parseSignedInts :: (Num a, Read a) => String -> [a]
parseSignedInts = parseMany signedIntRegex

parseMany :: (Read a) => String -> String -> [a]
parseMany regex str = map read (getAllTextMatches (str =~ regex))

unsignedIntRegex :: String
unsignedIntRegex = "\\d+"

signedIntRegex :: String
signedIntRegex = "-?" ++ unsignedIntRegex

-- Parsec parsers

unsignedIntegerParser :: (Num a, Read a) => Parsec String () a
unsignedIntegerParser = read <$> many1 digit

signedIntegerParser :: (Num a, Read a) => Parsec String () a
signedIntegerParser = do
  sign <- optionMaybe (char '-')
  num <- unsignedIntegerParser
  return $ case sign of
    Nothing -> num
    Just _  -> (-num)
