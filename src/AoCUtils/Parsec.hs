{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module AoCUtils.Parsec (
  unsignedIntegerParser,
  signedIntegerParser
) where

import           Text.Parsec (Parsec, char, digit, many1, optionMaybe)

unsignedIntegerParser :: (Integral a, Read a) => Parsec String () a
unsignedIntegerParser = read <$> many1 digit

signedIntegerParser :: (Integral a, Read a) => Parsec String () a
signedIntegerParser = do
  sign <- optionMaybe (char '-')
  num <- unsignedIntegerParser
  return $ case sign of
    Nothing -> num
    Just _  -> (-num)
