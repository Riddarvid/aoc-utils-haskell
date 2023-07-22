{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module AoCUtils.Regex (
  parseUnsignedInts,
  parseSignedInts
) where

import           Text.Regex.PCRE (AllTextMatches (getAllTextMatches), (=~))

parseUnsignedInts :: (Integral a, Read a) => String -> [a]
parseUnsignedInts = parseMany unsignedIntRegex

parseSignedInts :: (Integral a, Read a) => String -> [a]
parseSignedInts = parseMany signedIntRegex

parseMany :: (Read a) => String -> String -> [a]
parseMany regex str = map read (getAllTextMatches (str =~ regex))

unsignedIntRegex :: String
unsignedIntRegex = "\\d+"

signedIntRegex :: String
signedIntRegex = "-?" ++ unsignedIntRegex
