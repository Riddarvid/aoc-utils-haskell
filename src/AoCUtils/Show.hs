module AoCUtils.Show (
  showPoints
) where

import           AoCUtils.Geometry (Point2 (P2), findDimensions)
import           Data.Foldable     (toList)
import           Data.Hashable     (Hashable)
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS

showPoints :: (Integral a, Hashable a, Foldable t) => t (Point2 a) -> String
showPoints points = unlines $ map (showPointRow pointSet minX maxX) [minY .. maxY]
  where
    (minX, minY, maxX, maxY) = findDimensions points
    pointSet = HS.fromList $ toList points

showPointRow :: (Integral a, Hashable a) => HashSet (Point2 a) -> a -> a -> a -> String
showPointRow points minX maxX y = map (\x -> showPoint points x y) [minX .. maxX]

showPoint :: (Hashable a) => HashSet (Point2 a) -> a -> a -> Char
showPoint points x y
  | HS.member (P2 x y) points = '#'
  | otherwise = ' '
