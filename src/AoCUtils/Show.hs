module AoCUtils.Show (showPoints) where

import           AoCUtils.Geometry (Point2 (P2))
import           Data.Hashable     (Hashable)
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS

showPoints :: (Enum a, Hashable a, Ord a) => HashSet (Point2 a) -> String
showPoints points = unlines $ map (showPointRow points minX maxX) [minY .. maxY]
  where
    (minX, minY, maxX, maxY) = findDimensions points

showPointRow :: (Enum a, Hashable a) => HashSet (Point2 a) -> a -> a -> a -> String
showPointRow points minX maxX y = map (\x -> showPoint points x y) [minX .. maxX]

showPoint :: Hashable a => HashSet (Point2 a) -> a -> a -> Char
showPoint points x y
  | HS.member (P2 x y) points = '#'
  | otherwise = ' '

findDimensions :: (Ord a, Hashable a) => HashSet (Point2 a) -> (a, a, a, a)
findDimensions points = (minimum xs, minimum ys, maximum xs, maximum ys)
  where
    xs = HS.map (\(P2 x _) -> x) points
    ys = HS.map (\(P2 _ y) -> y) points
