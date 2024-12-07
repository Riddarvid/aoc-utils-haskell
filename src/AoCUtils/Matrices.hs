module AoCUtils.Matrices (matrixToMapList) where

import           AoCUtils.Geometry (Point2 (P2))

matrixToMapList :: [[a]] -> ([(Point2 Int, a)], Int, Int)
matrixToMapList input = (mapList, maxX, maxY)
  where
    list = concat input
    maxY = length input - 1
    maxX = maximum (map length input) - 1
    mapList = zip [P2 x y | y <- [0 .. maxY], x <- [0 .. (length (input !! y) - 1)]] list
