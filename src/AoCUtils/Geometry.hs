{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}

module AoCUtils.Geometry (
  Point2(P2),
  Vector2,
  upV,
  rightV,
  downV,
  leftV,
  turnLeft,
  turnRight,
  Point3(P3),
  Vector3,
  Point(
    origo,
    distanceBetween,
    distanceFromOrigo,
    moveBy,
    scaleBy,
    vectorBetween
  ),
  findDimensions
) where
import           Control.Applicative (Applicative (liftA2))
import           Data.Hashable       (Hashable)
import           GHC.Generics        (Generic)

data Point2 a = P2 {
  p2X :: a,
  p2Y :: a
}
  deriving (Generic)

type Vector2 = Point2

upV :: Num a => Vector2 a
upV = P2 0 (-1)

rightV :: Num a => Vector2 a
rightV = P2 1 0

downV :: Num a => Vector2 a
downV = P2 0 1

leftV :: Num a => Vector2 a
leftV = P2 (-1) 0

-- Rotates a vector 90 degrees to the left
turnLeft :: Num a => Vector2 a -> Vector2 a
turnLeft (P2 x y) = P2 y (-x)

turnRight :: Num a => Vector2 a -> Vector2 a
turnRight (P2 x y) = P2 (-y) x

instance Functor Point2 where
  fmap :: (a -> b) -> Point2 a -> Point2 b
  fmap f (P2 x y) = P2 (f x) (f y)

instance Applicative Point2 where
  pure :: a -> Point2 a
  pure c = P2 c c
  (<*>) :: Point2 (a -> b) -> Point2 a -> Point2 b
  (P2 f1 f2) <*> (P2 x y) = P2 (f1 x) (f2 y)

instance Foldable Point2 where
  foldr :: (a -> b -> b) -> b -> Point2 a -> b
  foldr f start (P2 x y) = f x (f y start)

instance (Eq a) => Eq (Point2 a) where
  (==) :: Point2 a -> Point2 a -> Bool
  (P2 x1 y1) == (P2 x2 y2) = (x1, y1) == (x2, y2)

instance (Hashable a) => Hashable (Point2 a)

instance (Show a) => Show (Point2 a) where
  show :: Point2 a -> String
  show (P2 x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Point Point2

data Point3 a = P3 {
  p3X :: a,
  p3Y :: a,
  p3Z :: a
}
  deriving (Generic)

type Vector3 = Point3

instance Functor Point3 where
  fmap :: (a -> b) -> Point3 a -> Point3 b
  fmap f (P3 x y z) = P3 (f x) (f y) (f z)

instance Applicative Point3 where
  pure :: a -> Point3 a
  pure c = P3 c c c
  (<*>) :: Point3 (a -> b) -> Point3 a -> Point3 b
  (P3 f1 f2 f3) <*> (P3 x y z) = P3 (f1 x) (f2 y) (f3 z)

instance Foldable Point3 where
  foldr :: (a -> b -> b) -> b -> Point3 a -> b
  foldr f start (P3 x y z) = f x $ f y $ f z start

instance (Eq a) => Eq (Point3 a) where
  (==) :: Point3 a -> Point3 a -> Bool
  (P3 x1 y1 z1) == (P3 x2 y2 z2) = (x1, y1, z1) == (x2, y2, z2)

instance (Hashable a) => Hashable (Point3 a)

instance Point Point3

class (Applicative p, Foldable p) => Point p where
  origo :: (Num a) => p a
  origo = pure 0

  -- Manhattan distance
  distanceBetween :: (Num a) => p a -> p a -> a
  distanceBetween p1 p2 = sum (liftA2 (\a b -> abs (a - b)) p1 p2)

  distanceFromOrigo :: (Num a) => p a -> a
  distanceFromOrigo = distanceBetween origo

  moveBy :: (Num a) => p a -> p a -> p a
  moveBy point vector = (+) <$> point <*> vector

  scaleBy :: Num a => p a -> a -> p a
  scaleBy vector s = (* s) <$> vector

  vectorBetween :: Num a => p a -> p a -> p a
  vectorBetween pStart pEnd = (-) <$> pEnd <*> pStart

findDimensions :: (Integral a, Foldable t, Functor t) => t (Point2 a) -> (a, a, a, a)
findDimensions points = (minimum xs, minimum ys, maximum xs, maximum ys)
  where
    xs = fmap (\(P2 x _) -> x) points
    ys = fmap (\(P2 _ y) -> y) points
