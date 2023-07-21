module AoCUtils.Graphics (
  tick,
  square,
  renderSquare
) where

import           AoCUtils.Geometry (Point2 (P2))
import           Data.Sequence     (Seq, ViewR ((:>)))
import qualified Data.Sequence     as Seq
import           Graphics.Gloss    (Color, Picture, color, rectangleSolid,
                                    translate)

tick :: Float -> [Picture] -> (Float -> Picture)
tick interval = tick' interval . Seq.fromList

tick' :: Float -> Seq Picture -> (Float -> Picture)
tick' interval pics time = case Seq.lookup index pics of
  Just pic -> pic
  Nothing  -> case Seq.viewr pics of
    (_ :> pic) -> pic
    _          -> error "Sequence is empty"
  where
    index = floor $ time / interval

square :: Picture
square = rectangleSolid 1 1

renderSquare :: Integral a => Color -> Point2 a -> Picture
renderSquare c (P2 x y) = translate x' y' $ color c square
  where
    x' = fromIntegral x
    y' = fromIntegral y
