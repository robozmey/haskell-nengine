module Figures where

import Constants
import Coords

data Sphere = Sphere { getSphereCoords :: Coords, getSphereR :: Double, getSphereColor :: MyColor4 } 
data Point = Point { getPointCoords :: Coords }

class Figure f where
    distanceF :: Coords -> f -> Double
    getColor :: f -> MyColor4
    getColor = const whiteColor

instance Figure Coords where
    distanceF cxs xs = max 0 $ (disctance2 xs cxs)
instance Figure Point where
    distanceF cxs (Point xs) = max 0 $ (disctance2 xs cxs)
instance Figure Sphere where
    distanceF cxs (Sphere xs r _) = max 0 $ (disctance2 xs cxs - r)
    getColor (Sphere _ _ c) = c

min_distance :: Coords -> (Double, MyColor4)
min_distance cxs = let (d, c) = foldr min (inf, purpleColor) $ map (\f -> (distanceF cxs f, getColor f)) figures in (sqrt d, c)

figures = [Sphere (Coords [0, 2, 0]) 0.1 brownColor, Sphere (Coords [0, 4, 0]) 1 redColor, Sphere (Coords [0, 4, 2]) 1 blueColor, Sphere (Coords [1, 6, 2]) 1 greenColor, Sphere (Coords [4, -3, 2]) 0.1 whiteColor] --map (\x -> Sphere (Coords [x, x, x]) 0.5) [1..1]--
