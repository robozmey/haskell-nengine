module Lib where

import Data.Foldable

import System.Console.ANSI

dimension = 3

inf = 999999

newtype Coords = Coords [Double] 
    deriving Show

newtype MyColor4 = MyColor4 (Float, Float, Float, Float)
    deriving (Eq, Ord)

instance Num Coords where
    (+) (Coords x) (Coords y) = Coords $ zipWith (+) x y
    (-) (Coords x) (Coords y) = Coords $ zipWith (-) x y
    (*) (Coords x) (Coords y) = Coords $ zipWith (*) x y
    abs (Coords x) = Coords $ map abs x
    signum (Coords x) = Coords $ map signum x
    fromInteger c = Coords $ take dimension $ repeat $ fromInteger c


scalarMul :: Coords -> Double -> Coords
scalarMul (Coords xs) a = Coords $ map (*a) xs

normilize :: Coords -> Coords
normilize (Coords xs) = Coords $ map (\x -> x / d) xs 
    where d = sqrt $ foldr (\x s -> x^2 + s) 0 xs

data Sphere = Sphere { getSphereCoords :: Coords, getSphereR :: Double, getSphereColor :: MyColor4 } 
data Point = Point { getPointCoords :: Coords }

class Figure f where
    distanceF :: Coords -> f -> Double
    getColor :: f -> MyColor4
    getColor = const whiteColor


whiteColor = MyColor4 (1, 1, 1, 1)
purpleColor = MyColor4 (1, 0, 1, 1)
blackColor = MyColor4 (0, 0, 0, 1)
redColor = MyColor4 (1, 0, 0, 1)
greenColor = MyColor4 (0, 1, 0, 1)
blueColor = MyColor4 (0, 0, 1, 1)
cyanColor = MyColor4 (0, 1, 1, 1)
brownColor = MyColor4 (1, 1, 0, 1)


figures = [Sphere (Coords [2, 0, 0]) 0.1 brownColor, Sphere (Coords [4, 0, 0]) 1 redColor, Sphere (Coords [4, 0, 2]) 1 blueColor, Sphere (Coords [6, 1, 2]) 1 greenColor, Sphere (Coords [4, -3, 2]) 0.1 whiteColor] --map (\x -> Sphere (Coords [x, x, x]) 0.5) [1..1]--

disctance2 :: Coords -> Coords -> Double
disctance2 (Coords x1s) (Coords x2s)  = foldr (+) 0 (zipWith (\y1 y2 -> (y1 - y2)^2) x1s x2s)

--min_distance :: Figure -> Int -> Int -> Int
instance Figure Coords where
    distanceF cxs xs = max 0 $ (disctance2 xs cxs)
instance Figure Point where
    distanceF cxs (Point xs) = max 0 $ (disctance2 xs cxs)
instance Figure Sphere where
    distanceF cxs (Sphere xs r _) = max 0 $ (disctance2 xs cxs - r)
    getColor (Sphere _ _ c) = c

zero_coords = Coords $ take dimension $ repeat 0

screen_distance = 1
pixel_x_size = 0.01
pixel_y_size = 0.01

min_distance :: Coords -> (Double, MyColor4)
min_distance cxs = let (d, c) = foldr min (inf, purpleColor) $ map (\f -> (distanceF cxs f, getColor f)) figures in (sqrt d, c)

moveCoordsD :: Coords -> Coords -> Double -> Coords
moveCoordsD (Coords cxs) (Coords dxs) d = Coords $ zipWith (\cx dx -> cx + d * dx) cxs dxs

moveCoords :: Coords -> Coords -> Coords
moveCoords (Coords cxs) (Coords dxs) = Coords $ zipWith (\cx dx -> cx + dx) cxs dxs

trace' :: Coords -> Coords -> Coords
trace' cxs dxs = moveCoordsD cxs dxs d
    where (d, c) = min_distance cxs

trace :: Coords -> [Coords] -> Int -> Int -> MyColor4
trace my_position (view_direction: (side_direction: (up_direction:oa))) x y = if (md > 0.1) then blackColor else c
    where dxs = normilize $ cxs - my_position
          cxs = my_position + view_direction `scalarMul` screen_distance + side_direction `scalarMul` (fromIntegral  x * pixel_x_size) + up_direction `scalarMul` (fromIntegral y * pixel_y_size)
          fxs = foldl (\cxs' i ->  trace' cxs' dxs) cxs [0..3]
          (md, c) = min_distance fxs
