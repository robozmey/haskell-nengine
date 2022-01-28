module Lib where

import Data.Foldable
import System.Console.ANSI

dimension = 3

inf = 999999

newtype Coords = Coords [Double]

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

data Sphere = Sphere { getSphereCoords :: Coords, getSphereR :: Double } 
data Point = Point { getPointCoords :: Coords }

class Figure f where
    distanceF :: Coords -> f -> Double


figures = [Sphere (Coords [2, 0, 0]) 0.1] --map (\x -> Sphere (Coords [x, x, x]) 0.5) [1..1]--

disctance2 :: Coords -> Coords -> Double
disctance2 (Coords x1s) (Coords x2s)  = foldr (+) 0 (zipWith (\y1 y2 -> (y1 - y2)^2) x1s x2s)

--min_distance :: Figure -> Int -> Int -> Int
instance Figure Coords where
    distanceF cxs xs = max 0 $ (disctance2 xs cxs)
instance Figure Point where
    distanceF cxs (Point xs) = max 0 $ (disctance2 xs cxs)
instance Figure Sphere where
    distanceF cxs (Sphere xs r) = max 0 $ (disctance2 xs cxs - r)

zero_coords = Coords $ take dimension $ repeat 0

screen_distance = 1
pixel_i_size = 0.028
pixel_j_size = 0.014


view_direction = Coords [1, 0, 0]
up_direction = Coords [0, 1, 0]
side_direction = Coords [0, 0, 1]

my_position = zero_coords

min_distance :: Coords -> Double
min_distance cxs = sqrt $ foldr min inf $ map (distanceF cxs) figures

moveCoordsD :: Coords -> Coords -> Double -> Coords
moveCoordsD (Coords cxs) (Coords dxs) d = Coords $ zipWith (\cx dx -> cx + d * dx) cxs dxs

moveCoords :: Coords -> Coords -> Coords
moveCoords (Coords cxs) (Coords dxs) = Coords $ zipWith (\cx dx -> cx + dx) cxs dxs

trace' :: Coords -> Coords -> Coords
trace' cxs dxs = moveCoordsD cxs dxs d
    where d = min_distance cxs

trace :: Int -> Int -> Int
trace i j = if (md > 0.5) then 0 else 4
    where dxs = normilize $ cxs - my_position
          cxs = my_position + view_direction `scalarMul` screen_distance + up_direction `scalarMul` (fromIntegral  i * pixel_i_size) + side_direction `scalarMul` (fromIntegral j * pixel_j_size)
          fxs = foldl (\cxs' i ->  trace' cxs' dxs) cxs [0..10]
          md = min_distance fxs

drawBorders :: IO ()
drawBorders = do
    clearScreen
    Just (x, y) <- getTerminalSize
    setCursorPosition (x `div` 2) (y `div` 2)
    setTitle "ANSI Terminal Short Example"
    forM_ [1..(x-2)] (\i -> do 
        setCursorPosition i 0
        putStr "│"
        setCursorPosition i (y-1)
        putStr "│"
        )
    forM_ [1..(y-2)] (\i -> do 
        setCursorPosition 0 i
        putStr "═"
        setCursorPosition (x-1) i
        putStr "═"
        )
    setCursorPosition x y
    putChar '╝'
    setCursorPosition x 0
    putChar '╚'
    setCursorPosition 0 y
    putChar '╗'
    setCursorPosition 0 0
    putChar '╔'
    

updateAll = do
    setCursorPosition 0 0
    putStrLn "╔"
