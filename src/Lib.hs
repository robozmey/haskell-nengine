module Lib where

import Data.Foldable
import Data.Matrix
import Control.Lens

import System.Console.ANSI


import Constants
import Coords
import Figures

data AxisType = ViewDirection | SideDirection | UpDirection

newtype Axises = Axises [Coords]
    deriving Show

getViewDirection :: Axises -> Coords
getViewDirection (Axises axs) = axs !! 1
getSideDirection :: Axises -> Coords
getSideDirection (Axises axs) = axs !! 0
getUpDirection :: Axises -> Coords
getUpDirection (Axises axs) = axs !! 2
getMainDirections :: Axises -> (Coords, Coords, Coords)
getMainDirections axs = (getViewDirection axs, getUpDirection axs, getSideDirection axs)

start_view_direction = Coords $ take dimension ([1, 0, 0] ++ repeat 0)
start_up_direction = Coords $ take dimension ([0, 1, 0] ++ repeat 0)
start_side_direction = Coords $ take dimension ([0, 0, 1] ++ repeat 0)

start_axises = Axises $ map (\i -> let r0 = repeat 0 in (Coords $ take i r0 ++ [1] ++ take (dimension - i - 1) r0)) [0..(dimension-1)] :: Axises

zero_coords = Coords $ take dimension $ repeat 0

rotationMatrix :: Int -> Int -> Double -> Data.Matrix.Matrix Double
rotationMatrix i j phi = identity dimension & setElem (cos phi) (i, i) & setElem (-(sin phi)) (i, j) & setElem (cos phi) (j, j) & setElem (sin phi) (j, i)

my_rotate :: Int -> Int -> Double -> Coords -> Coords
my_rotate i j phi (Coords x) = Coords $ Data.Matrix.toList $ rotationMatrix i j phi * (fromList dimension 1 x)

rotateByRotation :: Coords -> Coords -> Coords
rotateByRotation (Coords rotation) pos = foldl (\pos' (r, (i, j)) -> my_rotate i j r pos') pos $ zip rotation $ zip [1..dimension] ([2..dimension] ++ [1])


