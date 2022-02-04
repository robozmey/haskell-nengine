module Trace where

import Coords
import Constants
import Figures
import Lib

screen_distance = 1 :: Double
pixel_x_size = 0.01 :: Double
pixel_y_size = 0.01 :: Double

trace' :: Coords -> Coords -> Coords
trace' cxs dxs = moveCoordsD cxs dxs d
    where (d, c) = min_distance cxs

traceRay :: Coords -> Axises -> Int -> Int -> MyColor4
traceRay my_position my_axises x y = if (md > 0.1) then blackColor else c
    where dxs = normilize $ cxs - my_position
          cxs = my_position + view_direction `scalarMul` screen_distance + side_direction `scalarMul` (fromIntegral  x * pixel_x_size) + up_direction `scalarMul` (fromIntegral y * pixel_y_size)
          fxs = foldl (\cxs' i ->  trace' cxs' dxs) cxs [0..10]
          (md, c) = min_distance fxs
          (view_direction, up_direction, side_direction) = getMainDirections my_axises