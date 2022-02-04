module Coords where

import Constants

newtype Coords = Coords [Double] 
    deriving Show

instance Num Coords where
    (+) (Coords x) (Coords y) = Coords $ zipWith (+) x y
    (-) (Coords x) (Coords y) = Coords $ zipWith (-) x y
    (*) (Coords x) (Coords y) = Coords $ zipWith (*) x y
    abs (Coords x) = Coords $ map abs x
    signum (Coords x) = Coords $ map signum x
    fromInteger c = Coords $ take dimension $ repeat $ fromInteger c

getCoord :: Coords -> Int -> Double
getCoord (Coords xs) index = xs !! index 

setCoord :: Coords -> Int -> Double -> Coords
setCoord (Coords xs) index val = Coords $ take index xs ++ val : drop (index + 1) xs

scalarMul :: Coords -> Double -> Coords
scalarMul (Coords xs) a = Coords $ map (*a) xs

normilize :: Coords -> Coords
normilize (Coords xs) = Coords $ map (\x -> x / d) xs 
    where d = sqrt $ foldr (\x s -> x^2 + s) 0 xs

disctance2 :: Coords -> Coords -> Double
disctance2 (Coords x1s) (Coords x2s)  = foldr (+) 0 (zipWith (\y1 y2 -> (y1 - y2)^2) x1s x2s)

moveCoordsD :: Coords -> Coords -> Double -> Coords
moveCoordsD (Coords cxs) (Coords dxs) d = Coords $ zipWith (\cx dx -> cx + d * dx) cxs dxs

moveCoords :: Coords -> Coords -> Coords
moveCoords (Coords cxs) (Coords dxs) = Coords $ zipWith (\cx dx -> cx + dx) cxs dxs