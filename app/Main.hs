{-# LANGUAGE FlexibleContexts #-}

module Main where
    
import Control.Concurrent
import Data.Foldable
import Data.Matrix
import Lib

import Control.Lens
import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad.Par

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.IORef
--import Data.Time.Clock.POSIX

start_view_direction = Coords [1, 0, 0]
start_up_direction = Coords [0, 1, 0]
start_side_direction = Coords [0, 0, 1]

main = do
    _ <- getArgsAndInitialize
    _ <- createWindow "Haskell NEngine"
    let my_axises = (start_view_direction, start_side_direction, start_up_direction)
    let my_rotation = zero_coords
    let my_position = zero_coords
    st <- newIORef (my_position, my_rotation, my_axises)
    keyboardMouseCallback $= Just (keyboardMouse st)
    displayCallback $= display st
    idleCallback $= Just (idle st)
    mainLoop

screenX = 110
screenY = screenX

pixelX = 1 / fromIntegral screenX
pixelY = 1 / fromIntegral screenY

display st = do
    (my_position, my_rotation, my_axises) <- Graphics.Rendering.OpenGL.get st
    clear [ColorBuffer]
    forM_ (raytrace my_position my_axises) (\(x, y, z, MyColor4 (r, g, b, a)) -> do
        currentColor $= Color4 r g b a
        renderPrimitive Polygon ( do
            vertex (Vertex3 (x::GLfloat) y z)
            vertex (Vertex3 (x::GLfloat) (y+pixelY) z)
            vertex (Vertex3 (x+pixelX::GLfloat) (y+pixelY) z)
            vertex (Vertex3 (x+pixelX::GLfloat) (y) z)
            )
        ) 
      `using` rseq
    flush

raytrace my_position my_axises = runPar $ do
    let ij = [(i, j) | i <- [(-screenX)..screenX], j <- [(-screenY)..screenY]]
    return $ map (\(i, j) -> ((((fromIntegral (i) / fromIntegral screenX) )), (fromIntegral (j) / fromIntegral screenY), 0, (traceRay my_position my_axises (i) (j)))) ij
  --`using` parList rseq: (
            

idle st = do
  _ <- Graphics.Rendering.OpenGL.get st
  postRedisplay Nothing

side_speed = 0.1
view_speed = 0.1

keyboardMouse st key keyState _ {-modifiers-} _ {- pos -} =
  keyboardAct st key keyState

movePlayer st speed enum_axis = do
  (my_position, my_rotation, my_axises) <- Graphics.Rendering.OpenGL.get st
  let (view_direction, side_direction, up_direction) = my_axises

  let move_direction = case enum_axis of  ViewDirection -> view_direction
                                          SideDirection -> side_direction
                                          UpDirection   -> up_direction

  let my_position' = my_position + move_direction `scalarMul` speed

  st $=! (my_position', my_rotation, my_axises)


rotatePlayer st axis phi = do
  (my_position, my_rotation, my_axises) <- Graphics.Rendering.OpenGL.get st
  let (view_direction, side_direction, up_direction) = my_axises

  let r = my_rotation `getCoord` axis + phi
  let my_rotation' = setCoord my_rotation axis r

  setNewRotation st my_rotation'


-- step Left
keyboardAct st (Char 'a') Down = do
  movePlayer st (-side_speed) SideDirection

-- step Right
keyboardAct st (Char 'd') Down = do
  movePlayer st side_speed SideDirection

-- step Forward
keyboardAct st (Char 'w') Down = do
  movePlayer st (view_speed) ViewDirection

-- step Back
keyboardAct st (Char 's') Down = do
  movePlayer st (-view_speed) ViewDirection

-- rotate Up
keyboardAct st (SpecialKey KeyUp) Down = do
  rotatePlayer st 0 0.1

-- rotate Down
keyboardAct st (SpecialKey KeyDown) Down = do
  rotatePlayer st 0 (-0.1)

-- rotate Left
keyboardAct st (SpecialKey KeyLeft) Down = do
  rotatePlayer st 2 0.1

-- rotate Right
keyboardAct st (SpecialKey KeyRight) Down = do
  rotatePlayer st 2 (-0.1)

keyboardAct _ _ _ =
  return ()

setNewRotation st my_rotation' = do
  (my_position, my_rotation, my_axises) <- Graphics.Rendering.OpenGL.get st
  let (view_direction, side_direction, up_direction) = my_axises

  let view_direction' = rotateByRotation start_view_direction my_rotation'
  let side_direction' = rotateByRotation start_side_direction my_rotation'
  let up_direction'   = rotateByRotation start_up_direction my_rotation'
  
  let my_axises' = (view_direction', side_direction', up_direction')
  st $=! (my_position, my_rotation', my_axises')

rotationMatrix i j phi = identity dimension & setElem (cos phi) (i, i) & setElem (-(sin phi)) (i, j) & setElem (cos phi) (j, j) & setElem (sin phi) (j, i)

my_rotate :: Int -> Int -> Double -> Coords -> Coords
my_rotate i j phi (Coords x) = Coords $ Data.Matrix.toList $ rotationMatrix i j phi * (fromList 3 1 x)

rotateByRotation pos (Coords rotation) = foldl (\pos' (r, (i, j)) -> my_rotate i j r pos') pos $ zip rotation $ zip [1..dimension] ([2..dimension] ++ [1])

-- $ \(i, j) -> | x == i && y == i = cos phi
--                                                                  | x == i && y == j = (-(sin phi))
--                                                                  | x == j && y == j = cos phi
--                                                                  | x == j && y == i = sin phi
--                                                                  | i == j = 1
--                                                                  | otherwise = 0

                                                                 