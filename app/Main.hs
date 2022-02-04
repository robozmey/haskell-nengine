{-# LANGUAGE FlexibleContexts #-}

module Main where
    
import Control.Concurrent
import Data.Foldable
--import Data.Matrix
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

view_direction = Coords [1, 0, 0]
up_direction = Coords [0, 1, 0]
side_direction = Coords [0, 0, 1]

main = do
    _ <- getArgsAndInitialize
    _ <- createWindow "Haskell NEngine"
    let axises = [view_direction, side_direction, up_direction]
    st <- newIORef (zero_coords, axises)
    keyboardMouseCallback $= Just (keyboardMouse st)
    displayCallback $= display st
    idleCallback $= Just (idle st)
    mainLoop

screenX = 110
screenY = screenX

pixelX = 1 / fromIntegral screenX
pixelY = 1 / fromIntegral screenY

display st = do
    (my_position, axises) <- Graphics.Rendering.OpenGL.get st
    clear [ColorBuffer]
    forM_ (raytrace my_position axises) (\(x, y, z, MyColor4 (r, g, b, a)) -> do
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

raytrace my_position axises = runPar $ do
    let ij = [(i, j) | i <- [(-screenX)..screenX], j <- [(-screenY)..screenY]]
    return $ map (\(i, j) -> ((((fromIntegral (i) / fromIntegral screenX) )), (fromIntegral (j) / fromIntegral screenY), 0, (trace my_position axises (i) (j)))) ij
  --`using` parList rseq
            

idle st = do
  (my_position, tstamp) <- Graphics.Rendering.OpenGL.get st
  postRedisplay Nothing

side_speed = 0.1
view_speed = 0.1

keyboardMouse st key keyState _ {-modifiers-} _ {- pos -} =
  keyboardAct st key keyState

-- step Left
keyboardAct st (Char 'a') Down = do
  (my_position, axises) <- Graphics.Rendering.OpenGL.get st
  let my_position' = my_position - side_direction `scalarMul` side_speed
  st $=! (my_position', axises)

-- step Right
keyboardAct st (Char 'd') Down = do
  (my_position, axises) <- Graphics.Rendering.OpenGL.get st
  let my_position' = my_position + side_direction `scalarMul` side_speed
  st $=! (my_position', axises)

-- step Forward
keyboardAct st (Char 'w') Down = do
  (my_position, axises) <- Graphics.Rendering.OpenGL.get st
  let my_position' = my_position + view_direction `scalarMul` view_speed
  st $=! (my_position', axises)

-- step Back
keyboardAct st (Char 's') Down = do
  (my_position, axises) <- Graphics.Rendering.OpenGL.get st
  let my_position' = my_position - view_direction `scalarMul` view_speed
  st $=! (my_position', axises)

-- rotate Left
keyboardAct st (SpecialKey KeyLeft) Down = do
  (my_position, axises) <- Graphics.Rendering.OpenGL.get st
  let my_position' = my_position + side_direction `scalarMul` side_speed
  st $=! (my_position', axises)

keyboardAct st (SpecialKey keyRight) Down = do
  (my_position, axises) <- Graphics.Rendering.OpenGL.get st
  let my_position' = my_position - side_direction `scalarMul` side_speed
  st $=! (my_position', axises)

keyboardAct _ _ _ =
  return ()

-- rotationMatrix x y phi = matrix dimension dimension $ \(i, j) -> | x == i && y == i = cos phi
--                                                                  | x == i && y == j = (-(sin phi))
--                                                                  | x == j && y == j = cos phi
--                                                                  | x == j && y == i = sin phi
--                                                                  | i == j = 1
--                                                                  | otherwise = 0

                                                                 