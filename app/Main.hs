{-# LANGUAGE FlexibleContexts #-}

module Main where
    
import Control.Concurrent
import Data.Foldable

import Lib
import Trace
import Constants
import Coords
import Figures

import Control.Lens
import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies
--import Control.Monad.Par

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.IORef
--import Data.Time.Clock.POSIX

main = do
    _ <- getArgsAndInitialize
    _ <- createWindow "Haskell NEngine"
    let my_axises = start_axises :: Axises
    let my_rotation = zero_coords
    let my_position = zero_coords
    st <- newIORef (my_position, my_rotation, my_axises) :: IO (IORef (Coords, Coords, Axises))
    keyboardMouseCallback $= Just (keyboardMouse st)
    displayCallback $= display st
    idleCallback $= Just (idle st)
    mainLoop

screenX = 110
screenY = screenX

pixelX = 1 / fromIntegral screenX
pixelY = 1 / fromIntegral screenY

display st = do
    (my_position, my_rotation, my_axises) <- get st
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
      --`using` rseq
    flush

--raytrace :: Coords -> Axises -> [[(Float, Float, Float, MyColor4)]]
raytrace my_position my_axises = do
    i <- [(-screenX)..screenX]
    j <- [(-screenY)..screenY]
    return $ (fromIntegral i / fromIntegral screenX, fromIntegral j / fromIntegral screenY, 0, traceRay my_position my_axises i j)
  --`using` parList rseq: (
            

idle st = do
  _ <- Graphics.Rendering.OpenGL.get st
  postRedisplay Nothing

side_speed = 0.1
view_speed = 0.1

keyboardMouse st key keyState _ {-modifiers-} _ {- pos -} =
  keyboardAct st key keyState

movePlayer st speed enum_axis = do
  (my_position, my_rotation, my_axises) <- get st
  let move_direction = case enum_axis of  ViewDirection -> getViewDirection my_axises
                                          SideDirection -> getSideDirection my_axises
                                          UpDirection   -> getUpDirection my_axises

  let my_position' = my_position + move_direction `scalarMul` speed

  st $=! (my_position', my_rotation, my_axises)


rotatePlayer st axis phi = do
  (_, my_rotation, _) <- get st

  let r = my_rotation `getCoord` axis + phi
  let my_rotation' = setCoord my_rotation axis r

  setNewRotation st my_rotation'

setNewRotation st my_rotation' = do
  (my_position, my_rotation, my_axises) <- Graphics.Rendering.OpenGL.get st

  let (Axises ax) = start_axises
  let my_axises' = Axises $ map (rotateByRotation my_rotation') ax

  st $=! (my_position, my_rotation', my_axises')


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
  rotatePlayer st 1 0.1

-- rotate Down
keyboardAct st (SpecialKey KeyDown) Down = do
  rotatePlayer st 1 (-0.1)

-- rotate Left
keyboardAct st (SpecialKey KeyLeft) Down = do
  rotatePlayer st 0 0.1

-- rotate Right
keyboardAct st (SpecialKey KeyRight) Down = do
  rotatePlayer st 0 (-0.1)

keyboardAct _ _ _ =
  return ()

-- $ \(i, j) -> | x == i && y == i = cos phi
--                                                                  | x == i && y == j = (-(sin phi))
--                                                                  | x == j && y == j = cos phi
--                                                                  | x == j && y == i = sin phi
--                                                                  | i == j = 1
--                                                                  | otherwise = 0

                                                                 