{-# LANGUAGE FlexibleContexts #-}

module Main where
    
import Control.Concurrent
import Data.Foldable
import System.Console.ANSI
import Lib

import Control.Lens

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.IORef
--import Data.Time.Clock.POSIX

main = do
    _ <- getArgsAndInitialize
    _ <- createWindow "Points Window"
    let tstamp = 0
    st <- newIORef (zero_coords, tstamp)
    keyboardMouseCallback $= Just (keyboardMouse st)
    displayCallback $= display st
    idleCallback $= Just (idle st)
    mainLoop

    

screenX = 200
screenY = 200

display st = do
    (my_position, _) <- get st
    clear [ColorBuffer]
    currentColor $= Color4 0 0.3 1 1
    renderPrimitive Points(
        do
            forM_ [0..(screenX*2)] (\i -> do
                setCursorPosition i 1
                forM_ [0..(screenY*2)] (\j ->
                    case trace my_position (i - screenX) (j - screenY) of
                        4 -> vertex (Vertex3 (((fromIntegral (i - screenX) / fromIntegral screenX) )::GLfloat) (fromIntegral (j - screenY) / fromIntegral screenY) 0)
                        otherwise -> return ()
                    )
                )
            )
    flush

idle st = do
  (my_position, tstamp) <- get st
  postRedisplay Nothing

side_speed = 0.1
view_speed = 0.1

keyboardMouse st key keyState _ {-modifiers-} _ {- pos -} =
  keyboardAct st key keyState

keyboardAct st (SpecialKey KeyLeft) Down = do
  (my_position, tstamp) <- get st
  let my_position' = my_position - side_direction `scalarMul` side_speed
  st $=! (my_position', tstamp)

keyboardAct st (SpecialKey KeyRight) Down = do
  (my_position, tstamp) <- get st
  let my_position' = my_position + side_direction `scalarMul` side_speed
  st $=! (my_position', tstamp)

keyboardAct st (SpecialKey KeyUp) Down = do
  (my_position, tstamp) <- get st
  let my_position' = my_position + view_direction `scalarMul` view_speed
  st $=! (my_position', tstamp)

keyboardAct st (SpecialKey KeyDown) Down = do
  (my_position, tstamp) <- get st
  let my_position' = my_position - view_direction `scalarMul` view_speed
  st $=! (my_position', tstamp)

keyboardAct _ _ _ =
  return ()