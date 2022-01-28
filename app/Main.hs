module Main where
    
import Control.Concurrent
import Data.Foldable
import System.Console.ANSI
import Lib

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

main = do
    getArgsAndInitialize
    createAWindow "Points Window"
    mainLoop

createAWindow windowName = do
    createWindow windowName
    displayCallback $= display

screenX = 100
screenY = 100

display = do
    clear [ColorBuffer]
    currentColor $= Color4 0 0.3 1 1
    renderPrimitive Points(
        do
            forM_ [0..(screenX*2)] (\i -> do
                setCursorPosition i 1
                forM_ [0..(screenY*2)] (\j ->
                    case trace (i - screenX) (j - screenY) of
                        4 -> vertex (Vertex3 (((fromIntegral (i - screenX) / fromIntegral screenX) )::GLfloat) (fromIntegral (j - screenY) / fromIntegral screenY) 0)
                        otherwise -> return ()
                    )
                )
            )
    flush
